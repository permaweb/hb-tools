import { DbClient, Hydration, PaginationOptions, ProcessWithTimestamp } from './db.js'
import { TokenHydrator } from './hydrator/hydrator.js'
import { ErrorHandler } from './hydrator/errors.js'
import { Logger } from './hydrator/logger.js'

export type Deps = {
    db: DbClient
}

type Processes = {
    processes: string[],
    hydrations?: {
        [key: string]: Hydration[]
    },
    nextCursor?: number
}

// Initialize TokenHydrator once at the top
const logger = new Logger({
    level: 'debug',
    format: 'simple',
    colors: false,
    showTimestamp: true,
    fileOutput: '~/hydrator.log'
})

const errorHandler = new ErrorHandler(logger)

const hydrator = new TokenHydrator(
    {
        slotThreshold: 200,
        retry: {
            maxAttempts: 3,
            backoff: 1000
        },
        waiting: {
            adaptive: true,
            minInterval: 1000,
            maxInterval: 30000
        },
        output: {
            format: 'simple'
        },
        slotLogging: {
            enabled: false
        }
    },
    logger,
    errorHandler
)

export const readProcessesWith = ({ db }: Deps) => {
    return async ({ processes, query, pagination }: { processes?: string[], query?: string, pagination?: PaginationOptions } = {}): Promise<Processes> => {
        let processesWithTimestamp: ProcessWithTimestamp[]

        if (processes && processes.length > 0) {
            const hydrations: { [key: string]: Hydration[] } = {}
            const filteredProcesses: string[] = []

            for (const processId of processes) {
                const rows = await db.getHydrationsByProcessId(processId, query)

                if (rows.length > 0) {
                    hydrations[processId] = rows
                    filteredProcesses.push(processId)
                }
            }

            return {
                processes: filteredProcesses,
                hydrations: Object.keys(hydrations).length > 0 ? hydrations : undefined
            }
        } else if (query) {
            processesWithTimestamp = await db.getProcessesByQueryWithTimestamp(query, pagination)
        } else {
            processesWithTimestamp = await db.getAllProcessesWithTimestamp(pagination)
        }

        const hydrations: { [key: string]: Hydration[] } = {}
        const filteredProcesses: string[] = []

        for (const process of processesWithTimestamp) {
            const rows = await db.getHydrationsByProcessId(process.processId, query)

            if (rows.length > 0) {
                hydrations[process.processId] = rows
                filteredProcesses.push(process.processId)
            }
        }

        const result: Processes = {
            processes: filteredProcesses,
            hydrations: Object.keys(hydrations).length > 0 ? hydrations : undefined
        }

        if (pagination?.limit !== undefined && processesWithTimestamp.length > 0) {
            if (processesWithTimestamp.length === pagination.limit) {
                const lastProcess = processesWithTimestamp[processesWithTimestamp.length - 1]
                result.nextCursor = lastProcess.timestamp
            }
        }

        return result
    }
}

export const loadProcessesWith = ({ db }: Deps) => {
    return async ({ processes, hydrations }: Processes) => {
        for (const processId of processes) {
            await db.saveProcess(processId)
        }

        if (hydrations) {
            for (const [processId, hydrationList] of Object.entries(hydrations)) {
                for (const hydration of hydrationList) {
                    await db.saveHydration(processId, hydration.url, hydration.status)
                }
            }
        }
    }
}

export const startHydration = async ({ id, url, db }: { id: string, url: string, db: DbClient }) => {
    await db.saveHydration(id, url, 'REQUESTSENT')
    await hydrator.hydrateTokens([id], url)
        .then(async (res) => {
            if(res[0].success) {
                await db.saveHydration(id, url, 'HYDRATED')
            } else {
                const newStatus = await checkStatus({ id, url })
                await db.saveHydration(id, url, newStatus)
            }
            return res
        }).catch(async (e) => {
            const newStatus = await checkStatus({ id, url })
            await db.saveHydration(id, url, newStatus)
            return null
        })
}

const startCron = async ({ id, url, db }: { id: string, url: string, db: DbClient }) => {
    await hydrator.triggerCron(id, 'every', url)
        .then(async (res) => {
            return res
        }).catch(async (e) => {
            return null
        })
}

export const hydrateWith = ({ db }: Deps) => {
    return async ({ processes }: Processes) => {
        for (const processId of processes) {
            const hydrations = await db.getHydrationsByProcessId(processId)

            if (hydrations.length > 0) {
                for (const hydration of hydrations) {
                    if (!['INIT', 'NOPROGRESS', 'PROGRESS'].includes(hydration.status)) {
                        continue
                    }

                    startHydration({ id: processId, url: hydration.url, db }).then(() => {})
                }
            }
        }
    }
}

export const cronWith = ({ db }: Deps) => {
    return async ({ processes }: Processes) => {
        for (const processId of processes) {
            const hydrations = await db.getHydrationsByProcessId(processId)

            if (hydrations.length > 0) {
                for (const hydration of hydrations) {
                    startCron({ id: processId, url: hydration.url, db }).then(() => {})
                }
            }
        }
    }
}

const checkStatus = async ({ id, url }: { id: string, url: string }) => {
    const slot = await hydrator.getCurrentSlot(id, url)
        .then((res) => res)
        .catch((_) => 0)
    const nonce = await hydrator.fetchTokenInfo(id)
        .then((res) => res.nonce)
        .catch((_) => 0)
    if(slot > 0 && nonce > 0) {
        if(slot === nonce) {
            return 'HYDRATED'
        } else if ( nonce - slot < 100 ) {
            return 'HYDRATED'
        } else if (slot > 0) {
            return 'PROGRESS'
        }
    }
    return 'NOPROGRESS'
}

export const refreshStatusWith = ({ db }: Deps) => {
    return async ({ processes }: Processes) => {
        // Build all tasks
        const tasks: Array<{ processId: string, url: string }> = []

        for (const processId of processes) {
            const hydrations = await db.getHydrationsByProcessId(processId)
            for (const hydration of hydrations) {
                tasks.push({ processId, url: hydration.url })
            }
        }

        // Process tasks with concurrency limit of 10
        const concurrency = 10
        for (let i = 0; i < tasks.length; i += concurrency) {
            const batch = tasks.slice(i, i + concurrency)

            await Promise.all(
                batch.map(async ({ processId, url }) => {
                    const newStatus = await checkStatus({ id: processId, url })
                    await new Promise(resolve => setTimeout(resolve, 50))
                    console.log(processId, url, newStatus)
                    await db.saveHydration(processId, url, newStatus)
                })
            )
        }
    }
}

export const summaryWith = ({ db }: Deps) => {
    return async () => {
        const totalProcesses = await db.getAllProcessIds()
        const hydrationStatusCounts = await db.getStatusCounts()
        const repushStatusCounts = await db.getRepushStatusCounts()

        const totalHydrations = Object.values(hydrationStatusCounts).reduce((sum, count) => sum + count, 0)
        const totalRepushes = Object.values(repushStatusCounts).reduce((sum, count) => sum + count, 0)

        return {
            totalProcesses: totalProcesses.length,
            hydrations: {
                total: totalHydrations,
                statusCounts: hydrationStatusCounts
            },
            repushes: {
                total: totalRepushes,
                statusCounts: repushStatusCounts
            }
        }
    }
}