import { SqliteClient, Hydration } from './db.js'
import { TokenHydrator } from './hydrator/hydrator.js'
import { ErrorHandler } from './hydrator/errors.js'
import { Logger } from './hydrator/logger.js'

type Deps = {
    db: SqliteClient
}

type Processes = {
    processes: string[],
    hydrations?: {
        [key: string]: Hydration[]
    }
}

// Initialize TokenHydrator once at the top
const logger = new Logger({
    level: 'debug',
    format: 'simple',
    colors: false,
    showTimestamp: true,
    fileOutput: './data/logs/hydrator.log'
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

const startHydration = async ({ id, url, db }: { id: string, url: string, db: SqliteClient }) => {
    await db.saveHydration(id, url, 'REQUESTSENT')
    await hydrator.hydrateTokens([id], url)
        .then(async (res) => {
            if(res[0].success) {
                await db.saveHydration(id, url, 'HYDRATED')
            } else {
                const newStatus = await checkStatus({ id, url })
                await db.saveHydration(id, url, newStatus)
            }
        }) .catch(async (e) => {
                const newStatus = await checkStatus({ id, url })
                await db.saveHydration(id, url, newStatus)
        })
}

export const readProcessesWith = ({ db }: Deps) => {
    return async ({ processes }: { processes?: string[] } = {}): Promise<Processes> => {
        if (processes && processes.length > 0) {
            const hydrations: { [key: string]: Hydration[] } = {}

            for (const processId of processes) {
                const rows = await db.getHydrationsByProcessId(processId)

                if (rows.length > 0) {
                    hydrations[processId] = rows
                }
            }

            return {
                processes,
                hydrations: Object.keys(hydrations).length > 0 ? hydrations : undefined
            }
        } else {
            const allProcessIds = await db.getAllProcessIds()
            const hydrations: { [key: string]: Hydration[] } = {}

            for (const processId of allProcessIds) {
                const rows = await db.getHydrationsByProcessId(processId)

                if (rows.length > 0) {
                    hydrations[processId] = rows
                }
            }

            return {
                processes: allProcessIds,
                hydrations: Object.keys(hydrations).length > 0 ? hydrations : undefined
            }
        }
    }
}

export const hydrateWith = ({ db }: Deps) => {
    return async ({ processes }: Processes) => {
        for (const processId of processes) {
            const hydrations = await db.getHydrationsByProcessId(processId)

            if (hydrations.length > 0) {
                for (const hydration of hydrations) {
                    if (hydration.status === 'REQUESTSENT' || hydration.status === 'HYDRATED') {
                        continue
                    }

                    startHydration({ id: processId, url: hydration.url, db }).then(() => {})
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
                    await db.saveHydration(processId, url, newStatus)
                })
            )
        }
    }
}

export const summaryWith = ({ db }: Deps) => {
    return async () => {
        const totalProcesses = await db.getAllProcessIds()
        const statusCounts = await db.getStatusCounts()

        return {
            totalProcesses: totalProcesses.length,
            statusCounts
        }
    }
}