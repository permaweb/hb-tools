import { SqliteClient, Hydration } from './db.js'
import { TokenHydrator } from './hydrator/hydrator.js'
import { ErrorHandler } from './hydrator/errors.js'
import { Logger } from './hydrator/logger.js'
import { randomBytes } from 'crypto'

export type Deps = {
    db: SqliteClient
}

// Track active rolling hydration operations
const activeOperations = new Map<string, { aborted: boolean }>()

export function generateOperationId(): string {
    return randomBytes(16).toString('hex')
}

export function stopOperation(operationId: string): boolean {
    const operation = activeOperations.get(operationId)
    if (operation) {
        operation.aborted = true
        return true
    }
    return false
}

export function getActiveOperations(): string[] {
    return Array.from(activeOperations.keys())
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
            return res
        }).catch(async (e) => {
            const newStatus = await checkStatus({ id, url })
            await db.saveHydration(id, url, newStatus)
            return null
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

export const cleanBadProcsWith = ({ db }: Deps) => {
    return async () => {
        const allProcessIds = await db.getAllProcessIds()
        const processesToDelete: string[] = []

        for (const processId of allProcessIds) {
            const hydrations = await db.getHydrationsByProcessId(processId)

            const hasNonHydratedStatus = hydrations.some(h => h.status !== 'HYDRATED')

            if (hasNonHydratedStatus) {
                try {
                    const nonce = await hydrator.fetchTokenInfo(processId)
                        .then((res) => res.nonce)

                    if (nonce <= 1) {
                        console.log(`marking for deletion: ${processId}`)
                        processesToDelete.push(processId)
                    }
                } catch (e: any) {
                    continue
                }
            }
        }

        for (const processId of processesToDelete) {
            await db.deleteProcess(processId)
        }

        return {
            totalProcesses: allProcessIds.length,
            deletedProcesses: processesToDelete.length,
            deletedIds: processesToDelete
        }
    }
}

// this will run MAX_CONCURRENT hydrations at once, it will
// start them if they have a NOPROGRESS status
export const rollingHydrationWith = ({ db }: Deps) => {
    return async (operationId?: string) => {
        const opId = operationId || generateOperationId()

        activeOperations.set(opId, { aborted: false })

        try {
            const allProcessIds = await db.getAllProcessIds()
            const runningPromises = new Set<Promise<void>>()
            const MAX_CONCURRENT = 20

            const queueHydration = async (processId: string, url: string) => {
                while (runningPromises.size >= MAX_CONCURRENT) {
                    const op = activeOperations.get(opId)
                    if (op?.aborted) {
                        return
                    }

                    if (runningPromises.size > 0) {
                        await Promise.race(runningPromises)
                    }
                }

                const op = activeOperations.get(opId)
                if (op?.aborted) {
                    return
                }

                const promise = startHydration({ id: processId, url, db })
                    .then(() => {
                        runningPromises.delete(promise)
                        console.log(`Hydration completed for ${processId}. Queue size: ${runningPromises.size}/${MAX_CONCURRENT}`)
                    })
                    .catch((err) => {
                        console.error(`Hydration error for ${processId}:`, err)
                        runningPromises.delete(promise)
                        console.log(`Queue size after error: ${runningPromises.size}/${MAX_CONCURRENT}`)
                    })

                runningPromises.add(promise)
                console.log(`Started hydration for ${processId}. Queue size: ${runningPromises.size}/${MAX_CONCURRENT}`)
            }

            for (const processId of allProcessIds) {
                const operation = activeOperations.get(opId)
                if (operation?.aborted) {
                    console.log(`Rolling hydration ${opId} was stopped`)
                    break
                }

                const hydrations = await db.getHydrationsByProcessId(processId)

                for (const hydration of hydrations) {
                    const op = activeOperations.get(opId)
                    if (op?.aborted) {
                        console.log(`Rolling hydration ${opId} was stopped`)
                        break
                    }

                    if (hydration.status === 'NOPROGRESS') {
                        await queueHydration(processId, hydration.url)
                    }
                }

                const op = activeOperations.get(opId)
                if (op?.aborted) {
                    break
                }
            }

            if (runningPromises.size > 0) {
                console.log(`Waiting for ${runningPromises.size} remaining hydrations to complete...`)
                await Promise.all(runningPromises)
            }

        } finally {
            activeOperations.delete(opId)
        }

        return opId
    }
}