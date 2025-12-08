
import { Deps } from './fn'
import { PaginationOptions } from './db.js'
import { missingNonceReport } from './repusher/pushIssues.js'
import { pushMu } from './repusher/pushMu.js'

export const resolveUnpushedWith = ({ db }: Deps) => {
    return async (txs: string[], custom: boolean) => {
        const report = await missingNonceReport(txs, custom)
        for(let i=0; i< report.unpushedMessages.length; i ++) {
            const { 
                originalMessageId: messageId,
                slot: outboxSlot,
                processId: pid
            } = report.unpushedMessages[i]

            const muResult = await pushMu({
                messageId,
                outboxSlot,
                pid,
                fileResult: false,
                customCu: custom
            }) 
            .then((res) => res.json())
            .catch((e) => e)

            console.log(muResult)
        }

        console.log(`Waiting for gateway indexing...`)
        await new Promise(resolve => setTimeout(resolve, 15000))

        const updatedReport = await missingNonceReport(txs, custom)

        for(let i=0; i < txs.length; i++) {
            let txId = txs[i]
            let errors = updatedReport.errors.filter((e) => e.messageId === txId)
            let stillUnpushed = updatedReport.unpushedMessages.filter((u) => u.originalMessageId === txId)
            let alreadyPushed = updatedReport.alreadyPushedMessages.filter((a) => a.originalMessageId === txId)

            // extract the process id from one of the above lists and save it to the processes table
            const processId = (
                stillUnpushed[0]?.processId ||
                errors[0]?.processId ||
                alreadyPushed[0]?.processId
            )

            if (processId) {
                await db.saveProcess(processId)

                if(errors.length > 0 || stillUnpushed.length > 0) {
                    let reasons: any = {}
                    reasons["errors"] = errors
                    reasons["unpushedMessages"] = stillUnpushed
                    // save the repush in status CANNOTPUSHALL with reason JSON.stringify(reasons)
                    await db.saveRepush(processId, txId, 'CANNOTPUSHALL', JSON.stringify(reasons))
                } else {
                    // save the repush in status REPUSHED
                    await db.saveRepush(processId, txId, 'REPUSHED')
                }
            }
        }
    }
}

export const readRepushesWith = ({ db }: Deps) => {
    return async ({ query, pagination }: { query?: string, pagination?: PaginationOptions } = {}) => {
        const repushes = await db.getRepushes(query, pagination)

        const result: { repushes: typeof repushes, nextCursor?: number } = { repushes }

        // Add nextCursor only if we got a full page (indicating more results may exist)
        if (pagination?.limit !== undefined && repushes.length > 0) {
            if (repushes.length === pagination.limit) {
                const lastRepush = repushes[repushes.length - 1]
                result.nextCursor = lastRepush.timestamp
            }
        }

        return result
    }
}