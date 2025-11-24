import 'dotenv/config'
import { readFileSync } from 'fs'
import { createSqliteClient } from './db.js'
import {
  loadProcessesWith,
  hydrateWith,
  cronWith,
  refreshStatusWith,
  readProcessesWith,
  summaryWith,
  cleanBadProcsWith,
  rollingHydrationWith,
  stopOperation,
  getActiveOperations
} from './fn.js'
import { resolveUnpushedWith, readRepushesWith } from './fn-legacy.js'

function parseArgs() {
  const args = process.argv.slice(2)
  const parsed: Record<string, string> = {}

  for (let i = 0; i < args.length; i++) {
    if (args[i].startsWith('--')) {
      const key = args[i].slice(2)
      const value = args[i + 1]
      if (value && !value.startsWith('--')) {
        parsed[key] = value
        i++
      }
    }
  }

  return parsed
}

async function main() {
  if (!process.env.DATABASE_PATH) {
    throw new Error('DATABASE_PATH environment variable is required')
  }

  const args = parseArgs()
  const action = args.action

  const db = await createSqliteClient({
    url: process.env.DATABASE_PATH
  })

  const loadProcesses = loadProcessesWith({ db })
  const hydrate = hydrateWith({ db })
  const cron = cronWith({ db })
  const refreshStatus = refreshStatusWith({ db })
  const readProcesses = readProcessesWith({ db })
  const summary = summaryWith({ db })
  const cleanBadProcs = cleanBadProcsWith({ db })
  const rollingHydration = rollingHydrationWith({ db })
  const resolveUnpushed = resolveUnpushedWith({ db })
  const readRepushes = readRepushesWith({ db })

  if (action === 'load') {
    const filePath = args.file
    if (!filePath) {
      throw new Error('--file argument is required for load-processes action')
    }
    const fileContent = readFileSync(filePath, 'utf-8')
    const loadParams = JSON.parse(fileContent)
    await loadProcesses(loadParams)
  } else if (action === 'refresh-status') {
    const pidsParam = args.pids
    let processIds: string[]

    if (pidsParam) {
      processIds = pidsParam.split(',').map(id => id.trim())
    } else {
      const allProcesses = await readProcesses({})
      processIds = allProcesses.processes
    }

    await refreshStatus({ processes: processIds })
  } else if (action === 'hydrate') {
    const pidsParam = args.pids
    let processIds: string[]

    if (pidsParam) {
      processIds = pidsParam.split(',').map(id => id.trim())
    } else {
      const allProcesses = await readProcesses({})
      processIds = allProcesses.processes
    }

    await hydrate({ processes: processIds })
  } else if (action === 'cron') {
    const pidsParam = args.pids
    let processIds: string[]

    if (pidsParam) {
      processIds = pidsParam.split(',').map(id => id.trim())
    } else {
      const allProcesses = await readProcesses({})
      processIds = allProcesses.processes
    }

    await cron({ processes: processIds })
  } else if (action === 'read') {
    const pidsParam = args.pids
    let result

    if (pidsParam) {
      const processIds = pidsParam.split(',').map(id => id.trim())
      result = await readProcesses({ processes: processIds })
    } else {
      result = await readProcesses({})
    }

    console.log(JSON.stringify(result, null, 2))
  } else if (action === 'summary') {
    const result = await summary()
    console.log(JSON.stringify(result, null, 2))
  } else if (action === 'clean-bad-procs') {
    const result = await cleanBadProcs()
    console.log(JSON.stringify(result, null, 2))
  } else if (action === 'rolling-hydration') {
    const operationId = await rollingHydration()
    console.log(JSON.stringify({ operationId, message: 'Rolling hydration completed' }, null, 2))
  } else if (action === 'stop-rolling-hydration') {
    const operationId = args.id
    if (!operationId) {
      throw new Error('--id argument is required for stop-rolling-hydration action')
    }
    const stopped = stopOperation(operationId)
    console.log(JSON.stringify({ success: stopped, operationId, message: stopped ? 'Operation stopped' : 'Operation not found' }, null, 2))
  } else if (action === 'list-operations') {
    const operations = getActiveOperations()
    console.log(JSON.stringify({ activeOperations: operations }, null, 2))
  } else if (action === 'resolve-unpushed') {
    const txsParam = args.txs
    if (!txsParam) {
      throw new Error('--txs argument is required for resolve-unpushed action')
    }
    const txs = txsParam.split(',').map(id => id.trim())
    await resolveUnpushed(txs)
  } else if (action === 'read-repushes') {
    const result = await readRepushes()
    console.log(JSON.stringify(result, null, 2))
  } else if (action) {
    throw new Error(`Unknown action: ${action}`)
  }
}

main()
  .then(() => {
    
  })
  .catch((error) => {
    console.error('Failed', error)
    process.exit(1)
  })
