import 'dotenv/config'
import { readFileSync } from 'fs'
import { createSqliteClient } from './db.js'
import { loadProcessesWith, hydrateWith, refreshStatusWith, readProcessesWith, summaryWith, cleanBadProcsWith } from './fn.js'

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
  const refreshStatus = refreshStatusWith({ db })
  const readProcesses = readProcessesWith({ db })
  const summary = summaryWith({ db })
  const cleanBadProcs = cleanBadProcsWith({ db })

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
