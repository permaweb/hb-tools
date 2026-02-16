import 'dotenv/config'
import express from 'express';
import cors from 'cors';
import path from 'path';
import { fileURLToPath } from 'url';
import { Worker } from 'worker_threads';
import { createPostgresClient } from './db.js'
import {
  loadProcessesWith,
  hydrateWith,
  cronWith,
  refreshStatusWith,
  readProcessesWith,
  summaryWith
} from './fn.js'
import { resolveUnpushedWith, readRepushesWith } from './fn-legacy.js'
import { authRequestWith } from './auth.ts'

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
const PORT = process.env.PORT || 3001;

let worker: Worker;

app.use(cors());
app.use(express.json({ limit: '10mb' }));

async function startServer() {
  if (!process.env.DATABASE_PATH) {
    throw new Error('DATABASE_PATH environment variable is required')
  }

  const db = await createPostgresClient({
    url: process.env.DATABASE_PATH
  })

  // Initialize worker thread (worker.js is built from worker.ts via esbuild)
  const workerPath = path.join(__dirname, 'worker.js');
  worker = new Worker(workerPath);

  worker.on('message', (message) => {
    console.log('[Server] Received from worker:', message);
  });

  worker.on('error', (error) => {
    console.error('[Server] Worker error:', error);
  });

  worker.on('exit', (code) => {
    console.log('[Server] Worker exited with code:', code);
  });

  const authRequest = authRequestWith({ db })
  const loadProcesses = loadProcessesWith({ db })
  const hydrate = hydrateWith({ db })
  const cron = cronWith({ db })
  const refreshStatus = refreshStatusWith({ db })
  const readProcesses = readProcessesWith({ db })
  const summary = summaryWith({ db })
  const resolveUnpushed = resolveUnpushedWith({ db })
  const readRepushes = readRepushesWith({ db })

app.post('/api/load', async (req, res) => {
  try {
    // Auth check
    const auth = await authRequest(req)
    if (!auth.authorized) {
      return res.status(401).json({ error: auth.error })
    }

    const { processes, hydrations } = req.body
    if (!processes || !Array.isArray(processes)) {
      return res.status(400).json({ error: 'processes array is required' })
    }
    await loadProcesses({ processes, hydrations })
    res.json({ success: true, message: `Loaded ${processes.length} processes` })
  } catch (error) {
    console.error('Error loading processes:', error)
    res.status(500).json({ error: (error as Error).message })
  }
})

app.post('/api/queue-hydrations', async (req, res) => {
  try {
    // Auth check
    const auth = await authRequest(req)
    if (!auth.authorized) {
      return res.status(401).json({ error: auth.error })
    }

    const { processes, hydrations } = req.body
    if (!processes || !Array.isArray(processes)) {
      return res.status(400).json({ error: 'processes array is required' })
    }

    worker.postMessage({
      type: 'queue-hydrations',
      payload: { processes, hydrations }
    });

    res.json({ success: true, message: `Queued hydration for ${processes.length} processes` })
  } catch (error) {
    console.error('Error queueing hydration:', error)
    res.status(500).json({ error: (error as Error).message })
  }
})

app.post('/api/hydrate', async (req, res) => {
  try {
    // Auth check
    const auth = await authRequest(req)
    if (!auth.authorized) {
      return res.status(401).json({ error: auth.error })
    }

    const { processes } = req.body
    let processIds: string[]

    if (processes && Array.isArray(processes)) {
      processIds = processes
    } else {
      const allProcesses = await readProcesses({})
      processIds = allProcesses.processes
    }

    hydrate({ processes: processIds }).catch(err => {
      console.error('Hydration error:', err)
    })

    res.json({ success: true, message: `Started hydration for ${processIds.length} processes` })
  } catch (error) {
    console.error('Error starting hydration:', error)
    res.status(500).json({ error: (error as Error).message })
  }
})

app.post('/api/cron', async (req, res) => {
  try {
    // Auth check
    const auth = await authRequest(req)
    if (!auth.authorized) {
      return res.status(401).json({ error: auth.error })
    }

    const { processes } = req.body
    let processIds: string[]

    if (processes && Array.isArray(processes)) {
      processIds = processes
    } else {
      const allProcesses = await readProcesses({})
      processIds = allProcesses.processes
    }

    cron({ processes: processIds }).catch(err => {
      console.error('Cron error:', err)
    })

    res.json({ success: true, message: `Started cron for ${processIds.length} processes` })
  } catch (error) {
    console.error('Error starting cron:', error)
    res.status(500).json({ error: (error as Error).message })
  }
})

app.post('/api/refresh-status', async (req, res) => {
  try {
    // Auth check
    const auth = await authRequest(req)
    if (!auth.authorized) {
      return res.status(401).json({ error: auth.error })
    }

    const { processes } = req.body
    let processIds: string[]

    if (processes && Array.isArray(processes)) {
      processIds = processes
    } else {
      const allProcesses = await readProcesses({})
      processIds = allProcesses.processes
    }

    refreshStatus({ processes: processIds }).catch(err => {
      console.error('Refresh status error:', err)
    })

    res.json({ success: true, message: `Started status refresh for ${processIds.length} processes` })
  } catch (error) {
    console.error('Error refreshing status:', error)
    res.status(500).json({ error: (error as Error).message })
  }
})

app.get('/api/processes', async (req, res) => {
  try {
    const pidsParam = req.query.pids as string | undefined
    const queryParam = req.query.query as string | undefined
    const limitParam = req.query.limit as string | undefined
    const cursorParam = req.query.cursor as string | undefined

    // Default limit is 100
    const limit = limitParam ? parseInt(limitParam, 10) : 100
    const pagination = {
      limit: limit,
      cursor: cursorParam ? parseInt(cursorParam, 10) : undefined
    }

    let result

    if (pidsParam) {
      const processIds = pidsParam.split(',').map(id => id.trim())
      result = await readProcesses({ processes: processIds, query: queryParam, pagination })
    } else {
      result = await readProcesses({ query: queryParam, pagination })
    }

    res.json(result)
  } catch (error) {
    console.error('Error reading processes:', error)
    res.status(500).json({ error: (error as Error).message })
  }
})

  app.get('/api/repushes', async (req, res) => {
    try {
      const queryParam = req.query.query as string | undefined
      const limitParam = req.query.limit as string | undefined
      const cursorParam = req.query.cursor as string | undefined

      // Default limit is 100
      const limit = limitParam ? parseInt(limitParam, 10) : 100
      const pagination = {
        limit: limit,
        cursor: cursorParam ? parseInt(cursorParam, 10) : undefined
      }

      const result = await readRepushes({ query: queryParam, pagination })
      res.json(result)
    } catch (error) {
      console.error('Error reading repushes:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  app.get('/api/summary', async (_req, res) => {
    try {
      const result = await summary()
      res.json(result)
    } catch (error) {
      console.error('Error getting summary:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  app.post('/api/resolve-unpushed', async (req, res) => {
    try {
      // Auth check
      const auth = await authRequest(req)
      if (!auth.authorized) {
        return res.status(401).json({ error: auth.error })
      }

      const { txs, custom, 'skip-repush-checks-token': skipRepushChecksToken } = req.body
      if (!txs || !Array.isArray(txs)) {
        return res.status(400).json({ error: 'txs array is required' })
      }

      // Check if skip-repush-checks-token is provided and validate
      let skipChecksToken: string | undefined = undefined
      if (skipRepushChecksToken) {
        if (!auth.isAdmin) {
          return res.status(403).json({ error: 'Only admin users can use skip-repush-checks-token' })
        }
        if (typeof skipRepushChecksToken !== 'string') {
          return res.status(400).json({ error: 'skip-repush-checks-token must be a string' })
        }
        skipChecksToken = skipRepushChecksToken
        console.log('Skip repush checks enabled for this request')
      }

      await resolveUnpushed(txs, custom, skipChecksToken)
      res.json({ success: true, message: `Resolved unpushed for ${txs.length} transactions` })
    } catch (error) {
      console.error('Error resolving unpushed:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  app.post('/api/start-hydrations', async (req, res) => {
    try {
      // Auth check
      const auth = await authRequest(req)
      if (!auth.authorized) {
        return res.status(401).json({ error: auth.error })
      }

      worker.postMessage('start-hydrations');
      res.json({ success: true, message: 'Hydrations started' })
    } catch (error) {
      console.error('Error starting hydrations:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  app.post('/api/stop-hydrations', async (req, res) => {
    try {
      // Auth check
      const auth = await authRequest(req)
      if (!auth.authorized) {
        return res.status(401).json({ error: auth.error })
      }

      worker.postMessage('stop-hydrations');
      res.json({ success: true, message: 'Hydrations stopped' })
    } catch (error) {
      console.error('Error stopping hydrations:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  if (process.env.NODE_ENV === 'production') {
    app.use(express.static(path.join(__dirname, '../dist')));
    app.get(/^\/(?!api).*/, (_req, res) => {
      res.sendFile(path.join(__dirname, '../dist/index.html'));
    });
  }

  app.listen(PORT, () => {
    console.log(`Server running on http://localhost:${PORT}`);
    console.log(`API available at http://localhost:${PORT}/api`);
  });
}

startServer().catch((error) => {
  console.error('Failed to start server:', error);
  process.exit(1);
});
