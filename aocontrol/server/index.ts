import 'dotenv/config'
import express from 'express';
import cors from 'cors';
import path from 'path';
import { fileURLToPath } from 'url';
import { createSqliteClient } from './db.js'
import { loadProcessesWith, hydrateWith, refreshStatusWith, readProcessesWith, summaryWith, cleanBadProcsWith, rollingHydrationWith, stopOperation, getActiveOperations } from './fn.js'

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
const PORT = process.env.PORT || 3001;

app.use(cors());
app.use(express.json());

async function startServer() {
  if (!process.env.DATABASE_PATH) {
    throw new Error('DATABASE_PATH environment variable is required')
  }

  const db = await createSqliteClient({
    url: process.env.DATABASE_PATH
  })

  const loadProcesses = loadProcessesWith({ db })
  const hydrate = hydrateWith({ db })
  const refreshStatus = refreshStatusWith({ db })
  const readProcesses = readProcessesWith({ db })
  const summary = summaryWith({ db })
  const cleanBadProcs = cleanBadProcsWith({ db })
  const rollingHydration = rollingHydrationWith({ db })

  app.post('/api/load', async (req, res) => {
  try {
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

app.post('/api/hydrate', async (req, res) => {
  try {
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

app.post('/api/refresh-status', async (req, res) => {
  try {
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
    let result

    if (pidsParam) {
      const processIds = pidsParam.split(',').map(id => id.trim())
      result = await readProcesses({ processes: processIds })
    } else {
      result = await readProcesses({})
    }

    res.json(result)
  } catch (error) {
    console.error('Error reading processes:', error)
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

  app.post('/api/clean-bad-procs', async (_req, res) => {
    try {
      const result = await cleanBadProcs()
      res.json(result)
    } catch (error) {
      console.error('Error cleaning bad processes:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  app.post('/api/rolling-hydration', async (_req, res) => {
    try {
      const operationId = await rollingHydration()
      res.json({ success: true, operationId, message: 'Started rolling hydration for NOPROGRESS processes' })
    } catch (error) {
      console.error('Error starting rolling hydration:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  app.post('/api/stop-rolling-hydration', async (req, res) => {
    try {
      const { operationId } = req.body
      if (!operationId) {
        return res.status(400).json({ error: 'operationId is required' })
      }
      const stopped = stopOperation(operationId)
      res.json({ success: stopped, operationId, message: stopped ? 'Operation stopped' : 'Operation not found' })
    } catch (error) {
      console.error('Error stopping rolling hydration:', error)
      res.status(500).json({ error: (error as Error).message })
    }
  })

  app.get('/api/operations', async (_req, res) => {
    try {
      const operations = getActiveOperations()
      res.json({ activeOperations: operations })
    } catch (error) {
      console.error('Error getting operations:', error)
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
