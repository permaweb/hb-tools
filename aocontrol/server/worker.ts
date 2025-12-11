import 'dotenv/config'
import { parentPort, MessagePort } from 'worker_threads';
import { createPostgresClient } from './db.js';
import {
  startHydration
} from './fn.js'

// Type definitions for worker messages
interface QueueHydrationsMessage {
  type: 'queue-hydrations';
  payload: {
    processes: string[];
    hydrations?: any;
  };
}

interface StatusMessage {
  type: 'status';
  running: boolean;
}

type WorkerMessage = string | QueueHydrationsMessage;

if (!parentPort) {
  throw new Error('This file must be run as a Worker thread');
}

if (!process.env.DATABASE_PATH) {
    throw new Error('DATABASE_PATH environment variable is required')
}

const db = await createPostgresClient({
    url: process.env.DATABASE_PATH
})


console.log('[Worker] Worker thread initialized');

let isRunning: boolean = false;

const port: MessagePort = parentPort;

port.on('message', async (message: WorkerMessage) => {
  try {
    console.log('[Worker] Received message:', message);

    // Handle string messages for backwards compatibility
    if (typeof message === 'string') {
      if (message === 'start-hydrations') {
        isRunning = true;
        console.log('[Worker] Hydrations started');
        const response: StatusMessage = { type: 'status', running: true };
        port.postMessage(response);
      } else if (message === 'stop-hydrations') {
        isRunning = false;
        console.log('[Worker] Hydrations stopped');
        const response: StatusMessage = { type: 'status', running: false };
        port.postMessage(response);
      }
      return;
    }

    if (message.type === 'queue-hydrations') {
      saveHydrations(message)
    }
  } catch (error) {
    console.log('[Worker] error:', error);
  }
});

async function saveHydrations(message: QueueHydrationsMessage) {
    const { processes, hydrations } = message.payload;

    for (const processId of processes) {
        await db.saveProcess(processId);
    }

    if (hydrations && Array.isArray(hydrations)) {
        for (const hydration of hydrations) {
            const { processId, url, status } = hydration;
            const existingHydrations = await db.getHydrationsByProcessId(
              processId, 
              url
            );

            if (existingHydrations.length === 0) {
                await db.saveHydration(processId, url, status);
            }
        }
    }

    console.log(`[Worker] Queued hydration job with ${processes.length} processes.`);
}


// Continuously process INIT hydrations with MAX_CONCURRENT limit
const runningPromises = new Set<Promise<void>>()
const MAX = process.env.MAX_CONCURRENT_HYDRATIONS || '100'
const MAX_CONCURRENT = parseInt(MAX)

const queueHydration = async (processId: string, url: string) => {
    while (runningPromises.size >= MAX_CONCURRENT) {
        if (runningPromises.size > 0) {
            await Promise.race(runningPromises)
        }
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

while (true) {
    if (!isRunning) {
        await new Promise<void>(resolve => setTimeout(resolve, 1000))
        continue
    }

    const initHydrations = await db.getHydrationsByStatus('INIT', 200)

    if (initHydrations.length === 0) {
        console.log('[Worker] No INIT hydrations found, waiting 5 seconds before next check...')
        await new Promise<void>(resolve => setTimeout(resolve, 5000))
        continue
    }

    console.log(`[Worker] Found ${initHydrations.length} INIT hydrations to process`)

    for (const hydration of initHydrations) {
        await queueHydration(hydration.processId, hydration.url)
    }

    await new Promise<void>(resolve => setTimeout(resolve, 100))
}



