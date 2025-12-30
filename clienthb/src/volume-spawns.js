import fs from 'fs';
import { connect, createSigner } from '@permaweb/aoconnect';
import { Worker, isMainThread, parentPort, workerData } from 'worker_threads';
import { performance } from 'perf_hooks';

import { parseArgs, expect, createTestRunner } from './utils.js';

let TOTAL_SPAWNS = 1;                   // Total processes to spawn across all workers
let TOTAL_MESSAGES = 1;               // Total Info messages to send across all workers
let WORKERS = 1;                        // Number of worker threads

// Rate limits (global totals; each worker gets an even share)
const RATE_SPAWNS_PER_SEC = 1;         // Max spawn ops per second across all workers
const RATE_MSGS_PER_SEC = 1;           // Max message ops per second across all workers

// Jitter (adds +/- this many ms to each op's pacing sleep; 0 disables)
const JITTER_MS = 100;

// Per-worker concurrency caps
const CONCURRENCY_SPAWN_PER_WORKER = 1;
const CONCURRENCY_MSG_PER_WORKER = 1;

function log(...args) {
    console.log(`\x1b[36m[HB Client Volume]\x1b[0m`, ...args);
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const jitter = (ms) => (JITTER_MS > 0 ? Math.max(0, ms + (Math.random() * 2 - 1) * JITTER_MS) : ms);

class RateLimiter {
    constructor(opsPerSecPerWorker) {
        this.interval = opsPerSecPerWorker > 0 ? 1000 / opsPerSecPerWorker : 0;
        this._next = 0;
    }
    async take() {
        if (this.interval <= 0) return; // Unlimited
        const now = performance.now();
        this._next = Math.max(this._next, now) + this.interval;
        const wait = this._next - now;
        if (wait > 0) await sleep(jitter(wait));
    }
}

async function mapWithConcurrency(items, limit, fn) {
    const results = new Array(items.length);
    let i = 0;
    const workers = new Array(Math.min(limit || 1, items.length)).fill(0).map(async () => {
        while (true) {
            const idx = i++;
            if (idx >= items.length) break;
            results[idx] = await fn(items[idx], idx);
        }
    });
    await Promise.all(workers);
    return results;
}

async function addHandlers(ao, processId, signer) {
    const code = `
Handlers.add('Info', 'Info', function(msg)
  ao.send({ Target = msg.From, Data = require('json').encode({ Hello = 'World' }) })
  ao.send({ Target = msg.From, Data = require('json').encode({ Hello = 'World 2' }) })
end)
  `.trim();

    return ao.message({
        process: processId,
        tags: [{ name: 'Action', value: 'Eval' }, { name: 'Name', value: Date.now().toString() }],
        data: code,
        signer,
    });
}

async function triggerInfo(ao, processId, signer) {
    return ao.message({
        process: processId,
        tags: [{ name: 'Action', value: 'Info' }, { name: 'Time', value: Date.now().toString() }],
        data: '',
        signer,
    });
}

async function workerRun({
    configPath,
    group,
    totals,
    pacing,
    caps,
    workerIndex,
    workerCount,
    overrides = {},
}) {
    const runner = createTestRunner();
    runner.start();
    log(`Worker ${workerIndex}: Starting initialization`);
    const config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
    if (!config[group]) throw new Error(`Group '${group}' not found in ${configPath}`);

    const groupConfig = { ...config.defaults, ...config[group] };

    const MAINNET_URL = overrides.url || groupConfig.url;
    const MAINNET_SCHEDULER = overrides.scheduler || groupConfig.schedulerAddress;
    const WALLET = JSON.parse(fs.readFileSync(process.env.PATH_TO_WALLET));

    const SIGNER = createSigner(WALLET);
    const ao = connect({
        MODE: 'mainnet',
        URL: MAINNET_URL,
        SCHEDULER: MAINNET_SCHEDULER,
        signer: SIGNER,
    });
    log(`Worker ${workerIndex}: Connected to AO network`);

    // Even share with remainder distributed to lowest indices
    const share = (total, idx, n) => Math.floor(total / n) + (idx < total % n ? 1 : 0);
    const mySpawns = share(totals.spawns, workerIndex, workerCount);
    const myMsgs = share(totals.messages, workerIndex, workerCount);
    log(`Worker ${workerIndex}: Assigned ${mySpawns} spawns and ${myMsgs} messages`);

    // Per-worker rate limiters
    const spawnsRate = pacing.spawnsPerSec > 0 ? pacing.spawnsPerSec / workerCount : 0;
    const msgsRate = pacing.msgsPerSec > 0 ? pacing.msgsPerSec / workerCount : 0;
    const spawnLimiter = new RateLimiter(spawnsRate);
    const msgLimiter = new RateLimiter(msgsRate);

    const t0 = performance.now();

    let spawnCount = 0;
    let handlerMsgCount = 0;
    let infoMsgCount = 0;

    const spawnJobs = Array.from({ length: mySpawns }, (_, i) => i);
    const processIds = [];
    log(`Worker ${workerIndex}: Starting ${mySpawns} process spawns with concurrency ${caps.spawnConcurrency}`);
    await mapWithConcurrency(spawnJobs, caps.spawnConcurrency, async (i) => {
        await runner.test(async () => {
            await spawnLimiter.take();
            const pid = await ao.spawn({
                module: groupConfig.aosModule,
                tags: [{ name: 'Name', value: `${Date.now()}-${workerIndex}-${i}` }],
            });
            expect(pid).toEqualType('string');
            log(`Process ID: ${pid}`);
            spawnCount++;
            processIds.push(pid);
            if (spawnCount % 10 === 0 || spawnCount === mySpawns) {
                log(`Worker ${workerIndex}: Spawned ${spawnCount}/${mySpawns} processes`);
            }

            await msgLimiter.take(); // Treat handler install as a message op
            const message = await addHandlers(ao, pid, SIGNER);
            expect(message).toEqualType('number');
            log(`Handlers Add: ${message}`);
            handlerMsgCount++;
            if (handlerMsgCount % 10 === 0 || handlerMsgCount === mySpawns) {
                log(`Worker ${workerIndex}: Installed handlers on ${handlerMsgCount}/${mySpawns} processes`);
            }
        });
    });

    // If messages requested but no pids, make one fallback pid
    if (myMsgs > 0 && processIds.length === 0) {
        log(`Worker ${workerIndex}: No processes available, creating fallback process for messages`);
        await runner.test(async () => {
            await spawnLimiter.take();
            const pid = await ao.spawn({
                module: groupConfig.aosModule,
                tags: [{ name: 'Name', value: `fallback-${Date.now()}-${workerIndex}` }],
            });
            expect(pid).toEqualType('string');
            spawnCount++;
            processIds.push(pid);
            log(`Worker ${workerIndex}: Created fallback process ${pid}`);

            await msgLimiter.take();
            const message = await addHandlers(ao, pid, SIGNER);
            expect(message).toEqualType('number');
            log(`Handlers Add: ${message}`);
            handlerMsgCount++;
        });
    }

    const msgJobs = Array.from({ length: myMsgs }, (_, i) => i);
    if (myMsgs > 0) {
        log(`Worker ${workerIndex}: Starting ${myMsgs} Info messages with concurrency ${caps.msgConcurrency}`);
    }
    await mapWithConcurrency(msgJobs, caps.msgConcurrency, async (m) => {
        const pid = processIds.length ? processIds[m % processIds.length] : null;
        if (!pid) return;
        await runner.test(async () => {
            await msgLimiter.take();
            const message = await triggerInfo(ao, pid, SIGNER);
            expect(message).toEqualType('number');
            log(`Info Trigger: ${message}`);
            infoMsgCount++;
            if (infoMsgCount % 25 === 0 || infoMsgCount === myMsgs) {
                log(`Worker ${workerIndex}: Sent ${infoMsgCount}/${myMsgs} Info messages`);
            }
        });
    });

    const t1 = performance.now();
    const durationMs = Math.round(t1 - t0);

    log(`Worker ${workerIndex}: Completed in ${durationMs}ms - spawns: ${spawnCount}/${mySpawns}, handlers: ${handlerMsgCount}, messages: ${infoMsgCount}/${myMsgs}`);

    const exitCode = runner.getSummary(`Worker ${workerIndex} Volume Tests`);

    return {
        workerIndex,
        mySpawns,
        myMsgs,
        spawnCount,
        handlerMsgCount,
        infoMsgCount,
        durationMs,
        perWorkerRates: { spawnsRate, msgsRate },
        exitCode,
    };
}



if (isMainThread) {
    (async () => {
        const { group, flags } = parseArgs('volume.js');

        // Apply CLI overrides
        if (flags.spawns) TOTAL_SPAWNS = parseInt(flags.spawns);
        if (flags.messages) TOTAL_MESSAGES = parseInt(flags.messages);
        if (flags.workers) WORKERS = parseInt(flags.workers);

        log(
            `Start load test | group=${group} spawns=${TOTAL_SPAWNS} messages=${TOTAL_MESSAGES} workers=${WORKERS}`
        );
        log(
            `Rates: spawns=${RATE_SPAWNS_PER_SEC}/s, msgs=${RATE_MSGS_PER_SEC}/s | Jitter=±${JITTER_MS}ms | Concurrency: spawn=${CONCURRENCY_SPAWN_PER_WORKER}, msg=${CONCURRENCY_MSG_PER_WORKER}`
        );

        const start = performance.now();
        const configPath = flags.config || 'config.json';

        const mkWorker = (i) =>
            new Promise((resolve, reject) => {
                const w = new Worker(new URL(import.meta.url), {
                    workerData: {
                        configPath,
                        group,
                        totals: { spawns: TOTAL_SPAWNS, messages: TOTAL_MESSAGES },
                        pacing: { spawnsPerSec: RATE_SPAWNS_PER_SEC, msgsPerSec: RATE_MSGS_PER_SEC },
                        caps: {
                            spawnConcurrency: CONCURRENCY_SPAWN_PER_WORKER,
                            msgConcurrency: CONCURRENCY_MSG_PER_WORKER,
                        },
                        workerIndex: i,
                        workerCount: WORKERS,
                        overrides: {
                            url: flags.url,
                            scheduler: flags.scheduler,
                        },
                    },
                });
                w.on('message', resolve);
                w.on('error', reject);
                w.on('exit', (code) => {
                    if (code !== 0) reject(new Error(`Worker ${i} exited with code ${code}`));
                });
            });

        try {
            const results = await Promise.all(Array.from({ length: WORKERS }, (_, i) => mkWorker(i)));

            const agg = results.reduce(
                (a, r) => {
                    a.spawnCount += r.spawnCount;
                    a.handlerMsgCount += r.handlerMsgCount;
                    a.infoMsgCount += r.infoMsgCount;
                    a.workerDurations.push({ worker: r.workerIndex, ms: r.durationMs });
                    a.workerRates.push({ worker: r.workerIndex, spawnsRate: r.perWorkerRates.spawnsRate, msgsRate: r.perWorkerRates.msgsRate });
                    a.totalExitCode = Math.max(a.totalExitCode, r.exitCode || 0);
                    return a;
                },
                { spawnCount: 0, handlerMsgCount: 0, infoMsgCount: 0, workerDurations: [], workerRates: [], totalExitCode: 0 }
            );

            const elapsed = Math.round(performance.now() - start);

            log('--- Results ---');
            log(`Requested totals: spawns=${TOTAL_SPAWNS}, messages=${TOTAL_MESSAGES}, workers=${WORKERS}`);
            log(`Actual: spawns=${agg.spawnCount}, handlerMsgs=${agg.handlerMsgCount}, infoMsgs=${agg.infoMsgCount}`);
            log(`Per-worker rates (ops/sec):`, agg.workerRates);
            log(`Per-worker durations (ms):`, agg.workerDurations);
            log(`Elapsed (ms): ${elapsed}`);

            process.exit(agg.totalExitCode);
        } catch (e) {
            console.error(e);
            process.exit(1);
        }
    })();
} else {
    (async () => {
        try {
            const res = await workerRun(workerData);
            parentPort.postMessage(res);
        } catch (e) {
            throw e;
        }
    })();
}
