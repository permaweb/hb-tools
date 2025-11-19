import fs from 'fs';
import { connect, createSigner } from '@permaweb/aoconnect';

import { expect, parseArgs, createTestRunner } from './utils.js';

const { group, flags } = parseArgs('compatibility.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

const groupConfig = { ...config.defaults, ...config[group] };

const MAINNET_URL = flags.url || groupConfig.url;
const MAINNET_SCHEDULER = flags.scheduler || groupConfig.schedulerAddress;
const WALLET = JSON.parse(fs.readFileSync(process.env.PATH_TO_WALLET));
const SIGNER = createSigner(WALLET);

function modeLog(mode, ...args) {
    const ansi = mode === 'mainnet' ? '36' : '90';
    console.log(`\x1b[${ansi}m[${mode.toUpperCase()}]\x1b[0m`, ...args);
}

let exitCode = 0;

async function runConnect(mode) {
    const runner = createTestRunner();

    const useMainnet = mode === 'mainnet';

    let connectDeps = { MODE: useMainnet ? 'mainnet' : 'legacy', signer: SIGNER };
    if (useMainnet) {
        connectDeps.URL = MAINNET_URL;
        connectDeps.SCHEDULER = MAINNET_SCHEDULER;
    };

    const ao = connect(connectDeps);

    let spawnArgs = {
        tags: [
            { name: 'Name', value: Date.now().toString() },
            { name: 'Scheduler', value: MAINNET_SCHEDULER },
            { name: 'Module', value: groupConfig.aosModule },
            { name: 'Content-Type', value: 'text/plain' },
        ],
    };

    spawnArgs.module = groupConfig.aosModule;
    
    if (!useMainnet) {
        spawnArgs.scheduler = '_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA';
        spawnArgs.signer = SIGNER;
    }

    let processId;
    await runner.test(async () => {
        const start = Date.now();
        processId = await ao.spawn(spawnArgs);
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        modeLog(mode, `Process ID: ${processId} (${duration}s)`);
        expect(processId).toEqualType('string');
    });

    let versionMessage;
    await runner.test(async () => {
        const start = Date.now();
        versionMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `require('.process')._version`,
            signer: SIGNER
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        modeLog(mode, `Message: ${versionMessage} (${duration}s)`);
        expect(versionMessage).toEqualType('number');
    });

    let versionResult;
    await runner.test(async () => {
        const start = Date.now();
        versionResult = await ao.result({
            process: processId,
            message: versionMessage
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        const data = versionResult.Output?.data;
        modeLog(mode, `Result Data: ${data} (${duration}s)`);
        expect(data).toEqualType('string');
    });

    let handlerAddMessage;
    await runner.test(async () => {
        const start = Date.now();
        handlerAddMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `
                Handlers.add('Info', 'Info', function(msg)
                    ao.send({
                        Target = msg.From,
                        Data = require('json').encode({ Hello = 'World' })
                    })
                    ao.send({
                        Target = msg.From,
                        Data = require('json').encode({ Hello = 'World 2' })
                    })
                end)
            `,
            signer: SIGNER,
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        modeLog(mode, `Added Handlers | Message: ${handlerAddMessage} (${duration}s)`);
        expect(handlerAddMessage).toEqualType('number');
    });

    let handlerAddResult;
    await runner.test(async () => {
        const start = Date.now();
        handlerAddResult = await ao.result({
            process: processId,
            message: handlerAddMessage
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        const data = handlerAddResult.Output?.data;
        modeLog(mode, `Result Data: ${data} (${duration}s)`);
        expect(data).toEqualType('string');
    });

    let handlerReadMessage;
    await runner.test(async () => {
        const start = Date.now();
        handlerReadMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `Handlers.list`,
            signer: SIGNER,
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        modeLog(mode, `Read Handlers | Message: ${handlerReadMessage} (${duration}s)`);
        expect(handlerReadMessage).toEqualType('number');
    });

    let handlerReadResult;
    await runner.test(async () => {
        const start = Date.now();
        handlerReadResult = await ao.result({
            process: processId,
            message: handlerReadMessage
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        const data = handlerReadResult.Output?.data;
        modeLog(mode, `Result Data: ${data} (${duration}s)`);
        expect(data).toEqualType('string');
    });

    let infoMessage;
    await runner.test(async () => {
        const start = Date.now();
        infoMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Info' }],
            signer: SIGNER
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        modeLog(mode, `Action: Info | Message: ${infoMessage} (${duration}s)`);
        expect(infoMessage).toEqualType('number');
    });

    let infoResult;
    await runner.test(async () => {
        const start = Date.now();
        infoResult = await ao.result({
            process: processId,
            message: infoMessage
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        const messages = infoResult?.Messages;
        modeLog(mode, `Info Result Messages: ${JSON.stringify(messages, null, 2)} (${duration}s)`);
        expect(messages).toEqualLength(2);
    });

    let results;
    await runner.test(async () => {
        const start = Date.now();
        results = await ao.results({
            process: processId
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        const messages = results?.edges?.[0].node?.Messages;
        modeLog(mode, `Results Messages: ${JSON.stringify(messages, null, 2)} (${duration}s)`);
        expect(messages).toEqualLength(2);
    });

    let dryrun;
    await runner.test(async () => {
        const start = Date.now();
        dryrun = await ao.dryrun({
            process: processId,
            tags: [
                { name: 'Action', value: 'Info' },
                { name: 'Test', value: 'Dryrun' }
            ]
        });
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        modeLog(mode, `Dryrun | Result: ${JSON.stringify(dryrun, null, 2)} (${duration}s)`);
    });

    exitCode = runner.getSummary('HB Tools Compatibility Tests');
}


(async function () {
    const startTime = Date.now();
    await runConnect('mainnet');
    const endTime = Date.now();
    const durationSeconds = ((endTime - startTime) / 1000).toFixed(2);
    console.log(`\n\x1b[1mTotal test duration: ${durationSeconds} seconds\x1b[0m`);
    process.exit(exitCode);
})();