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
    runner.start();

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
    spawnArgs.authority = groupConfig.authority;
    
    if (!useMainnet) {
        spawnArgs.scheduler = '_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA';
        spawnArgs.signer = SIGNER;
    }

    let processId;
    await runner.test(async () => {
        processId = await ao.spawn(spawnArgs);
        expect(processId).toEqualType('string');
        return processId;
    }).then(({ result, duration }) => {
        modeLog(mode, `Process ID: ${result} (${duration}s)`);
    });

    let versionMessage;
    await runner.test(async () => {
        versionMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `require('.process')._version`,
            signer: SIGNER
        });
        expect(versionMessage).toEqualType('number');
        return versionMessage;
    }).then(({ result, duration }) => {
        modeLog(mode, `Message: ${result} (${duration}s)`);
    });

    let versionResult;
    await runner.test(async () => {
        versionResult = await ao.result({
            process: processId,
            message: versionMessage
        });
        const data = versionResult.Output?.data;
        expect(data).toEqualType('string');
        return data;
    }).then(({ result, duration }) => {
        modeLog(mode, `Result Data: ${result} (${duration}s)`);
    });

    let handlerAddMessage;
    await runner.test(async () => {
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
        expect(handlerAddMessage).toEqualType('number');
        return handlerAddMessage;
    }).then(({ result, duration }) => {
        modeLog(mode, `Added Handlers | Message: ${result} (${duration}s)`);
    });

    let handlerAddResult;
    await runner.test(async () => {
        handlerAddResult = await ao.result({
            process: processId,
            message: handlerAddMessage
        });
        const data = handlerAddResult.Output?.data;
        expect(data).toEqualType('string');
        return data;
    }).then(({ result, duration }) => {
        modeLog(mode, `Result Data: ${result} (${duration}s)`);
    });

    let handlerReadMessage;
    await runner.test(async () => {
        handlerReadMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `Handlers.list`,
            signer: SIGNER,
        });
        expect(handlerReadMessage).toEqualType('number');
        return handlerReadMessage;
    }).then(({ result, duration }) => {
        modeLog(mode, `Read Handlers | Message: ${result} (${duration}s)`);
    });

    let handlerReadResult;
    await runner.test(async () => {
        handlerReadResult = await ao.result({
            process: processId,
            message: handlerReadMessage
        });
        const data = handlerReadResult.Output?.data;
        expect(data).toEqualType('string');
        return data;
    }).then(({ result, duration }) => {
        modeLog(mode, `Result Data: ${result} (${duration}s)`);
    });

    let infoMessage;
    await runner.test(async () => {
        infoMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Info' }],
            signer: SIGNER
        });
        expect(infoMessage).toEqualType('number');
        return infoMessage;
    }).then(({ result, duration }) => {
        modeLog(mode, `Action: Info | Message: ${result} (${duration}s)`);
    });

    let infoResult;
    await runner.test(async () => {
        infoResult = await ao.result({
            process: processId,
            message: infoMessage
        });
        const messages = infoResult?.Messages;
        expect(messages).toEqualLength(2);
        return messages;
    }).then(({ result, duration }) => {
        modeLog(mode, `Info Result Messages: ${JSON.stringify(result, null, 2)} (${duration}s)`);
    });

    let results;
    await runner.test(async () => {
        results = await ao.results({
            process: processId
        });
        const messages = results?.edges?.[0].node?.Messages;
        expect(messages).toEqualLength(2);
        return messages;
    }).then(({ result, duration }) => {
        modeLog(mode, `Results Messages: ${JSON.stringify(result, null, 2)} (${duration}s)`);
    });

    let dryrun;
    await runner.test(async () => {
        dryrun = await ao.dryrun({
            process: processId,
            tags: [
                { name: 'Action', value: 'Info' },
                { name: 'Test', value: 'Dryrun' }
            ]
        });
        return dryrun;
    }).then(({ result, duration }) => {
        modeLog(mode, `Dryrun | Result: ${JSON.stringify(result, null, 2)} (${duration}s)`);
    });

    exitCode = runner.getSummary('HB Tools Compatibility Tests');
}


(async function () {
    await runConnect('mainnet');
    process.exit(exitCode);
})();