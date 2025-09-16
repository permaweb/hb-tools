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

    let spawnArgs = { tags: [
        { name: 'Name', value: Date.now().toString() },
        { name: 'Content-Type', value: 'text/html' } // BREAKS
    ] };

    spawnArgs.module = groupConfig.aosModule;

    if (!useMainnet) {
        spawnArgs.scheduler = '_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA';
        spawnArgs.signer = SIGNER;
    }

    let processId;
    await runner.test(async () => {
        processId = await ao.spawn(spawnArgs);
        modeLog(mode, `Process ID: ${processId}`);
        expect(processId).toEqualType('string');
    });

    let versionMessage;
    await runner.test(async () => {
        versionMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `require('.process')._version`,
            signer: SIGNER
        });
        modeLog(mode, `Message: ${versionMessage}`);
        expect(versionMessage).toEqualType('number');
    });

    let versionResult;
    await runner.test(async () => {
        versionResult = await ao.result({
            process: processId,
            message: versionMessage
        });
        const data = versionResult.Output?.data;
        modeLog(mode, `Result Data: ${data}`);
        expect(data).toEqualType('string');
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
        modeLog(mode, `Added Handlers | Message: ${handlerAddMessage}`);
        expect(handlerAddMessage).toEqualType('number');
    });

    let handlerAddResult;
    await runner.test(async () => {
        handlerAddResult = await ao.result({
            process: processId,
            message: handlerAddMessage
        });
        const data = handlerAddResult.Output?.data;
        modeLog(mode, `Result Data: ${data}`);
        expect(data).toEqualType('string');
    });

    let handlerReadMessage;
    await runner.test(async () => {
        handlerReadMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `Handlers.list`,
            signer: SIGNER,
        });
        modeLog(mode, `Read Handlers | Message: ${handlerReadMessage}`);
        expect(handlerReadMessage).toEqualType('number');
    });

    let handlerReadResult;
    await runner.test(async () => {
        handlerReadResult = await ao.result({
            process: processId,
            message: handlerReadMessage
        });
        const data = handlerReadResult.Output?.data;
        modeLog(mode, `Result Data: ${data}`);
        expect(data).toEqualType('string');
    });

    let infoMessage;
    await runner.test(async () => {
        infoMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Info' }],
            signer: SIGNER
        });
        modeLog(mode, `Action: Info | Message: ${infoMessage}`);
        expect(infoMessage).toEqualType('number');
    });

    let infoResult;
    await runner.test(async () => {
        infoResult = await ao.result({
            process: processId,
            message: infoMessage
        });
        const messages = infoResult?.Messages;
        modeLog(mode, `Info Result Messages: ${JSON.stringify(messages, null, 2)}`);
        expect(messages).toEqualLength(2);
    });

    let results;
    await runner.test(async () => {
        results = await ao.results({
            process: processId
        });
        const messages = results?.edges?.[0].node?.Messages;
        modeLog(mode, `Results Messages: ${JSON.stringify(messages, null, 2)}`);
        expect(messages).toEqualLength(2);
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
        modeLog(mode, `Dryrun | Result: ${JSON.stringify(dryrun, null, 2)}`);
    });

    exitCode = runner.getSummary('HB Tools Compatibility Tests');
}

(async function () {
    await runConnect('mainnet');
    process.exit(exitCode);
})();