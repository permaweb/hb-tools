import fs from 'fs';
import { createSigner } from '@permaweb/ao-core-libs';
import { connect } from '@permaweb/aoconnect';

function parseArgs() {
    const args = process.argv.slice(2);
    const flags = {};
    let group = null;
    
    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg.startsWith('--')) {
            const [key, value] = arg.slice(2).split('=');
            if (value !== undefined) {
                flags[key] = value;
            } else if (i + 1 < args.length && !args[i + 1].startsWith('--')) {
                flags[key] = args[++i];
            } else {
                flags[key] = true;
            }
        } else if (!group) {
            group = arg;
        }
    }
    
    if (!group) {
        console.error('Usage: node compatibility.js <group> [--url <url>] [--scheduler <address>] [--config <path>]');
        process.exit(1);
    }
    
    return { group, flags };
}

const { group, flags } = parseArgs();
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

const MAINNET_URL = flags.url || config[group].url;
const MAINNET_SCHEDULER = flags.scheduler || config[group].schedulerAddress;
const WALLET = config[group].wallet;
const SIGNER = createSigner(WALLET);

function modeLog(mode, ...args) {
    const ansi = mode === 'mainnet' ? '36' : '90';
    console.log(`\x1b[${ansi}m[${mode.toUpperCase()}]\x1b[0m`, ...args);
}

async function runConnect(mode) {
    const useMainnet = mode === 'mainnet';

    let connectDeps = { MODE: useMainnet ? 'mainnet' : 'legacy', signer: SIGNER };
    if (useMainnet) {
        connectDeps.URL = MAINNET_URL;
        connectDeps.SCHEDULER = MAINNET_SCHEDULER;
    };

    console.log('connectDeps', connectDeps);
    const ao = connect(connectDeps);

    // /* ------------------------------------------------------------------- */
    // const r = await ao.result({
    //     process: 'VgfmfEW0wrRjjJunPmO5RaO7XyfWFQj5efPyetf2dbE',
    //     message: '3'
    // });

    // console.log(JSON.stringify(r, null, 2));
    // /* ------------------------------------------------------------------- */
    
    let spawnArgs = { tags: [{ name: 'Name', value: Date.now().toString() }] };

    if (!useMainnet) {
        spawnArgs.module = 'URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk';
        spawnArgs.scheduler = '_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA';
        spawnArgs.signer = SIGNER;
    }

    console.log('spawnArgs', spawnArgs);
    const processId = await ao.spawn(spawnArgs);

    modeLog(mode, `Process ID: ${processId}`);

    const versionMessage = await ao.message({
        process: processId,
        tags: [{ name: 'Action', value: 'Eval' }],
        data: `require('.process')._version`,
        signer: SIGNER
    });

    modeLog(mode, `Message: ${versionMessage}`);

    const versionResult = await ao.result({
        process: processId,
        message: versionMessage
    });

    modeLog(mode, `Result: ${JSON.stringify(versionResult, null, 2)}`);

    modeLog(mode, `Result Data: ${versionResult.Output?.data}`);

    const handlerAddMessage = await ao.message({
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

    const handlerAddResult = await ao.result({
        process: processId,
        message: handlerAddMessage
    });

    modeLog(mode, `Added Handlers | Result: ${JSON.stringify(handlerAddResult, null, 2)}`);

    const handlerReadMessage = await ao.message({
        process: processId,
        tags: [{ name: 'Action', value: 'Eval' }],
        data: `Handlers.list`,
        signer: SIGNER,
    });

    modeLog(mode, `Read Handlers | Message: ${handlerReadMessage}`);

    const handlerReadResult = await ao.result({
        process: processId,
        message: handlerReadMessage
    });

    modeLog(mode, `Read Handlers | Result: ${JSON.stringify(handlerReadResult, null, 2)}`);

    const infoMessage = await ao.message({
        process: processId,
        tags: [{ name: 'Action', value: 'Info' }],
        signer: SIGNER
    });

    modeLog(mode, `Action: Info | Message: ${infoMessage}`);

    const infoResult = await ao.result({
        process: processId,
        message: infoMessage
    });

    modeLog(mode, `Action: Info | Result: ${JSON.stringify(infoResult, null, 2)}`);

    const results = await ao.results({
        process: processId
    });

    modeLog(mode, `Results: ${JSON.stringify(results, null, 2)}`);

    await new Promise((r) => setTimeout(r, 1000))

    const dryrun = await ao.dryrun({
        process: processId,
        tags: [
            { name: 'Action', value: 'Info' },
            {name:'test', value:'dryrun'}
        ]
    });

    modeLog(mode, `Dryrun | Result: ${JSON.stringify(dryrun, null, 2)}`);
}

(async function () {
    await runConnect('mainnet');
    // await runConnect('legacy');
    process.exit(0);
})();