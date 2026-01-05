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
            { name: 'Content-Type', value: 'text/html' },
            { name: 'On-Boot', value: 'gqzwDfkE7fOGCL2scXHptIX6HoscikEGeqFE31yGZd4' },
        ],
        data: `
            <!DOCTYPE html>
            <html lang="en">
            <head>
                <meta charset="UTF-8" />
                <meta name="viewport" content="width=device-width, initial-scale=1.0" />
                <meta name="robots" content="index, follow, max-image-preview:large, max-snippet:-1, max-video-preview:-1" />
                <title>Portal</title>
            </head>
            <body>
                <div id="portal"></div>
                <script type="module">
                function getGateway() {
                    const host = window.location.hostname;
                    const parts = host.split('.');
                    return \`\${parts[parts.length - 2]}.\${parts[parts.length - 1]}\`;
                }

                const gateway = getGateway();

                const script = document.createElement('script');
                script.type = 'module';
                script.src = \`https://engine_portalenv.\${gateway}\`;
                document.body.appendChild(script);
                </script>
            </body>
            </html>
        `
    };

    spawnArgs.module = groupConfig.aosModule;
    spawnArgs.authority = groupConfig.authority;

    if (!useMainnet) {
        spawnArgs.scheduler = '_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA';
        spawnArgs.signer = SIGNER;
    }

    const excludeFromIndex = JSON.stringify([
        'Balances',
        'Ticker',
        'Process-Type',
        'Total-Supply',
        'Transferable',
        'Metadata.Content',
    ]);

    let processId;
    await runner.test(async () => {
        processId = await ao.spawn(spawnArgs);
        expect(processId).toEqualType('string');
        return processId;
    }).then(({ result, duration }) => {
        modeLog(mode, `Process ID: ${result} (${duration}s)`);
    });

    let indexMessage;
    await runner.test(async () => {
        indexMessage = await ao.message({
            process: processId,
            tags: [
                { name: 'Action', value: 'Send-Index' },
                { name: 'Asset-Type', value: 'blog-post' },
                { name: 'Content-Type', value: 'text/html' },
                { name: 'Date-Added', value: new Date().getTime().toString() },
                { name: 'Exclude', value: excludeFromIndex },
            ],
            data: JSON.stringify({ Recipients: ['_-87e0lp1oJcdBCg6UUzqXpXyOCSvR00RS3ZovJfLko'] }),
            signer: SIGNER
        });
        expect(indexMessage).toEqualType('number');
        return indexMessage;
    }).then(({ result, duration }) => {
        modeLog(mode, `Message: ${result} (${duration}s)`);
    });

    let indexResult;
    await runner.test(async () => {
        indexResult = await ao.result({
            process: processId,
            message: indexMessage
        });
        expect(indexResult?.Messages).toEqualLength(1);
        return indexResult;
    }).then(({ result, duration }) => {
        modeLog(mode, `Result Messages: ${result} (${duration}s)`);
    });

    exitCode = runner.getSummary('HB Tools Content Type Tests');
}


(async function () {
    await runConnect('mainnet');
    process.exit(exitCode);
})();