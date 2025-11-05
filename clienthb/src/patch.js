import fs from 'fs';
import { connect, createSigner } from '@permaweb/aoconnect';

import { parseArgs, expect, createTestRunner } from './utils.js';

const { group, flags } = parseArgs('patch.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

if (!config[group]) {
    throw new Error(`Group '${group}' not found in ${configPath}`);
}

const groupConfig = { ...config.defaults, ...config[group] };

const MAINNET_URL = flags.url || groupConfig.url;
const MAINNET_SCHEDULER = flags.scheduler || groupConfig.schedulerAddress;
const WALLET = JSON.parse(fs.readFileSync(process.env.PATH_TO_WALLET));
const SIGNER = createSigner(WALLET);

function log(...args) {
    console.log(`\x1b[36m[HB Client Patch]\x1b[0m`, ...args);
}

const indexLengths = [1000, 5000, 10_000, 25_000];

(async function () {
    const runner = createTestRunner();
    const ao = connect({
        MODE: 'mainnet',
        URL: MAINNET_URL,
        SCHEDULER: MAINNET_SCHEDULER,
        signer: SIGNER
    });

    let processId;
    await runner.test(async () => {
        processId = await ao.spawn({
            module: groupConfig.aosModule,
            tags: [{ name: 'Name', value: new Date().getTime().toString() }],
        });
        expect(processId).toEqualType('string');
        log(`Process ID: ${processId}`);
    });

    await runner.test(async () => {
        const patchMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `
            Index = { 1, 2, 3, 4, 5 }
            Send({ device = 'patch@1.0', zone = Index })
            Index = { 1 }
            Send({ device = 'patch@1.0', zone = Index })
            Index = { 1, 2, 3 }
            Send({ device = 'patch@1.0', zone = Index })
            Index = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }
            Send({ device = 'patch@1.0', zone = Index })
                `,
            signer: SIGNER,
        });
        expect(patchMessage).toEqualType('number');
        log(`Patch | Message: ${patchMessage}`);
    });

    await runner.test(async () => {
        const t0Fetch = performance.now();

        const response = await fetch(`${MAINNET_URL}/${processId}/now/zone?require-codec=application/json&accept-bundle=true`);
        const json = await response.json();
        const data = json.body;

        expect(response.ok).toEqual(true);
        expect(data.length).toEqual(10);

        const t1Fetch = performance.now();

        const durationMsFetch = Math.round(t1Fetch - t0Fetch);
        const durationSecFetch = (durationMsFetch / 1000).toFixed(2);

        log(`Fetch: ${durationMsFetch}ms (${durationSecFetch}s)`);
    });

    const exitCode = runner.getSummary('Patch Tests');
    process.exit(exitCode);
})();

