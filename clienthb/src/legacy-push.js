import fs from 'fs';
import { connect, createSigner } from '@permaweb/aoconnect';

import { expect, parseArgs, createTestRunner } from './utils.js';

const { group, flags } = parseArgs('legacy-push.js');
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

async function runLegacyProcessPush() {
    const runner = createTestRunner();

    let connectDepsLegacy = { MODE: 'legacy', signer: SIGNER };

    let connectDepsMainnet = { MODE: 'mainnet', signer: SIGNER }
    connectDepsMainnet.URL = MAINNET_URL;
    connectDepsMainnet.SCHEDULER = MAINNET_SCHEDULER;

    let spawnArgsLegacy = {
        tags: [{ name: 'Name', value: Date.now().toString() }]
    };

    spawnArgsLegacy.module = groupConfig.aosModule;
    spawnArgsLegacy.scheduler = '_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA';
    spawnArgsLegacy.signer = SIGNER;

     let spawnArgsMainnet = {
        tags: [{ name: 'Name', value: Date.now().toString() }]
    };

    spawnArgsMainnet.module = groupConfig.aosModule;

    const aoLegacy = connect(connectDepsLegacy);
    const aoMainnet = connect(connectDepsMainnet);

    let processIdLegacy;
    await runner.test(async () => {
        processIdLegacy = await aoLegacy.spawn(spawnArgsLegacy);
        modeLog('legacy', `Process ID: ${processIdLegacy}`);
        expect(processIdLegacy).toEqualType('string');
    });

    let processIdMainnet;
    await runner.test(async () => {
        processIdMainnet = await aoMainnet.spawn(spawnArgsMainnet);
        modeLog('mainnet', `Process ID: ${processIdMainnet}`);
        expect(processIdMainnet).toEqualType('string');
    });

    let versionMessageMainnetProcess;
    await runner.test(async () => {
        versionMessageMainnetProcess = await aoMainnet.message({
            process: processIdMainnet,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `require('.process')._version`,
            signer: SIGNER
        });
        modeLog('mainnet', `Message: ${versionMessageMainnetProcess}`);
        expect(versionMessageMainnetProcess).toEqualType('number');
    });

    let versionMessageLegacyProcess;
    await runner.test(async () => {
        versionMessageLegacyProcess = await aoMainnet.message({
            process: processIdLegacy,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `require('.process')._version`,
            signer: SIGNER
        });
        modeLog('mainnet', `Message: ${versionMessageLegacyProcess}`);
        expect(versionMessageLegacyProcess).toEqualType('number');
    });

    exitCode = runner.getSummary('Legacy Process Push Tests');
}

(async function () {
    await runLegacyProcessPush();
    process.exit(exitCode);
})();