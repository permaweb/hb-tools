import fs from 'fs';
import { connect, createSigner } from '@permaweb/aoconnect';

import { parseArgs, expect, createTestRunner } from './utils.js';

const { group, flags } = parseArgs('rate-limits.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

let groupConfig;

function log(...args) {
  console.log(`\x1b[36m[Rate limits]\x1b[0m`, ...args);
}

async function run() {
    const runner = createTestRunner();
    runner.start();
    log(`Running rate limit tests...`);
    if (!config[group]) throw new Error(`Group '${group}' not found in ${configPath}`);

    groupConfig = { ...config.defaults, ...config[group] };

    const MAINNET_URL = flags.url || groupConfig.url;
    const MAINNET_SCHEDULER = flags.scheduler || groupConfig.schedulerAddress;
    const WALLET = JSON.parse(fs.readFileSync(process.env.PATH_TO_WALLET));

    const SIGNER = createSigner(WALLET);
    const ao = connect({
        MODE: 'mainnet',
        URL: MAINNET_URL,
        SCHEDULER: MAINNET_SCHEDULER,
        signer: SIGNER,
    });

    return runner.getSummary('Rate Limit Tests');
}

run().then((exitCode) => {
  process.exit(exitCode);
}).catch(err => {
  console.error(err);
  process.exit(1);
});