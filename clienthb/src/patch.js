import fs from 'fs';
import { createSigner } from '@permaweb/ao-core-libs';
import { connect } from '@permaweb/aoconnect';

import { parseArgs } from './utils.js';

const { group, flags } = parseArgs('patch.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

if (!config[group]) {
  throw new Error(`Group "${group}" not found in ${configPath}`);
}

const MAINNET_URL = flags.url || config[group].url;
const MAINNET_SCHEDULER = flags.scheduler || config[group].schedulerAddress;
const WALLET = config[group].wallet;
const SIGNER = createSigner(WALLET);

function log(...args) {
    console.log(`\x1b[32m[HB Client Patch]\x1b[0m`, ...args);
}

function logError(...args) {
    console.log(`\x1b[35m[Error]\x1b[0m`, ...args);
}

const indexLengths = [1000, 5000, 10_000, 25_000, 63_500];

(async function () {
    const ao = connect({
        MODE: 'mainnet',
        URL: MAINNET_URL,
        SCHEDULER: MAINNET_SCHEDULER,
        signer: SIGNER
    });

    for (const length of indexLengths) {
        log(`Index Length: ${length}`);

        const processId = await ao.spawn({
            tags: [{ name: 'Name', value: new Date().getTime().toString() }],
        });

        log(`Process ID: ${processId}`);

        const handlerAddMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Eval' }],
            data: `
            local json = require('json')
            Index = Index or {}
            Handlers.add('Set-Index', 'Set-Index', function(msg)
                Index = json.decode(msg.Data)
                Send({ device = 'patch@1.0', zone = json.encode(Index) })
            end)
            `,
            signer: SIGNER,
        });

        log(`Added Handlers | Message: ${handlerAddMessage}`);

        const data = JSON.stringify({
            Index: Array.from({ length: length }, (_, i) => ({
                Id: i,
                Data: 'x'.repeat(100),
                DateCreated: new Date().getTime().toString()
            }))
        });

        const t0Send = performance.now();

        log(`Setting Index...`);

        const setIndexMessage = await ao.message({
            process: processId,
            tags: [{ name: 'Action', value: 'Set-Index' }],
            data: data,
            signer: SIGNER,
        });

        if (setIndexMessage) {
            log(`Set Index | Message: ${setIndexMessage}`);

            const t1Send = performance.now();

            const durationMsSend = Math.round(t1Send - t0Send);
            const durationSecSend = (durationMsSend / 1000).toFixed(2);

            log(`Message Send: ${durationMsSend}ms (${durationSecSend}s)`);

            const t0Fetch = performance.now();

            const response = await fetch(`${MAINNET_URL}/${processId}/now/zone`);

            const t1Fetch = performance.now();

            const buf = Buffer.from(await response.arrayBuffer());

            const sizeBytes = buf.length;
            const sizeMB = (sizeBytes / (1024 * 1024)).toFixed(2);
            const sizeGB = (sizeBytes / (1024 * 1024 * 1024)).toFixed(2);

            log(`Raw size: ${sizeBytes} bytes (${sizeMB} MB, ${sizeGB} GB)`);

            const durationMsFetch = Math.round(t1Fetch - t0Fetch);
            const durationSecFetch = (durationMsFetch / 1000).toFixed(2);

            log(`Fetch: ${durationMsFetch}ms (${durationSecFetch}s)`);
        }
        else logError(`Set Index Failed (Index Length: ${length})`);

        if (length !== indexLengths[indexLengths.length - 1]) console.log('-'.repeat(75));
    }

    process.exit(0);
})();

