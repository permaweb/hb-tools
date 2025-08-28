
import fs from 'fs';
import AOCore, { createSigner } from '@permaweb/ao-core-libs';

import { parseArgs } from './utils.js';

const { group, flags } = parseArgs('core-libs.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

if (!config[group]) {
  throw new Error(`Group "${group}" not found in ${configPath}`);
}

const MAINNET_URL = flags.url || config[group].url;
const MAINNET_SCHEDULER = flags.scheduler || config[group].schedulerAddress;
const jwk = config[group].wallet;

let scheduler = MAINNET_SCHEDULER
const authority = scheduler
const module = 'URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk';
const aoCore = AOCore.init({ signer: createSigner(jwk), url: MAINNET_URL });

const baseParams = {
  method: 'POST',
  'signing-format': 'ans104',
  'accept-bundle': 'true',
  'accept-codec': 'httpsig@1.0'
}

const getAOParams = (type) => ({
  Type: type,
  'Data-Protocol': 'ao',
  Variant: 'ao.N.1'
})

const getTags = (args) =>
  args.tags
    ? Object.fromEntries(args.tags.map(tag => [tag.name, tag.value]))
    : {}

const getData = (args) => args.data ?? '1984'

function log(...args) {
    console.log(`\x1b[36m[Core Libs]\x1b[0m`, ...args);
}

async function retryInitPush(processId, maxAttempts = 10, Name) {
  const params = {
    path: `/${processId}~process@1.0/push/serialize~json@1.0`,
    target: processId,
    Action: 'Eval',
    data: 'require(\'.process\')._version',
    Name,
    ...getAOParams('Message'),
    ...baseParams
  }

  let lastError = null

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      const initPush = await aoCore.request(params)
      if (initPush.ok) {
        return initPush
      } else {
        lastError = new Error(`Init push returned ok=false (status=${initPush.status})`);
        await new Promise((r) => setTimeout(r, 500));
      }
    } catch (err) {
      log('warn', `Init push attempt ${attempt} threw`, err)
      lastError = err
    }

    if (attempt === maxAttempts) break
  }

  throw new Error(`Init push failed after ${maxAttempts} attempts: ${lastError?.message || 'unknown'}`)
}

async function runAOFlow() {
  let n = Date.now().toString();

  const args = {
    tags: [
      { name: 'Name', value: n }
    ]
  }
  
  const params = {
    path: '/push',
    device: 'process@1.0',
    scheduler,
    'scheduler-location': scheduler,
    'scheduler-device': 'scheduler@1.0',
    'push-device': 'push@1.0',
    'execution-device': 'genesis-wasm@1.0',
    Authority: authority,
    Module: module,
    data: getData(args),
    ...getTags(args),
    ...getAOParams('Process'),
    ...baseParams
  };

  const response = await aoCore.request(params);

  const processId = response.headers.get('process');

  await retryInitPush(processId, 10, n);

  log(`Spawned process ${processId}...`);

  const msgArgs = {
    tags: [
      { name: 'Action', value: 'Info'}
    ],
    data: 'hello'
  }

  const messageParams = {
    path: `/${processId}~process@1.0/push/serialize~json@1.0`,
    target: processId,
    data: getData(msgArgs),
    ...getTags(msgArgs),
    ...getAOParams('Message'),
    ...baseParams
  }

  const messageResponse = await aoCore.request(messageParams)

  const parsedResponse = await messageResponse.json()

  if(messageResponse.ok) {
    const resultParams = {
      path: `/${processId}~process@1.0/compute/serialize~json@1.0`,
      target: processId,
      slot: parsedResponse.slot,
      data: getData(args),
      ...getTags(args),
      ...baseParams,
      method: 'POST',
      'signing-format': 'ans104'
    }

    const resultResponse = await aoCore.request(resultParams)

    if(resultResponse.status == 200) {
      log(`Read result, AO flow was successful!`);
    }
  }
}

async function run() {
  await runAOFlow();
}

run().then(() => { 
  process.exit(0); 
}).catch(err => { 
  console.error(err); 
  process.exit(1); 
});