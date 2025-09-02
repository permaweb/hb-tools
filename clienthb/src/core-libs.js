import fs from 'fs';
import AOCore, { createSigner } from '@permaweb/ao-core-libs';

import { parseArgs, expect, createTestRunner } from './utils.js';

const { group, flags } = parseArgs('core-libs.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

if (!config[group]) {
  throw new Error(`Group '${group}' not found in ${configPath}`);
}

const MAINNET_URL = flags.url || config[group].url;
const MAINNET_SCHEDULER = flags.scheduler || config[group].schedulerAddress;
const jwk = JSON.parse(fs.readFileSync(process.env.PATH_TO_WALLET));

let scheduler = MAINNET_SCHEDULER;
const authority = scheduler;
const module = config[group].aosModule;
const aoCore = AOCore.init({ signer: createSigner(jwk), url: MAINNET_URL });

const baseParams = {
  method: 'POST',
  'signing-format': 'ans104',
  'accept-bundle': 'true',
  'accept-codec': 'httpsig@1.0'
}

const jsonParams = {
  'signing-format': 'ans104',
  'accept': 'application/json',
  'accept-bundle': 'true'
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
    path: `/${processId}~process@1.0/push`,
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
  const runner = createTestRunner();
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

  let response;
  let processId;

  await runner.test(async () => {
    response = await aoCore.request(params);
    processId = response.headers.get('process');
    expect(processId).toEqualType('string');
  });

  await runner.test(async () => {
    await retryInitPush(processId, 10, n);
    log(`Spawned process ${processId}...`);
  });

  const msgArgs = {
    tags: [
      { name: 'Action', value: 'Info' }
    ],
    data: 'hello'
  }

  const messageParams = {
    path: `/${processId}~process@1.0/push`,
    target: processId,
    data: getData(msgArgs),
    ...getTags(msgArgs),
    ...getAOParams('Message'),
    ...jsonParams
  }

  let messageResponse;
  let parsedResponse;

  await runner.test(async () => {
    messageResponse = await aoCore.request(messageParams);
    parsedResponse = await messageResponse.json();
    expect(messageResponse.ok).toEqual(true);
  });

  if (messageResponse?.ok) {
    const resultParams = {
      path: `/${processId}~process@1.0/compute`,
      target: processId,
      slot: parsedResponse.slot,
      data: getData(args),
      ...getTags(args),
      ...jsonParams
    }

    await runner.test(async () => {
      const resultResponse = await aoCore.request(resultParams);
      expect(resultResponse.status).toEqual(200);

      const resultData = await resultResponse.json();
      console.log('Result Data:', JSON.stringify(resultData, null, 2));
      log(`Read result, AO flow was successful!`);
    });
  }

  return runner.getSummary('Core Libs Tests');
}

async function run() {
  const exitCode = await runAOFlow();
  process.exit(exitCode);
}

run().catch(err => {
  console.error(err);
  process.exit(1);
});