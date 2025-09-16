import fs from 'fs';
import { connect, createSigner } from '@permaweb/aoconnect';

import { parseArgs, expect, createTestRunner } from './utils.js';

const { group, flags } = parseArgs('message-passing.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

let groupConfig;

function log(...args) {
  console.log(`\x1b[36m[Message Passing]\x1b[0m`, ...args);
}

async function spawnNew(ao, SIGNER) {
  let spawnArgs = { module: groupConfig.aosModule, tags: [{ name: 'Name', value: Date.now().toString() }] };

  const processId = await ao.spawn(spawnArgs);

  await ao.message({
    process: processId,
    tags: [{ name: 'Action', value: 'Eval' }],
    data: `require('.process')._version`,
    signer: SIGNER
  });

  return processId;
}

async function run() {
  const runner = createTestRunner();
  log(`Running message passing tests...`);
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

  let pid1, pid2, pid3;

  await runner.test(async () => {
    pid1 = await spawnNew(ao, SIGNER);
    expect(pid1).toEqualType('string');
  });

  await runner.test(async () => {
    pid2 = await spawnNew(ao, SIGNER);
    expect(pid2).toEqualType('string');
  });

  // await runner.test(async () => {
  //   pid3 = await spawnNew(ao, SIGNER);
  //   expect(pid3).toEqualType('string');
  // });

  log(`Processes: ${JSON.stringify([pid1, pid2], null, 2)}`);

  const code = `
    Handlers.add('Ping', 'Ping', function(msg)
      Send({ Target = msg.From, Data = 'Got Ping', Action = 'Got-Ping' })
      Send({ Target = '${pid2}', Data = 'Pong', Action = 'Pong'}) 
    end)
  `.trim();

  await runner.test(async () => {
    const message = await ao.message({
      process: pid1,
      tags: [{ name: 'Action', value: 'Eval' }],
      data: code,
      signer: SIGNER,
    });
    expect(message).toEqualType('number');
  });

  //   const codePong = `
  // Handlers.add('Pong', 'Pong', function(msg)
  //   print('Received Pong')
  // end)
  //   `.trim();

  await runner.test(async () => {
    const message = await ao.message({
      process: pid1,
      tags: [{ name: 'Action', value: 'Ping' }],
      signer: SIGNER,
    });
    expect(message).toEqualType('number');
  });

  //   await runner.test(async () => {
  //     const message = await ao.message({
  //       process: pid3,
  //       tags: [{ name: 'Action', value: 'Eval' }],
  //       data: codePong,
  //       signer: SIGNER,
  //     });
  //     expect(message).toEqualType('number');
  //   });

  //   await runner.test(async () => {
  //     const message = await ao.message({
  //       process: pid1,
  //       tags: [{ name: 'Action', value: 'Eval' }],
  //       data: `
  // ao.send({ Target = '${pid2}', Data = 'ping', Action = 'Info' })
  //         `.trim(),
  //       signer: SIGNER,
  //     });
  //     expect(message).toEqualType('number');
  //   });

  //   await runner.test(async () => {
  //     const resultsPid3 = await ao.results({
  //       process: pid3
  //     });

  //     let data = resultsPid3['edges'][0]['node']['Output']['data'];
  //     expect(data).toEqual('Received Pong');
  //   });

  log(`Message passing test successful!`);

  return runner.getSummary('Message Passing Tests');
}

run().then((exitCode) => {
  process.exit(exitCode);
}).catch(err => {
  console.error(err);
  process.exit(1);
});