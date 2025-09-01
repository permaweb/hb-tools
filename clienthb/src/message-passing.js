

import fs from 'fs';
import { createSigner } from '@permaweb/ao-core-libs';
import { connect } from '@permaweb/aoconnect';

import { parseArgs } from './utils.js';

const { group, flags } = parseArgs('message-passing.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

function log(...args) {
    console.log(`\x1b[36m[Message Passing]\x1b[0m`, ...args);
}

async function spawnNew(ao, SIGNER) {
  let spawnArgs = { tags: [{ name: 'Name', value: Date.now().toString() }] };

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
  log(`Running message passing tests...`);
  if (!config[group]) throw new Error(`Group "${group}" not found in ${configPath}`);

  const MAINNET_URL = flags.url || config[group].url;
  const MAINNET_SCHEDULER = flags.scheduler || config[group].schedulerAddress;
  const WALLET = config[group].wallet;

  const SIGNER = createSigner(WALLET);
  const ao = connect({
      MODE: 'mainnet',
      URL: MAINNET_URL,
      SCHEDULER: MAINNET_SCHEDULER,
      signer: SIGNER,
  });

  let pid1 = await spawnNew(ao, SIGNER);
  let pid2 = await spawnNew(ao, SIGNER);
  let pid3 = await spawnNew(ao, SIGNER);

  log(`Processes: `, pid1, pid2, pid3);

  const code = `
Handlers.add('Info', 'Info', function(msg)
  ao.send({ Target = msg.From, Data = 'pong', Action = 'Pong' })
  ao.send({ Target = '${pid3}', Data = 'pong to another', Action = 'Pong'}) 
end)
  `.trim();

  ao.message({
      process: pid2,
      tags: [{ name: 'Action', value: 'Eval' }],
      data: code,
      signer: SIGNER,
  });

  const codePong = `
Handlers.add('Pong', 'Pong', function(msg)
  print('Received Pong')
end)
  `.trim();

  ao.message({
      process: pid3,
      tags: [{ name: 'Action', value: 'Eval' }],
      data: codePong,
      signer: SIGNER,
  });

  
  ao.message({
      process: pid1,
      tags: [{ name: 'Action', value: 'Eval' }],
      data: `
ao.send({ Target = '${pid2}', Data = 'ping', Action = 'Info' })
      `.trim(),
      signer: SIGNER,
  });

  await new Promise((r) => setTimeout(r, 15000))

  const resultsPid3 = await ao.results({
      process: pid3
  });

  let data = resultsPid3["edges"][0]["node"]["Output"]["data"];

  if(data !== 'Received Pong') {
    throw new Error(`Did not receive expected message on pid3, got: ${JSON.stringify(resultsPid3["edges"])}`);
  }

  log(`Message passing test successful!`);
}

run().then(() => { 
  process.exit(0); 
}).catch(err => { 
  console.error(err); 
  process.exit(1); 
});