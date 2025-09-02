import fs from 'fs'

import Arweave from 'arweave';
import { connect, createSigner } from '@permaweb/aoconnect';
import { parseArgs } from '../utils.js';

const { group, flags } = parseArgs('message-passing.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

const BETA_GZ_OWNER_WALLET = config[group].wallet;

async function run() {
  const aoLegacy = connect({ MODE: 'legacy' });

  const gzAOAddress = await Arweave.init({}).wallets.jwkToAddress(BETA_GZ_OWNER_WALLET);
  console.log(`Greenzone AO Owner Address: ${gzAOAddress}`);

  const authorities = [config[group]['authority'], config[group]['legacy-authority']];

  console.log('Spawning token process...');
  const processId = await aoLegacy.spawn({
    module: config[group]['process-module'],
    scheduler: config[group]['process-scheduler'],
    signer: createSigner(BETA_GZ_OWNER_WALLET),
    data: '1984',
    tags: [
      { name: 'On-Boot', value: config[group]['beta-gz-ao-src'] },
      { name: 'Authority', value: authorities.join(',') },
      { name: 'ParentToken', value: config[group]['ao-token'] },
      { name: 'Name', value: config[group]['beta-gz-ao-name'] },
      { name: 'Ticker', value: config[group]['beta-gz-ao-ticker'] },
      { name: 'Denomination', value: config[group]['beta-gz-ao-denomination'] },
    ]
  });

  console.log(`Process ID: ${processId}`);

  console.log('Setting authorities...');
  const authoritiesUpdate = await aoLegacy.message({
    process: processId,
    type: 'deploy',
    data: `ao.authorities = {'${authorities[0]}', '${authorities[1]}'}`,
    signer: createSigner(BETA_GZ_OWNER_WALLET),
    tags: [
      { name: 'Action', value: 'Eval' },
    ]
  });

  console.log(`Authorities update: ${authoritiesUpdate}`);
}

run().then(() => { 
  process.exit(0) 
}).catch(err => { 
  console.error(err); process.exit(1); 
});