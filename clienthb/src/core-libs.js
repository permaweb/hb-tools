
import AOCore, { createSigner } from '@permaweb/ao-core-libs';

import { parseArgs } from './utils.js';

const { group, flags } = parseArgs('message-passing.js');
const configPath = flags.config || 'config.json';

const MAINNET_URL = config[group].url;
const MAINNET_SCHEDULER = config[group].schedulerAddress;
const jwk = config[group].wallet;

const aoCore = AOCore.init({ signer: createSigner(jwk), url: MAINNET_URL });

async function run() {

}

run().then(() => { 
  process.exit(0); 
}).catch(err => { 
  console.error(err); 
  process.exit(1); 
});