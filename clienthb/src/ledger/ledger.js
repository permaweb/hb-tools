import fs from 'fs'

import Arweave from 'arweave';
import { connect, createSigner } from '@permaweb/aoconnect';
import { parseArgs } from './utils.js';

const { group, flags } = parseArgs('message-passing.js');
const configPath = flags.config || 'config.json';
const config = JSON.parse(fs.readFileSync(configPath));

const AO_HOLDER_WALLET = JSON.parse(fs.readFileSync(process.env.PATH_TO_AO_HOLDER_WALLET));

async function run() {
  const aoLegacy = connect({ 
    MODE: 'legacy'
  });

  const aoHolderAddress = await Arweave.init({}).wallets.jwkToAddress(AO_HOLDER_WALLET);
  console.log(`AO Holder Address: ${aoHolderAddress}`);

  const topupAddress = aoHolderAddress;

  const currentBetaGZAOBalance = (await aoLegacy.dryrun({
      process: config[group]['beta-gz-ao-token'],
      tags: [
          { name: 'Action', value: 'Balance' },
          { name: 'Recipient', value: aoHolderAddress }
      ]
  })).Messages[0].Data;

  console.log(`Current Beta Green Zone AO Balance: ${currentBetaGZAOBalance}`);

  const sendAmount = flags.amount;

  console.log(`Send Amount: ${sendAmount}`);

  console.log('Transferring AO to Beta Green Zone AO...');

  const transferId = await aoLegacy.message({
      process: config[group]['ao-token'],
      tags: [
          { name: 'Action', value: 'Transfer' },
          { name: 'Quantity', value: sendAmount },
          { name: 'Recipient', value: config[group]['beta-gz-ao-token'] },
      ],
      signer: createSigner(AO_HOLDER_WALLET)
  });

  console.log(`Transfer: ${transferId}`);

  await aoLegacy.result({
      process: config[group]['ao-token'],
      message: transferId
  });

  let updatedBetaGZAOBalance = 0;
  do {
      console.log('Getting Updated Beta Green Zone AO Balance...');

      await new Promise((r) => setTimeout(r, 20000));
      updatedBetaGZAOBalance = (await aoLegacy.dryrun({
          process: config[group]['beta-gz-ao-token'],
          tags: [
              { name: 'Action', value: 'Balance' },
              { name: 'Recipient', value: aoHolderAddress }
          ]
      })).Messages[0].Data;

      console.log(`Updated Beta Green Zone AO Balance: ${updatedBetaGZAOBalance}`);
  }
  while (updatedBetaGZAOBalance === currentBetaGZAOBalance);

  const aoMainnet = connect({
      MODE: 'mainnet',
      URL: config[group]['url'],
      signer: createSigner(AO_HOLDER_WALLET)
  });

  const ledgerAddressRes = await fetch(`${config[group]['url']}/ledger~node-process@1.0/commitments/keys/1`);
  const ledgerAddress = await ledgerAddressRes.text();
  console.log(`Ledger Address: ${ledgerAddress}`);

  // console.log('Fetching current node ledger...');
  // let ledgerRes = await fetch(`${config[group]['url']}/ledger~node-process@1.0/now/balance`, {
  //     headers: { 'accept': 'application/json', 'accept-bundle': 'true' }
  // });
  // let ledger = await ledgerRes.json();

  // console.log('-'.repeat(20));
  // console.log('Current Ledger');
  // console.log(ledger);
  // console.log('-'.repeat(20));

  // const path = `/${config[group]['beta-gz-ao-token']}~process@1.0/push`;

  // console.log(`Path: ${path}`);
  // console.log(`Router Address: ${routerAddress}`);
  // console.log(`Ledger Address: ${ledgerAddress}`);
  // console.log('-'.repeat(20));

  // console.log('Transferring Beta Green Zone AO to Subledger...');
  // const transferParams = {
  //     type: 'Message',
  //     path: path,
  //     method: 'POST',
  //     'data-protocol': 'ao',
  //     variant: 'ao.N.1',
  //     target: config['beta-gz-ao-token'],
  //     'accept-bundle': 'true',
  //     'accept-codec': 'httpsig@1.0',
  //     'signingFormat': 'ANS-104',
  //     action: 'Transfer',
  //     Recipient: topupAddress,
  //     Route: ledgerAddress,
  //     Quantity: topupAmount
  // };

  // const res = await aoMainnet.request(transferParams);
  // console.log(`Transfer status: ${res.status}`);

  // ledgerRes = await fetch(`${node}/ledger~node-process@1.0/now/balance`, {
  //     headers: { 'accept': 'application/json', 'accept-bundle': 'true' }
  // });
  // ledger = await ledgerRes.json();

  // console.log('-'.repeat(20));
  // console.log('Updated Ledger');
  // console.log(ledger);
  // console.log('-'.repeat(20));
}

run().then(() => { 
  process.exit(0) 
}).catch(err => { 
  console.error(err); 
  process.exit(1); 
});

