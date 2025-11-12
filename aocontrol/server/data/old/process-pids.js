import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

// Get __dirname equivalent in ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Paths
const resultsDir = path.join(__dirname, 'data', 'results');
const allPidsPath = path.join(__dirname, 'data', 'allpids.json');

async function processPids() {
  try {
    console.log('Reading allpids.json...');
    const allPidsData = JSON.parse(fs.readFileSync(allPidsPath, 'utf8'));

    // Get existing processes to avoid duplicates
    const existingProcesses = new Set(allPidsData.processes || []);
    const newTokens = [];

    console.log('Reading result files from data/results...');
    const files = fs.readdirSync(resultsDir).filter(f => f.endsWith('.json'));

    console.log(`Found ${files.length} JSON files`);

    // Process each file
    for (const file of files) {
      const filePath = path.join(resultsDir, file);
      console.log(`Processing ${file}...`);

      const data = JSON.parse(fs.readFileSync(filePath, 'utf8'));

      // Extract tokens from results array
      if (data.results && Array.isArray(data.results)) {
        for (const result of data.results) {
          if (result.token && !existingProcesses.has(result.token)) {
            newTokens.push(result.token);
            existingProcesses.add(result.token);
          }
        }
      }
    }

    console.log(`Found ${newTokens.length} new tokens`);

    // Add new tokens to processes array
    allPidsData.processes = allPidsData.processes || [];
    allPidsData.processes.push(...newTokens);

    // Deduplicate processes array
    const uniqueProcesses = [...new Set(allPidsData.processes)];
    const duplicatesRemoved = allPidsData.processes.length - uniqueProcesses.length;
    allPidsData.processes = uniqueProcesses;

    if (duplicatesRemoved > 0) {
      console.log(`Removed ${duplicatesRemoved} duplicate(s)`);
    }

    console.log(`Total processes: ${allPidsData.processes.length}`);

    // Create hydrations for all processes
    console.log('Creating hydrations...');
    allPidsData.hydrations = allPidsData.hydrations || {};

    for (const pid of allPidsData.processes) {
      if (!allPidsData.hydrations[pid]) {
        allPidsData.hydrations[pid] = [
          {
            url: 'https://push-router.forward.computer',
            status: 'LOADED'
          }
        ];
      }
    }

    console.log(`Created hydrations for ${allPidsData.processes.length} processes`);

    // Save the updated file
    console.log('Saving allpids.json...');
    fs.writeFileSync(allPidsPath, JSON.stringify(allPidsData, null, 4));

    console.log('Done!');
    console.log(`Summary:`);
    console.log(`  - Total processes: ${allPidsData.processes.length}`);
    console.log(`  - New tokens added: ${newTokens.length}`);
    console.log(`  - Total hydrations: ${Object.keys(allPidsData.hydrations).length}`);

  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

processPids();
