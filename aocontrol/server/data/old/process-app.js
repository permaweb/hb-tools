import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

// Get __dirname equivalent in ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Paths
const appPath = path.join(__dirname, 'pids-app.json');
const outputPath = path.join(__dirname, 'pids-app-all.json');

async function loadApp() {
  try {
    console.log('Reading defcons.json...');
    const defconsData = JSON.parse(fs.readFileSync(appPath, 'utf8'));

    // Create a merged map: defcons first, then override with defcons-override
    const mergedMap = {};

    // Add all from defcons.json
    console.log(`Processing ${Object.keys(defconsData).length} entries from defcons.json...`);
    for (const [pid, url] of Object.entries(defconsData)) {
      mergedMap[pid] = url;
    }

    // Create the output structure
    const output = {
      processes: Object.keys(mergedMap),
      hydrations: {}
    };

    // Create hydrations for all processes
    console.log('Creating hydrations...');
    for (const [pid, url] of Object.entries(mergedMap)) {
      output.hydrations[pid] = [
        {
          url: url,
          status: 'INIT'
        },
        {
          url: 'https://app-2.forward.computer',
          status: 'INIT'
        }
      ];
    }

    console.log(`Total processes: ${output.processes.length}`);
    console.log(`Total hydrations: ${Object.keys(output.hydrations).length}`);

    // Save the output file
    console.log('Saving pids-app-all.json...');
    fs.writeFileSync(outputPath, JSON.stringify(output, null, 4));

    console.log('Done!');
    console.log(`Summary:`);
    console.log(`  - Total processes: ${output.processes.length}`);
    console.log(`  - Entries from defcons.json: ${Object.keys(defconsData).length}`);
    console.log(`  - Overridden entries: ${overrideCount}`);
    console.log(`  - Output file: pids-new.json`);

  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

loadApp();
