import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

// Get __dirname equivalent in ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Paths
const defconsPath = path.join(__dirname, 'defcons.json');
const defconsOverridePath = path.join(__dirname, 'defcons-override.json');
const outputPath = path.join(__dirname, 'pids-new.json');

async function mergeDefcons() {
  try {
    console.log('Reading defcons.json...');
    const defconsData = JSON.parse(fs.readFileSync(defconsPath, 'utf8'));

    console.log('Reading defcons-override.json...');
    const defconsOverrideData = JSON.parse(fs.readFileSync(defconsOverridePath, 'utf8'));

    // Create a merged map: defcons first, then override with defcons-override
    const mergedMap = {};

    // Add all from defcons.json
    console.log(`Processing ${Object.keys(defconsData).length} entries from defcons.json...`);
    for (const [pid, url] of Object.entries(defconsData)) {
      mergedMap[pid] = url;
    }

    // Override with defcons-override.json
    console.log(`Processing ${Object.keys(defconsOverrideData).length} entries from defcons-override.json...`);
    let overrideCount = 0;
    for (const [pid, value] of Object.entries(defconsOverrideData)) {
      // Extract the host URL from the object
      const url = typeof value === 'object' && value.host ? value.host : value;

      if (mergedMap[pid]) {
        overrideCount++;
        console.log(`  Overriding ${pid}: ${mergedMap[pid]} -> ${url}`);
      }
      mergedMap[pid] = url;
    }

    console.log(`Overrode ${overrideCount} duplicate entries`);

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
          status: 'LOADED'
        }
      ];
    }

    console.log(`Total processes: ${output.processes.length}`);
    console.log(`Total hydrations: ${Object.keys(output.hydrations).length}`);

    // Save the output file
    console.log('Saving pids-new.json...');
    fs.writeFileSync(outputPath, JSON.stringify(output, null, 4));

    console.log('Done!');
    console.log(`Summary:`);
    console.log(`  - Total processes: ${output.processes.length}`);
    console.log(`  - Entries from defcons.json: ${Object.keys(defconsData).length}`);
    console.log(`  - Entries from defcons-override.json: ${Object.keys(defconsOverrideData).length}`);
    console.log(`  - Overridden entries: ${overrideCount}`);
    console.log(`  - Output file: pids-new.json`);

  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

mergeDefcons();
