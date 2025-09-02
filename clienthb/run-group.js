#!/usr/bin/env node

import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Parse command line arguments
const args = process.argv.slice(2);

if (args.length === 0) {
  console.error('Usage: node run-group.js <group> [additional-args...]');
  console.error('This will run all scripts defined for the group in config.json');
  process.exit(1);
}

const [group, ...additionalArgs] = args;

// Read config.json
const configPath = path.join(__dirname, 'config.json');
let config;

try {
  config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
} catch (error) {
  console.error(`Error reading config.json: ${error.message}`);
  process.exit(1);
}

// Check if group exists in config
if (!config[group]) {
  console.error(`Group "${group}" not found in config.json`);
  console.error(`Available groups: ${Object.keys(config).join(', ')}`);
  process.exit(1);
}

// Get scripts for the group
const groupConfig = config[group];
if (!groupConfig.scripts || !Array.isArray(groupConfig.scripts)) {
  console.error(`Group "${group}" does not have a "scripts" array in config.json`);
  process.exit(1);
}

const scripts = groupConfig.scripts;
const srcDir = path.join(__dirname, 'src');

console.log(`Running group "${group}" with ${scripts.length} scripts`);
console.log(`Scripts: ${scripts.join(', ')}`);
console.log(`Arguments: ${group} ${additionalArgs.join(' ')}`);
console.log('---');

// Function to run a single script
function runScript(scriptName) {
  return new Promise((resolve, reject) => {
    const scriptPath = path.join(srcDir, scriptName);
    
    // Check if script exists
    if (!fs.existsSync(scriptPath)) {
      reject({ script: scriptName, error: new Error(`Script not found: ${scriptPath}`) });
      return;
    }

    const child = spawn('node', [scriptPath, group, ...additionalArgs], {
      stdio: 'pipe'
    });

    let stdout = '';
    let stderr = '';

    child.stdout.on('data', (data) => {
      const output = data.toString();
      stdout += output;
      // Prefix output with script name
      process.stdout.write(`[${scriptName}] ${output}`);
    });

    child.stderr.on('data', (data) => {
      const output = data.toString();
      stderr += output;
      // Prefix error output with script name
      process.stderr.write(`[${scriptName}] ${output}`);
    });

    child.on('close', (code) => {
      if (code === 0) {
        resolve({ script: scriptName, code, stdout, stderr });
      } else {
        reject({ script: scriptName, code, stdout, stderr });
      }
    });

    child.on('error', (err) => {
      reject({ script: scriptName, error: err });
    });
  });
}

// Function to parse test results from output
function parseTestResults(stdout) {
  const match = stdout.match(/TEST_RESULTS: passed=(\d+) failed=(\d+) total=(\d+)/);
  if (match) {
    return {
      passed: parseInt(match[1]),
      failed: parseInt(match[2]),
      total: parseInt(match[3])
    };
  }
  return null;
}

// Run all scripts sequentially
async function runScriptsSequentially() {
  const results = [];
  let totalTestsPassed = 0;
  let totalTestsFailed = 0;
  
  for (const scriptName of scripts) {
    try {
      console.log(`\nRunning ${scriptName}...`);
      const result = await runScript(scriptName);
      results.push({ status: 'fulfilled', value: result });
      
      // Parse test results if available
      const testResults = parseTestResults(result.stdout);
      if (testResults) {
        totalTestsPassed += testResults.passed;
        totalTestsFailed += testResults.failed;
        console.log(`\x1b[32m[SUCCESS]\x1b[0m ${scriptName} completed successfully (${testResults.passed} passed, ${testResults.failed} failed)`);
      } else {
        console.log(`\x1b[32m[SUCCESS]\x1b[0m ${scriptName} completed successfully`);
      }
    } catch (error) {
      results.push({ status: 'rejected', reason: error });
      console.log(`\x1b[31m[FAILED]\x1b[0m ${scriptName} failed: ${error.error ? error.error.message : `exit code ${error.code}`}`);
      
      // Stop execution on first failure
      console.log('\n--- Execution stopped due to failure ---');
      break;
    }
  }
  
  console.log('\n--- Summary ---');
  
  const successful = results.filter(r => r.status === 'fulfilled');
  const failed = results.filter(r => r.status === 'rejected');
  
  console.log(`Scripts: ${successful.length}/${scripts.length} successful`);
  
  const totalTests = totalTestsPassed + totalTestsFailed;
  if (totalTests > 0) {
    console.log(`\nTotal Test Summary:`);
    console.log(`   \x1b[32mPassed\x1b[0m: ${totalTestsPassed}`);
    console.log(`   \x1b[${totalTestsFailed > 0 ? '31' : '90'}mFailed\x1b[0m: ${totalTestsFailed}`);
    console.log(`   Total: ${totalTests}`);
  }
  
  if (successful.length > 0) {
    console.log('\x1b[32mCompleted scripts:\x1b[0m');
    successful.forEach(result => {
      console.log(`  - ${result.value.script}`);
    });
  }
  
  if (failed.length > 0) {
    console.log('\x1b[31mFailed scripts:\x1b[0m');
    failed.forEach(result => {
      const reason = result.reason;
      console.log(`  - ${reason.script}: ${reason.error ? reason.error.message : `exit code ${reason.code}`}`);
    });
  }
  
  process.exit(failed.length > 0 ? 1 : 0);
}

runScriptsSequentially().catch(err => {
  console.error('Unexpected error:', err);
  process.exit(1);
});