#!/usr/bin/env node

import { spawn } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const args = process.argv.slice(2);

if (args.length === 0) {
  console.error('Usage: node run-group.js <group> [additional-args...]');
  console.error('This will run all scripts defined for the group in config.json');
  console.error('Options:');
  console.error('  --stop-on-error  Stop execution when a script fails (default: continue)');
  process.exit(1);
}

const continueOnError = !args.includes('--stop-on-error');
const filteredArgs = args.filter(arg => arg !== '--stop-on-error');

const [group, ...additionalArgs] = filteredArgs;

const configPath = path.join(__dirname, 'config.json');
let config;

try {
  config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
} catch (error) {
  console.error(`Error reading config.json: ${error.message}`);
  process.exit(1);
}

if (!config[group]) {
  console.error(`Group '${group}' not found in config.json`);
  console.error(`Available groups: ${Object.keys(config).filter(k => k !== 'defaults').join(', ')}`);
  process.exit(1);
}

const groupConfig = { ...config.defaults, ...config[group] };

const scripts = groupConfig.scripts || [];

const srcDir = path.join(__dirname, 'src');

console.log(`Running group '${group}' with ${scripts.length} scripts`);
console.log(`Scripts: ${scripts.join(', ')}`);
console.log(`Arguments: ${group} ${additionalArgs.join(' ')}`);
if (!continueOnError) {
  console.log(`Mode: Stop on error enabled`);
}

function runScript(scriptName) {
  return new Promise((resolve, reject) => {
    const scriptPath = path.join(srcDir, scriptName);

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
      process.stdout.write(`${output}`);
    });

    child.stderr.on('data', (data) => {
      const output = data.toString();
      stderr += output;
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

async function runScripts() {
  const results = [];
  let totalTestsPassed = 0;
  let totalTestsFailed = 0;

  for (const scriptName of scripts) {
    try {
      console.log(`\nRunning ${scriptName}...`);
      const result = await runScript(scriptName);
      results.push({ status: 'fulfilled', value: result });

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

      const testResults = parseTestResults(error.stdout || '');
      if (testResults) {
        totalTestsPassed += testResults.passed;
        totalTestsFailed += testResults.failed;
        console.log(`\x1b[31m[FAILED]\x1b[0m ${scriptName} failed: exit code ${error.code} (${testResults.passed} passed, ${testResults.failed} failed)`);
      } else {
        console.log(`\x1b[31m[FAILED]\x1b[0m ${scriptName} failed: ${error.error ? error.error.message : `exit code ${error.code}`}`);
      }

      if (!continueOnError) {
        console.log('\n--- Execution stopped due to failure ---');
        break;
      }
    }
  }

  const successful = results.filter(r => r.status === 'fulfilled');
  const failed = results.filter(r => r.status === 'rejected');

  console.log(`Scripts: ${successful.length}/${scripts.length} successful`);

  const totalTests = totalTestsPassed + totalTestsFailed;
  if (totalTests > 0) {
    console.log(`Total Test Summary:`);
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

runScripts().catch(err => {
  console.error('Unexpected error:', err);
  process.exit(1);
});