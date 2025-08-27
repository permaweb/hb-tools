# ClientHB

AO client tools for compatibility testing and volume/load testing.

## Scripts

### compatibility.js

Tests compatibility with AO legacy / mainnet by calling each function in aoconnect for each mode.

**Usage:**
```bash
node src/compatibility.js <group> [options]
```

**Options:**
- `--url <url>` - Override the AO node URL
- `--scheduler <address>` - Override the scheduler address  
- `--config <path>` - Use a different config file (default: config.json)

**Examples:**
```bash
# Basic usage with config group
node src/compatibility.js local_basic

# Override URL
node src/compatibility.js local_basic --url https://custom-ao-node.com

# Override scheduler address
node src/compatibility.js local_basic --scheduler mYJTM8VpIibDLuyGLQTcbcPy-LeOY48qzECADTUYfWc

# Use custom config file
node src/compatibility.js local_basic --config ./production-config.json

# Combine multiple overrides
node src/compatibility.js local_basic --url https://mainnet.ao.dev --scheduler XYZ789...abc --config ./staging-config.json
```

### volume.js

Performs volume/load testing by spawning multiple processes and sending messages using configurable worker threads, rate limiting, and concurrency controls.

**Usage:**
```bash
node src/volume.js <group> [options]
```

**Options:**
- `--spawns <count>` - Override total processes to spawn (default: 10)
- `--messages <count>` - Override total messages to send (default: 100)  
- `--workers <count>` - Override number of worker threads (default: 1)
- `--url <url>` - Override the AO node URL
- `--scheduler <address>` - Override the scheduler address
- `--config <path>` - Use a different config file (default: config.json)

**Examples:**
```bash
# Basic usage with config group
node src/volume.js local_basic

# Override spawn and message counts
node src/volume.js local_basic --spawns 50 --messages 500

# Use multiple workers for parallel execution
node src/volume.js local_basic --workers 4

# Full load test configuration
node src/volume.js production --spawns 100 --messages 1000 --workers 8 --config ./prod-config.json

# Override network configuration
node src/volume.js local_basic --url https://mainnet.ao.dev --scheduler XYZ789...abc
```

## Configuration

Both scripts require a configuration file (default: `config.json`) with group definitions:

```json
{
  "local_basic": {
    "url": "http://localhost:8734",
    "schedulerAddress": "mYJTM8VpIibDLuyGLQTcbcPy-LeOY48qzECADTUYfWc",
    "wallet": {
      "kty": "RSA",
      "n": "...",
      "e": "AQAB",
      "d": "...",
      "p": "...",
      "q": "...",
      "dp": "...",
      "dq": "...",
      "qi": "..."
    }
  }
}
```

Each group must include:
- `url` - AO network node URL
- `schedulerAddress` - Scheduler process address
- `wallet` - RSA private key in JWK format for signing transactions

## Volume Test Configuration

The volume test includes additional hardcoded configuration that can be modified in the script:

- **Rate Limits:** `RATE_SPAWNS_PER_SEC` (20/s) and `RATE_MSGS_PER_SEC` (100/s)
- **Jitter:** `JITTER_MS` (±50ms) for randomizing operation timing
- **Concurrency:** `CONCURRENCY_SPAWN_PER_WORKER` (1) and `CONCURRENCY_MSG_PER_WORKER` (10)

These settings control the load characteristics and can be adjusted for different testing scenarios.