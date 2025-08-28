# ClientHB

AO client tools for integration and load testing.

## Scripts

---

## Group Runner

### run-group.js

Executes multiple test scripts in parallel for a given configuration group. This utility script reads the `scripts` array from a group's configuration and runs all specified scripts simultaneously, providing consolidated output and status reporting.

**Usage:**
```bash
node run-group.js <group> [additional-args...]
```

**Arguments:**
- `<group>` - The configuration group to run (must exist in config.json)
- `[additional-args...]` - Additional arguments passed to each script

**Examples:**
```bash
# Run all scripts defined for local_basic group
node run-group.js local_basic

# Run with additional arguments passed to all scripts
node run-group.js local_basic --url https://custom-node.com

# Run production group with custom scheduler
node run-group.js production --scheduler ABC123...def
```

## Configuration

All scripts require a configuration file (default: `config.json`) with group definitions:

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
    },
    "scripts": [
      "compatibility.js",
      "core-libs.js",
      "message-passing.js",
      "patch.js"
    ]
  }
}
```

Each group must include:
- `url` - AO network node URL
- `schedulerAddress` - Scheduler process address
- `wallet` - RSA private key in JWK format for signing transactions
- `scripts` - Array of script filenames to run with `run-group.js` (optional)

---

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

---

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

### Volume Test Configuration

The volume test includes additional hardcoded configuration that can be modified in the script:

- **Rate Limits:** `RATE_SPAWNS_PER_SEC` (20/s) and `RATE_MSGS_PER_SEC` (100/s)
- **Jitter:** `JITTER_MS` (±50ms) for randomizing operation timing
- **Concurrency:** `CONCURRENCY_SPAWN_PER_WORKER` (1) and `CONCURRENCY_MSG_PER_WORKER` (10)

These settings control the load characteristics and can be adjusted for different testing scenarios.

---

### core-libs.js

Tests the AO flow using `@permaweb/ao-core-libs` directly instead of aoconnect. This script performs a complete AO process lifecycle: spawning a process, sending initialization pushes, and sending messages to verify the core libraries work correctly.

**Usage:**
```bash
node src/core-libs.js <group> [options]
```

**Options:**
- `--url <url>` - Override the AO node URL
- `--scheduler <address>` - Override the scheduler address
- `--config <path>` - Use a different config file (default: config.json)

**Examples:**
```bash
# Basic usage with config group
node src/core-libs.js local_basic

# Override URL and scheduler
node src/core-libs.js local_basic --url https://custom-ao-node.com --scheduler ABC123...def

# Use custom config file
node src/core-libs.js local_basic --config ./custom-config.json
```

**What it does:**
- Spawns a new AO process using ao-core-libs
- Performs initialization push with retry logic (up to 10 attempts)
- Sends an "Info" action message to the process
- Reads the computation result to verify the flow worked

---

### message-passing.js

Tests inter-process message passing between multiple AO processes. This script verifies that processes can communicate with each other by spawning three processes and setting up handlers for message routing.

**Usage:**
```bash
node src/message-passing.js <group> [options]
```

**Options:**
- `--url <url>` - Override the AO node URL
- `--scheduler <address>` - Override the scheduler address
- `--config <path>` - Use a different config file (default: config.json)

**Examples:**
```bash
# Basic usage with config group
node src/message-passing.js local_basic

# Override network configuration
node src/message-passing.js local_basic --url https://mainnet.ao.dev --scheduler XYZ789...abc

# Use custom config file
node src/message-passing.js local_basic --config ./staging-config.json
```

**What it does:**
- Spawns three separate AO processes
- Sets up message handlers on process 2 to forward messages to process 3
- Sets up a "Pong" handler on process 3 to log received messages
- Process 1 sends a message to process 2, which forwards it to process 3
- Verifies that process 3 received the expected "pong" message

---

### patch.js

Tests AO process performance with different data sizes by creating processes that store and retrieve large JSON indexes. This script measures message send times and zone data fetch performance across various payload sizes.

**Usage:**
```bash
node src/patch.js <group> [options]
```

**Options:**
- `--url <url>` - Override the AO node URL
- `--scheduler <address>` - Override the scheduler address  
- `--config <path>` - Use a different config file (default: config.json)

**Examples:**
```bash
# Basic usage with config group
node src/patch.js local_basic

# Override URL and scheduler
node src/patch.js local_basic --url https://custom-ao-node.com --scheduler ABC123...def

# Use custom config file
node src/patch.js production --config ./prod-config.json
```

**What it does:**
- Tests with index sizes: 1,000, 5,000, 10,000, 25,000, and 63,500 records
- For each size, spawns a process and sets up JSON index handling
- Sends large JSON payloads and measures message send performance
- Fetches the stored data via zone endpoint and measures fetch performance
- Reports timing and data size metrics for performance analysis



