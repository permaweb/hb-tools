# HyperBEAM Tools

A suite of integration, compatibility, and performance testing utilities for AO / HyperBEAM. These tools validate AO Connect API behavior, stress test message throughput, and benchmark process performance—helping ensure reliability, backwards compatibility, and scalability across HyperBEAM deployments.

## ClientHB

Compatibility / Load Tests built in JavaScript.

#### Key Features

- **Core Library Validation**: Direct testing of `@permaweb/ao-core-libs` functionality
- **Compatibility Testing**: Validate AO node implementations against mainnet specifications
- **Load Testing**: Generate configurable message volumes to stress test infrastructure
- **Performance Benchmarking**: Measure message throughput and data handling capabilities
- **Inter-Process Communication**: Test message passing between AO processes

#### Configuration

**All scripts require a configuration file (default: `config.json`) with group definitions:**

```json
{
  "local_basic": {
    "url": "http://localhost:8734",
    "schedulerAddress": "NoZH3pueH0Cih6zjSNu_KRAcmg4ZJV1aGHKi0Pi5_Hc",
    "scripts": [
      "core-libs.js",
      "compatibility.js",
      "message-passing.js",
      "patch.js",
      "volume.js"
    ]
  }
}
```

Each group must include:

- `url` - AO network node URL (e.g., `http://localhost:8734` for local, `https://mainnet.ao.dev` for production)
- `schedulerAddress` - Scheduler process address for handling message ordering
- `scripts` - Array of script filenames to run with `run-group.js` (optional)

#### Required Environment Variables

- `PATH_TO_WALLET`: JWK Interface of the Arweave Wallet the tests will use

#### Available Test Scripts

| Script               | Description                              |
| -------------------- | ---------------------------------------- |
| `core-libs.js`       | Tests direct library integration         |
| `compatibility.js`   | Validates AO Connet API compatibility    |
| `message-passing.js` | Tests inter-process communication        |
| `patch.js`           | Benchmarks data handling performance     |
| `volume.js`          | Load tests with configurable concurrency |

#### Adding New Test Scripts

To add a new test script:

1. Create the script in `clienthb/src/`
2. Follow the existing pattern for configuration handling
3. Add the script to relevant groups in `config.json`
4. Document usage in this README

#### Testing Scenarios

###### Local Development

```bash
# Quick validation
npm run group local_basic

# Individual component testing
node src/compatibility.js local_basic
node src/message-passing.js local_basic
```

#### Monitoring and Metrics

The test scripts output various metrics that can be used for monitoring:

- **Success Rate**: Percentage of successful operations
- **Throughput**: Messages or operations per second
- **Latency**: Response time distributions (p50, p95, p99)
- **Error Types**: Classification of failures for debugging

#### Performance Tuning

###### Volume Test Parameters

When running volume tests (`volume.js`), you can fine-tune performance by adjusting these parameters in the script:

| Parameter                      | Default | Description                                               |
| ------------------------------ | ------- | --------------------------------------------------------- |
| `RATE_SPAWNS_PER_SEC`          | 20      | Maximum process spawns per second                         |
| `RATE_MSGS_PER_SEC`            | 100     | Maximum messages sent per second                          |
| `JITTER_MS`                    | ±50ms   | Random timing variation to simulate real-world conditions |
| `CONCURRENCY_SPAWN_PER_WORKER` | 1       | Concurrent spawns per worker thread                       |
| `CONCURRENCY_MSG_PER_WORKER`   | 10      | Concurrent messages per worker thread                     |
