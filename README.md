# HyperBEAM Tools

A suite of integration, compatibility, and performance testing utilities for AO / HyperBEAM. These tools validate AO Connect API behavior, stress test message throughput, and benchmark process performance—helping ensure reliability, backwards compatibility, and scalability across HyperBEAM deployments.

## Modules

### ClientHB

AO client tools for integration and load testing. ClientHB provides a comprehensive test suite for validating AO network functionality, from basic compatibility checks to high-volume stress testing.

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
node run-group.js local_basic

# Individual component testing
node src/compatibility.js local_basic
node src/message-passing.js local_basic
```

## Monitoring and Metrics

The test scripts output various metrics that can be used for monitoring:

- **Success Rate**: Percentage of successful operations
- **Throughput**: Messages or operations per second
- **Latency**: Response time distributions (p50, p95, p99)
- **Error Types**: Classification of failures for debugging
