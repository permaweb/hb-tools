# AOControl API

API for managing AO process hydrations and repushes.

## Setup

```sh
# Install dependencies
npm install

# Set environment variables in .env
DATABASE_PATH=postgresql://user:pass@localhost:5432/dbname
ADMIN_TOKEN=your-admin-token
USER_PERMISSIONS='{"user-token-1":["process-id-1","process-id-2"]}'

# Start the server
npm run start

# Or run in development mode
npm run dev:backend
```

## Using Test Scripts

Quick test scripts are provided for common operations:

```sh
# Set your environment variables
export AUTH_TOKEN="your-auth-token"
export BASE_URL="http://localhost:3001"

# Read processes and summary
./read.sh

# Hydrate specific processes
export PROCESS_ID="wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"
./hydrate.sh

# Resolve unpushed transactions
export TEST_ID="your-message-id"
./repush.sh

# Run full API test suite
./test.sh
```

## API Endpoints

### Read Operations (No Auth Required)

#### Get Summary
```sh
curl http://localhost:3001/api/summary
```

#### Get Processes
```sh
# Get specific process by ID
curl "http://localhost:3001/api/processes?pids=wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"

# Search by hydration URL
curl "http://localhost:3001/api/processes?query=https://push.forward.computer"

# Combine filters
curl "http://localhost:3001/api/processes?pids=wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0&query=https://push.forward.computer"

# Pagination - default limit is 100
curl "http://localhost:3001/api/processes?limit=10"

# Use cursor for next page (cursor is timestamp from previous response)
curl "http://localhost:3001/api/processes?limit=10&cursor=1734567890"
```

#### Get Repushes
```sh
# Get all repushes (default limit 100)
curl http://localhost:3001/api/repushes

# Search by message ID
curl "http://localhost:3001/api/repushes?query=14AWd4r_Spgfgg83LlWQQ4pA3EC2pjLhRqz23Z6tv94"

# Pagination
curl "http://localhost:3001/api/repushes?limit=10"
curl "http://localhost:3001/api/repushes?limit=10&cursor=1734567890"
```

#### Get Active Operations
```sh
curl http://localhost:3001/api/operations
```

### Write Operations (Auth Required)

Set your auth token:
```sh
TOKEN="your_auth_token_here"
```

#### Load Processes
Load processes and hydrations into the database:
```sh
curl -X POST http://localhost:3001/api/load \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"],
      "hydrations": [
        {
          "processId": "wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0",
          "url": "https://push.forward.computer",
          "status": "INIT"
        }
      ]
    }'
```

#### Queue Hydrations
Queue hydrations for the worker to process:
```sh
curl -X POST http://localhost:3001/api/queue-hydrations \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"],
      "hydrations": [
        {
          "processId": "wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0",
          "url": "https://push.forward.computer",
          "status": "INIT"
        }
      ]
    }'
```

#### Start/Stop Worker Hydrations
Control the background worker hydration processing:
```sh
# Start processing INIT hydrations
curl -X POST http://localhost:3001/api/start-hydrations \
    -H "Authorization: Bearer $TOKEN"

# Stop processing
curl -X POST http://localhost:3001/api/stop-hydrations \
    -H "Authorization: Bearer $TOKEN"
```

#### Hydrate Processes
Manually trigger hydration for specific processes:
```sh
curl -X POST http://localhost:3001/api/hydrate \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'
```

#### Refresh Status
Refresh the status of process hydrations:
```sh
curl -X POST http://localhost:3001/api/refresh-status \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'
```

#### Run Cron
Run cron tasks for processes:
```sh
curl -X POST http://localhost:3001/api/cron \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'
```

#### Resolve Unpushed
Resolve unpushed messages for processes:
```sh
curl -X POST http://localhost:3001/api/resolve-unpushed \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "txs": ["IURFsLgLv9lan6LLBUMAOeJbLT25uLSxnS3OY7dWs_0"],
      "processes": ["pid"]
    }'
```

#### Clean Bad Processes
Remove processes that have bad/invalid hydrations:
```sh
curl -X POST http://localhost:3001/api/clean-bad-procs \
    -H "Authorization: Bearer $TOKEN"
```

## Worker Thread

The server runs a background worker thread that continuously processes hydrations with status `INIT`:

- Queries the database for batches of 200 INIT hydrations
- Processes them with MAX_CONCURRENT = 100 (configurable via `MAX_CONCURRENT_HYDRATIONS` env var)
- Automatically picks up new hydrations within seconds
- Can be started/stopped via the `/api/start-hydrations` and `/api/stop-hydrations` endpoints

## Authentication

- Admin tokens have access to all processes
- User tokens have access only to processes listed in their permissions
- Set `ADMIN_TOKEN` in your environment for admin access
- Set `USER_PERMISSIONS` as a JSON object mapping tokens to process ID arrays

Example:
```json
{
  "user-token-abc": ["process-1", "process-2"],
  "user-token-xyz": ["process-3"]
}
```
