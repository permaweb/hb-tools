
# server read
```sh
curl "http://localhost:3001/api/processes?pids=wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"

curl "http://localhost:3001/api/processes?query=https://push.forward.computer"

curl "http://localhost:3001/api/processes?pids=wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0&query=https://push.forward.computer"

# Pagination - default limit is 100
curl "http://localhost:3001/api/processes"

# Pagination - get first 10 processes
# Response includes nextCursor if more results exist
curl "http://localhost:3001/api/processes?limit=10"

# Pagination - get next 10 processes using cursor from previous response
# If nextCursor is absent in response, you've reached the end
curl "http://localhost:3001/api/processes?limit=10&cursor=1234567890"

# Pagination with query filter
curl "http://localhost:3001/api/processes?query=https://push.forward.computer&limit=10"

curl http://localhost:3001/api/summary
```

# server functions
```sh
npm run dev:backend

curl -X POST http://localhost:3001/api/load \
    -H "Content-Type: application/json" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"],
      "hydrations": {
        "wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0": [
          {
            "url": "https://push.forward.computer",
            "status": "INIT"
          }
        ]
      }
    }'

curl -X POST http://localhost:3001/api/refresh-status \
    -H "Content-Type: application/json" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'

curl -X POST http://localhost:3001/api/hydrate \
    -H "Content-Type: application/json" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'

curl -X POST http://localhost:3001/api/cron \
    -H "Content-Type: application/json" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'

curl -X POST http://localhost:3001/api/clean-bad-procs

curl -X POST http://localhost:3001/api/resolve-unpushed \
      -H "Content-Type: application/json" \
      -d '{
        "txs": ["14AWd4r_Spgfgg83LlWQQ4pA3EC2pjLhRqz23Z6tv94"]
      }'

curl http://localhost:3001/api/repushes

curl -X POST http://localhost:3001/api/rolling-hydration

curl http://localhost:3001/api/operations
```