
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

# Repushes - default limit is 100
curl http://localhost:3001/api/repushes

# Search repushes by txid (messageId)
curl "http://localhost:3001/api/repushes?query=14AWd4r_Spgfgg83LlWQQ4pA3EC2pjLhRqz23Z6tv94"

# Pagination with repushes
curl "http://localhost:3001/api/repushes?limit=10"
curl "http://localhost:3001/api/repushes?limit=10&cursor=1234567890"
```

# server functions
```sh
npm run dev:backend

# Set your auth token (use ADMIN_TOKEN from .env or a user token)
TOKEN="your_auth_token_here"

curl -X POST http://localhost:3001/api/load \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
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
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'

curl -X POST http://localhost:3001/api/hydrate \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'

curl -X POST http://localhost:3001/api/cron \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "processes": ["wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"]
    }'

curl -X POST http://localhost:3001/api/clean-bad-procs \
    -H "Authorization: Bearer $TOKEN" \
    -d '{"processes": []}'

curl -X POST http://localhost:3005/api/resolve-unpushed \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{
      "txs": ["FBt9A5GA_KXMMSxA2DJ0xZbAq8sLLU2ak-YJe9zDvg8"]
    }'

curl -X POST http://localhost:3001/api/rolling-hydration \
    -H "Authorization: Bearer $TOKEN" \
    -d '{"processes": []}'

curl http://localhost:3001/api/operations
```