


# server
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

curl "http://localhost:3001/api/processes?pids=wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"

curl "http://localhost:3001/api/processes?query=https://push.forward.computer"

curl "http://localhost:3001/api/processes?pids=wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0&query=https://push.forward.computer"

curl http://localhost:3001/api/summary

curl -X POST http://localhost:3001/api/clean-bad-procs

curl -X POST http://localhost:3001/api/resolve-unpushed \
      -H "Content-Type: application/json" \
      -d '{
        "txs": ["14AWd4r_Spgfgg83LlWQQ4pA3EC2pjLhRqz23Z6tv94"]
      }'

curl http://localhost:3001/api/repushes

curl -X POST http://localhost:3001/api/rolling-hydration

curl -X POST http://localhost:3001/api/stop-rolling-hydration \
      -H "Content-Type: application/json" \
      -d '{
        "operationId": "operation-id-from-rolling-hydration"
      }'

curl http://localhost:3001/api/operations
```