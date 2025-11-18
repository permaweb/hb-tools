

# cli

```sh
npm run cli -- --action load --file server/data/old/allpids.json
npm run cli -- --action refresh-status 
npm run cli -- --action read
npm run cli -- --action hydrate
npm run cli -- --action summary
npm run cli -- --action read-repushes

npm run cli -- --action refresh-status --pids pid1,pid2
npm run cli -- --action read --pids pid1,pid2
npm run cli -- --action hydrate --pids pid1,pid2

npm run cli -- --action resolve-unpushed --txs tx1,tx2,tx3
```

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
            "url": "https://push-router.forward.computer",
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

curl "http://localhost:3001/api/processes?pids=wHA9yct1yxYFDCeI1PBJuWGnJKl3yk3QJib4Lf4qkU0"

curl http://localhost:3001/api/repushes

curl http://localhost:3001/api/summary

curl -X POST http://localhost:3001/api/clean-bad-procs

curl -s -X POST http://localhost:3001/api/rolling-hydration

curl -X POST http://localhost:3001/api/stop-rolling-hydration \
    -H "Content-Type: application/json" \
    -d "{\"operationId\": \"$OPERATION_ID\"}"

curl -X POST http://localhost:3001/api/resolve-unpushed \
    -H "Content-Type: application/json" \
    -d '{
      "txs": ["tx1", "tx2", "tx3"]
    }'
```