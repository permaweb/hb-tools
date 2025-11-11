

# cli

```sh
npm run cli -- --action load --file server/data/old/allpids.json
npm run cli -- --action refresh-status 
npm run cli -- --action read
npm run cli -- --action hydrate
npm run cli -- --action summary

npm run cli -- --action refresh-status --pids pid1,pid2
npm run cli -- --action read --pids pid1,pid2
npm run cli -- --action hydrate --pids pid1,pid2
```