#!/bin/bash

echo -e "\n6. POST /api/resolve-unpushed"
curl -s -X POST "$BASE_URL/api/resolve-unpushed" \
  -H "Authorization: Bearer $AUTH_TOKEN" \
  -H "Content-Type: application/json" \
  -d "{
    \"txs\": [\"$REPUSH_ID\"]
  }" | jq
