#!/bin/bash

echo -e "\n5. POST /api/queue-hydrations"
curl -s -X POST "$BASE_URL/api/queue-hydrations" \
  -H "Authorization: Bearer $AUTH_TOKEN" \
  -H "Content-Type: application/json" \
  -d "{
    \"processes\": [\"$HYDRATE_ID\"],
    \"hydrations\": [
      {\"processId\": \"$HYDRATE_ID\", \"url\": \"$HYDRATE_URL\", \"status\": \"INIT\"}
    ]
  }" | jq

echo -e "\n7. POST /api/start-hydrations"
curl -s -X POST "$BASE_URL/api/start-hydrations" \
  -H "Authorization: Bearer $AUTH_TOKEN" \
  -H "Content-Type: application/json" | jq

echo -e "\n8. POST /api/stop-hydrations"
curl -s -X POST "$BASE_URL/api/stop-hydrations" \
  -H "Authorization: Bearer $AUTH_TOKEN" \
  -H "Content-Type: application/json" | jq