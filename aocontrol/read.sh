#!/bin/bash 

echo -e "\n1. GET /api/summary"
curl -s "$BASE_URL/api/summary" | jq

echo -e "\n2. GET /api/processes (limit 5)"
curl -s "$BASE_URL/api/processes?limit=5" | jq

echo -e "\n3. GET /api/repushes (limit 5)"
curl -s "$BASE_URL/api/repushes?limit=5" | jq