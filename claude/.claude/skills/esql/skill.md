---
name: esql
description: Query Elasticsearch/Kibana using ES|QL via the Kibana async search API. Requires an initial curl command from the user to extract session credentials. Triggers: /esql, "query elastic", "search logs", "check elastic logs", "run esql", "elasticsearch query"
user_invocable: true
---

# ES|QL Query Skill

Query Elasticsearch via the Kibana ES|QL async search API. This skill lets you run ES|QL queries directly against Kibana clusters without the user needing to copy-paste results.

## Setup

On first use in a conversation, ask the user to provide a curl command from Kibana's network tab (browser DevTools). Extract these from the curl:

1. **Kibana URL** — the base URL (e.g., `https://<region>.azure.elastic-cloud.com`)
2. **Cookie** — the `sid=...` session cookie
3. **kbn-version** — the Kibana version header
4. **kbn-build-number** — the Kibana build number header

Store these in shell variables for the session. The cookie is a session token and will expire — if you get 401s, ask the user for a fresh curl.

## How to query

Use curl to POST to the Kibana async ES|QL endpoint:

```bash
curl -s "${KIBANA_URL}/internal/search/esql_async" \
  --compressed \
  -X POST \
  -H 'Content-Type: application/json' \
  -H "kbn-version: ${KBN_VERSION}" \
  -H "kbn-build-number: ${KBN_BUILD_NUMBER}" \
  -H 'elastic-api-version: 1' \
  -H 'x-elastic-internal-origin: Kibana' \
  -H "Cookie: ${KIBANA_COOKIE}" \
  --data-raw '{"params":{"query":"YOUR_ESQL_QUERY_HERE","locale":"en","include_execution_metadata":true,"filter":{"bool":{"must":[],"filter":[],"should":[],"must_not":[]}},"dropNullColumns":true},"isSearchStored":false,"stream":true}'
```

### Important notes

- ES|QL query must have newlines escaped as `\n` and quotes escaped as `\"` in the JSON payload
- `labels.status` is a **keyword** field — compare with strings, not integers (e.g., `labels.status >= "500"`)
- `service.environment` values vary by cluster — common values: `prod`, `prd`, `qa`, `dev`
- Use `dropNullColumns: true` to keep response compact
- Parse the JSON response to extract `columns` and `values` arrays

### Parsing results

Use python3 to format results into readable tables:

```bash
| python3 -c "
import sys,json
d=json.load(sys.stdin)
cols = [c['name'] for c in d.get('columns',[])]
print(f'Found: {d[\"documents_found\"]} docs')
if cols: print(' | '.join(cols))
print('-'*80)
for v in d.get('values',[]):
    print(' | '.join(str(x)[:100] for x in v))
"
```

## Common query patterns

### List services in a time window
```
FROM logs-*
| WHERE service.environment == "prod"
  AND @timestamp >= "2026-04-07T07:00:00.000Z"
  AND @timestamp <= "2026-04-07T08:00:00.000Z"
| STATS count = COUNT(*) BY service.name
| SORT count DESC
| LIMIT 30
```

### Find 5xx errors from nginx
```
FROM logs-*
| WHERE service.name == "nginx"
  AND service.environment == "prod"
  AND labels.status >= "500"
| SORT @timestamp DESC
| LIMIT 20
| KEEP @timestamp, labels.status, labels.path, labels.method, message
```

### Search by org/tenant ID across all services
```
FROM logs-*
| WHERE service.environment == "prod"
  AND @timestamp >= "START"
  AND @timestamp <= "END"
  AND (labels.organization_id == "ORG_ID" OR labels.org_id == "ORG_ID")
| SORT @timestamp DESC
| LIMIT 20
| KEEP @timestamp, service.name, message, labels.error
```

### Check latency metrics
```
FROM logs-*
| WHERE service.name == "SERVICE"
  AND service.environment == "prod"
  AND message LIKE "*metric*latency*"
| SORT @timestamp DESC
| LIMIT 30
| KEEP @timestamp, message, numeric_labels.value_ms
```

## Tips

- Start broad, narrow down. First identify which services are involved, then drill into specific errors.
- nginx logs show the external-facing status codes; upstream service logs show root cause.
- `labels.error` and `labels.error_message` fields often contain the actual error details.
- Use `trace.id` to correlate requests across services when available.
- `numeric_labels.*` fields are numeric; `labels.*` fields are keywords (strings).
