---
name: esql
description: Query Elasticsearch/Kibana using ES|QL via the Kibana async search API. Requires an initial curl command from the user to extract session credentials. Triggers: /esql, "query elastic", "search logs", "check elastic logs", "run esql", "elasticsearch query", "check kibana"
user_invocable: true
---

# ES|QL Query Skill

Query Elasticsearch via the Kibana ES|QL async search API. Run ES|QL queries directly against Kibana clusters without the user needing to copy-paste results.

## Setup

On first use in a conversation, ask the user to provide a curl command from Kibana's network tab (browser DevTools). Extract these from the curl:

1. **Kibana URL** — the base URL (e.g., `https://<region>.azure.elastic-cloud.com`)
2. **Cookie** — the `sid=...` session cookie
3. **kbn-version** — the Kibana version header
4. **kbn-build-number** — the Kibana build number header

Store these in shell variables for the session:

```bash
export KIBANA_URL="https://..."
export KIBANA_COOKIE="sid=..."
export KBN_VERSION="8.x.x"
export KBN_BUILD_NUMBER="12345"
```

The cookie is a session token and will expire — if you get 401s, ask the user for a fresh curl.

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

### Handling async responses

The endpoint may return results immediately or require polling:

- **Immediate**: Response contains `columns`, `values`, and `"is_running": false` — parse directly.
- **Still running**: Response contains `"is_running": true` and an `id` but no `columns`/`values` — you must poll.

**Polling**: Re-POST to the same endpoint with the returned `id` appended as a query parameter, or use the Elasticsearch async get API. In practice, poll by re-requesting with a short delay:

```bash
# If is_running is true, wait and poll
QUERY_ID="<id from response>"
sleep 2
curl -s "${KIBANA_URL}/internal/search/esql_async/${QUERY_ID}" \
  --compressed \
  -H "kbn-version: ${KBN_VERSION}" \
  -H "kbn-build-number: ${KBN_BUILD_NUMBER}" \
  -H 'elastic-api-version: 1' \
  -H 'x-elastic-internal-origin: Kibana' \
  -H "Cookie: ${KIBANA_COOKIE}"
```

Repeat until `is_running` is `false` (max ~5 attempts with increasing delay).

### Parsing results

Pipe curl output to python3 for readable tables. This script handles both immediate and still-running responses:

```bash
| python3 -c "
import sys, json
d = json.load(sys.stdin)
if d.get('is_running'):
    print(f'Query still running. ID: {d.get(\"id\", \"unknown\")}')
    sys.exit(1)
if 'statusCode' in d:
    print(f'Error {d[\"statusCode\"]}: {d.get(\"message\", \"unknown error\")}')
    sys.exit(1)
cols = [c['name'] for c in d.get('columns', [])]
vals = d.get('values', [])
found = d.get('documents_found', len(vals))
print(f'Found: {found} docs, showing {len(vals)} rows')
if not cols:
    print('No columns returned')
    sys.exit(0)
widths = [max(len(c), max((len(str(v[i])[:80]) for v in vals), default=0)) for i, c in enumerate(cols)]
header = ' | '.join(c.ljust(w) for c, w in zip(cols, widths))
print(header)
print('-' * len(header))
for v in vals:
    print(' | '.join(str(x if x is not None else '')[:80].ljust(w) for x, w in zip(v, widths)))
"
```

## Known service names

These are the `service.name` values for control-plane-backend services:

| Service | `service.name` |
|---|---|
| Workload Tenants | `workload-tenants-svc` |
| Subscriptions | `subscriptions-svc` |
| User Management | `user-management` |
| Routing | `routing` |
| EARN | `earn` |
| EARN Telemetry | `earn-telemetry` |
| Usage Reports | `usage_reports` |
| Onboarding | `onboarding` |
| Scheduler | `scheduler` |

The nginx ingress controller logs under `service.name == "nginx"`.

## Common log fields

### Standard fields
- `@timestamp` — event timestamp (ISO 8601)
- `service.name` — service identifier (see table above)
- `service.environment` — `prod`, `prd`, `qa`, `dev`
- `message` — log message text
- `level` — log level (`info`, `warn`, `error`)
- `trace.id` — distributed trace ID for correlating across services

### Label fields (keyword/string type)
- `labels.status` — HTTP status code (string, not int)
- `labels.path` — request path
- `labels.method` — HTTP method
- `labels.error` / `labels.error_message` — error details
- `labels.organization_id` / `labels.org_id` — organization identifier
- `labels.organization.id` — alternative org ID field
- `labels.workload_tenant.id` — workload tenant identifier
- `labels.user.id` / `labels.principal.id` — user/principal identifiers
- `labels.operation` — operation being performed
- `labels.handler` — handler name
- `labels.earn.event.type` — EARN event type
- `labels.earn.event.dedupe_key` — EARN dedup key
- `labels.scheduler.event.id` — scheduler event ID
- `labels.request.url` — outgoing request URL
- `labels.http.router.request_id` — request ID

### Numeric label fields
- `numeric_labels.*` — numeric values (latency, counts, etc.)

### Overview labels (for filtering structured log categories)
These are set via `clog.Label()` and appear in a labels field:
- `http_server_overview`, `dispatcher_overview`, `queue_dispatcher_overview`
- `earn_event_delivery`, `events_processor`
- `panic_recovery`, `panic_recovered`
- `azure_sdk_logging`, `secrets_handling`
- `deprecated_behavior_overview`

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

### Find errors for a specific service
```
FROM logs-*
| WHERE service.name == "earn"
  AND service.environment == "prod"
  AND level == "error"
  AND @timestamp >= "START"
  AND @timestamp <= "END"
| SORT @timestamp DESC
| LIMIT 30
| KEEP @timestamp, message, labels.error, labels.error_message, labels.operation, trace.id
```

### Search by org/tenant ID across all services
```
FROM logs-*
| WHERE service.environment == "prod"
  AND @timestamp >= "START"
  AND @timestamp <= "END"
  AND (labels.organization_id == "ORG_ID" OR labels.org_id == "ORG_ID" OR labels.organization.id == "ORG_ID")
| SORT @timestamp DESC
| LIMIT 20
| KEEP @timestamp, service.name, message, labels.error, trace.id
```

### Search by workload tenant ID
```
FROM logs-*
| WHERE service.environment == "prod"
  AND @timestamp >= "START"
  AND @timestamp <= "END"
  AND labels.workload_tenant.id == "TENANT_ID"
| SORT @timestamp DESC
| LIMIT 30
| KEEP @timestamp, service.name, message, labels.error, labels.operation
```

### Trace a request across services
```
FROM logs-*
| WHERE trace.id == "TRACE_ID"
| SORT @timestamp ASC
| LIMIT 100
| KEEP @timestamp, service.name, level, message, labels.error, labels.status, labels.path
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

### Error rate by service (last hour)
```
FROM logs-*
| WHERE service.environment == "prod"
  AND @timestamp >= "START"
  AND @timestamp <= "END"
  AND level == "error"
| STATS error_count = COUNT(*) BY service.name
| SORT error_count DESC
| LIMIT 20
```

### Find panics
```
FROM logs-*
| WHERE service.environment == "prod"
  AND @timestamp >= "START"
  AND @timestamp <= "END"
  AND (message LIKE "*panic*" OR labels.panic_recovered == "true" OR labels.panic_recovery == "true")
| SORT @timestamp DESC
| LIMIT 20
| KEEP @timestamp, service.name, message, labels.error, trace.id
```

## Dev / Personal Environments — `,es` CLI

The `,es` CLI at `/Users/meain/.local/bin/utils/,es` provides a simpler way to query Elasticsearch, but it **only works for the dev environment**. Do not use it for staging or production.

### Common queries

Search by trace ID:
```
,es -q 'trace.id:<TRACE_ID>' -n 50
```

Filter for errors:
```
,es -q 'trace.id:<TRACE_ID> AND event.type:error' -n 10
```

Broader service-level search (no trace ID):
```
,es -q 'service.name:<SERVICE_NAME> AND (level:error OR event.type:error)' -n 20
```

### `,es` CLI options

- `-q` — Lucene query string
- `-n` — Number of results
- `-f` — Comma-separated fields to display
- `-s` — Sort field (e.g. `@timestamp:desc`)

## Choosing the right approach

| Environment | Method |
|---|---|
| Dev / personal | Use `,es` CLI directly |
| Staging / production | Use the Kibana ES\|QL curl approach (requires session cookie from user) |
| Unknown / `,es` fails | Fall back to clipboard mode (see below) |

When falling back to clipboard mode (no session cookie available):
1. Copy the ES|QL query to the user's clipboard with `pbcopy`
2. Tell the user to open Kibana for the appropriate environment
3. Open browser DevTools Network tab, filter for `esql_async`
4. Paste and run the ESQL query in Discover (ESQL mode)
5. In the Network tab, find the `esql_async` request, copy the response JSON
6. Paste the response back into the conversation

## Investigation workflow tips

- **Start broad, narrow down.** First identify which services are involved, then drill into specific errors.
- **nginx → upstream.** nginx logs show external-facing status codes; upstream service logs show root cause.
- **Use trace.id** to correlate requests across services when available.
- **Error details** live in `labels.error` and `labels.error_message`.
- **Field types matter:** `numeric_labels.*` fields are numeric; `labels.*` fields are keywords (strings).
- **Time windows:** Always scope queries with `@timestamp` ranges to avoid scanning too much data. Start with 1-hour windows and expand if needed.
- **Overview labels** (like `http_server_overview`, `dispatcher_overview`) are useful for filtering to structured log categories without needing to match on message text.
