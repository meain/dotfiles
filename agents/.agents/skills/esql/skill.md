---
name: esql
description: Query Elasticsearch/Kibana using ES|QL via the Kibana async search API. Requires an initial curl command from the user to extract session credentials. Triggers: /esql, "query elastic", "search logs", "check elastic logs", "run esql", "elasticsearch query", "check kibana"
user_invocable: true
---

# ES|QL Query Skill

Query Elasticsearch via the Kibana ES|QL async search API. Run ES|QL queries directly against Kibana clusters without the user needing to copy-paste results.

## Setup

### Staging / Production — `,es-web` CLI

The `,es-web` CLI at `/Users/meain/.local/bin/utils/,es-web` handles Kibana ES|QL queries with automatic async polling, session management, and table output.

**First-time setup** for a cluster: the user provides a curl command from Kibana's network tab:
```bash
pbpaste | ,es-web init stg-us
pbpaste | ,es-web init prd-eu
```

**Check configured clusters:**
```bash
,es-web list
```

Session cookies expire after a few hours. If you get a 401/session expired error:
1. Look up the Kibana URL for the target cluster from the Confluence Speed Dial page (`confluence page 405504500`)
2. Open it with `open <url>` (with `dangerouslyDisableSandbox: true`)
3. Ask the user to copy a curl from the network tab and run `! pbpaste | ,es-web init <cluster>`

## How to query with `,es-web`

```bash
# Simple: service + time window
,es-web -c stg-us -s earn -t 1h -n 20

# With additional WHERE clause
,es-web -c stg-us -s earn -t 1h -q 'message LIKE "*validation*"'

# Custom fields
,es-web -c stg-us -s earn -t 1h -f '@timestamp, message, labels.error'

# Raw ES|QL query
,es-web -c stg-us 'FROM logs-* | WHERE service.name == "earn" | STATS count = COUNT(*) BY labels.event_type | SORT count DESC | LIMIT 10'

# JSON output for further processing
,es-web -c stg-us -s earn -t 30m --json

# Debug: print generated query
,es-web -c stg-us -s earn -t 1h --debug
```

### `,es-web` options

- `-c` — Cluster alias (e.g. `stg-us`, `prd-eu`)
- `-s` — `service.name` filter
- `-t` — Time window (e.g. `30m`, `1h`, `2d`). Defaults to `4h` if omitted (a warning is printed to stderr)
- `-n` — Max rows (default: 20)
- `-q` — Additional WHERE clause
- `-f` — Comma-separated fields for KEEP clause
- `-i` — Index pattern (default: `logs-*`)
- `--json` — Raw JSON output
- `--debug` — Print generated query to stderr

### Important notes

- `labels.status` is a **keyword** field — compare with strings, not integers (e.g., `labels.status >= "500"`)
- When no `-f` is specified, default fields are: `@timestamp, service.name, message, labels.error, labels.error_message, labels.path, labels.status, trace.id`
- For raw ES|QL queries, pass the full query as a positional argument — no default KEEP is added
- LIKE queries on `message` across many services can be slow — scope with service and tight time windows

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
| Staging / production | Use `,es-web -c <cluster>` (requires session cookie from user) |
| No session available | Fall back to clipboard mode (see below) |

**Cluster naming convention:** `<env>-<region>`, e.g. `stg-us`, `stg-eu`, `prd-us`, `prd-eu`, `prd-apac`.

When no `,es-web` session is configured for the target cluster:
1. Look up the Kibana URL on the Confluence Speed Dial page (`confluence page 405504500`), open `<kibana-base-url>/app/discover#/` with `open <url>` (with `dangerouslyDisableSandbox: true`), then ask the user to copy a curl from the network tab and run `! pbpaste | ,es-web init <cluster>`
2. If that's not possible, fall back to clipboard mode:
   - Copy the ES|QL query to the user's clipboard with `pbcopy`
   - Tell the user to open Kibana, run the query in Discover (ES|QL mode)
   - Have them copy the response JSON from the network tab

## Investigation workflow tips

- **Start broad, narrow down.** First identify which services are involved, then drill into specific errors.
- **nginx → upstream.** nginx logs show external-facing status codes; upstream service logs show root cause.
- **Use trace.id** to correlate requests across services when available.
- **Error details** live in `labels.error` and `labels.error_message`.
- **Field types matter:** `numeric_labels.*` fields are numeric; `labels.*` fields are keywords (strings).
- **Time windows:** Default is 4h when `-t` is omitted. If a query times out, retry with a narrower window (e.g., `1h` → `30m`). For very broad searches (e.g., no service filter), start at `30m` and expand only if needed.
- **Overview labels** (like `http_server_overview`, `dispatcher_overview`) are useful for filtering to structured log categories without needing to match on message text.
