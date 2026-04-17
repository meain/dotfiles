---
name: interview-report
description: >
  Analyze a URL shortener take-home interview submission and generate a detailed report card.
  Reads all code from a zip file or git repo, performs deep code review (bugs, architecture, testing,
  security, production readiness), scores across dimensions, identifies issues with actual code examples
  and suggested fixes, generates follow-up interview questions with good/bad answer signals, and
  produces an HTML + PDF report card.
  Triggers: /interview-report, "review this candidate", "analyze take home", "generate report card",
  "interview report", "assess this submission", "review assignment", "candidate report",
  "analyze the code", "go through this submission".
  Always triggered when reviewing code in an interviews/ directory.
---

# Interview Report Card Generator

Generate a comprehensive HTML report card for a URL shortener take-home assignment submission.

## Input

The submission is either:
- A `.zip` file in the current directory -- extract it first
- A git repo (the current directory itself)

The candidate name should be inferred from the directory name, repo name, git author, or zip filename.

## Process

### 1. Extract and Read All Code

If zip: `unzip -o <file>.zip -d extracted` then read all files.
If repo: read all source files directly.

Read EVERY file. Do not skip any. Use a subagent for parallel reading if >10 files.

### 2. Build, Run Tests, and Functional Testing

Before analyzing the code, verify it actually works:

1. **Run existing tests**: `go test ./...` (or equivalent for the language). Note which pass, fail, or are missing.
2. **Build the project**: `go build -o url-shortener .` — note if it even compiles (check go.mod version).
3. **Start the server** in the background and test all endpoints with curl:
   - `POST /shorten` with a valid URL — verify you get a short URL back
   - `GET /<shortcode>` — verify 302 redirect to the original URL
   - `POST /shorten` with the same URL again — verify deterministic (same code) or not
   - `GET /metrics` (or equivalent) — verify the response
   - **Error cases**: malformed JSON, empty URL, non-URL string, unknown short code
   - **Security probes**: `javascript:alert(1)`, `data:text/html,...`, empty scheme
   - **Metrics stress test**: shorten 5+ URLs from the same domain, then check if top-K results are correct or have duplicate entries
4. **Kill the server** when done: `pkill -f url-shortener`

Record all test results (HTTP status codes, response bodies, redirect locations). Include a **Functional Test Results** section in the report with a pass/fail table showing what was tested and what broke. This provides concrete evidence beyond static code review.

### 3. Analyze Against URL Shortener Criteria

Score each dimension 1-10. Use these as evaluation anchors specific to a URL shortener:

**Architecture & Design (weight: high)**
- Layering: handler/controller -> service -> storage. Are layers cleanly separated?
- Dependency injection: can components be swapped/tested independently?
- ID/short-code generation strategy: hash-based, counter-based, random? Collision handling?
- URL validation: scheme checks, hostname validation, length limits?
- Route design: clean API paths, no conflicts?

**Correctness (weight: critical)**
- Does shorten -> resolve round-trip actually work?
- Hash/ID collisions: detected or silently overwritten?
- Operation ordering: validate before store, not after
- Concurrency: TOCTOU races, proper locking patterns
- Error propagation: do errors bubble up correctly?
- Edge cases: empty URL, duplicate URL, non-existent short code

**Code Quality (weight: medium)**
- Language idioms (Go: error handling, naming conventions, receiver patterns; Python: PEP8; JS/TS: async patterns)
- Consistent logging (not mixing fmt.Println and log.Printf)
- Naming: typos, shadowed variables, initialism conventions
- Dead code, debug statements left in

**Testing (weight: high)**
- Coverage: which layers have tests? Which don't?
- Mock quality: do mocks accurately simulate real behavior, or do they have hard-coded values that make tests pass for wrong reasons?
- Assertion strength: do tests verify meaningful properties or just "not empty"?
- Edge case tests: invalid input, not-found, concurrent access
- Missing tests for the buggiest components is a red flag

**Security (weight: medium)**
- Open redirect: does it validate URL schemes before redirecting?
- Input validation: length limits, scheme whitelist (http/https only)
- Rate limiting or auth (presence/absence)
- Data exposure: logging sensitive data, leaking internal state
- Injection vectors

**Container & Infra Readiness (weight: high — org hires for k8s skills)**
This is a key hiring signal. Evaluate thoroughly:
- **Dockerfile quality**: multi-stage build? Layer caching (COPY go.mod before COPY .)? Minimal final image (alpine/distroless/scratch)? .dockerignore present? Non-root user? Valid base image version?
- **Docker Compose**: present? Services wired correctly? Depends_on, healthchecks, volume mounts for persistent storage?
- **Kubernetes manifests**: any k8s YAML (Deployment, Service, Ingress, ConfigMap, HPA)? Resource limits/requests? Liveness/readiness probes? Even a basic manifest is a strong positive.
- **Graceful shutdown**: handles SIGTERM for clean pod termination? Uses http.Server with Shutdown(ctx)?
- **Health check endpoints**: /healthz, /readyz for k8s probes?
- **Configuration**: via env vars (12-factor) suitable for ConfigMaps/Secrets?
- **Bonus signals**: Helm chart, Makefile with docker/k8s targets, CI/CD pipeline, Kustomize

Rate this dimension generously for any k8s/compose presence. A candidate who includes even a basic Deployment+Service YAML shows infra awareness that is directly relevant to the role.

**Documentation & README (weight: medium)**
- Does the README explain how to build, run, and test the project?
- Are design decisions documented (why MD5, why in-memory, trade-offs)?
- API documentation (endpoints, request/response examples)?
- Architecture overview or diagram?
- Known limitations / future improvements section?
- A good README shows communication skills and product thinking.

**Testing Quality (weight: high)**
Beyond just test existence, evaluate test quality holistically:
- Table-driven tests (idiomatic Go)?
- Integration/E2E tests that start the server and test HTTP?
- Benchmark tests for performance-sensitive paths?
- Race condition tests (`go test -race`)?
- Test helpers/fixtures vs copy-pasted setup?
- Meaningful test names that describe behavior?

NOTE: Do NOT score or include a "Production Readiness" section. This is a quick take-home assignment done in a couple of days on the side — we don't expect full production hardening. Prod-relevant items like graceful shutdown and health probes are evaluated under Container & Infra instead.

### 3. Generate HTML Report

Write a `report.html` in the current working directory using the template from `assets/report-template.html` as the CSS/structure base. Do NOT just fill in placeholders -- write a complete HTML file with all findings populated. The template includes `@media print` CSS that prevents page breaks and preserves colors in PDF output -- always include this in the generated HTML.

Required sections (all collapsible):

1. **Header**: candidate name, project type, language
2. **Overall verdict**: weighted score, one-line summary, paragraph rationale
3. **Score grid**: one card per dimension with color coding
   - 1-3: `c-red` / `bg-red`
   - 4-5: `c-orange` / `bg-orange`
   - 6-7: `c-yellow` / `bg-yellow`
   - 8-10: `c-green` / `bg-green`
4. **Functional test results**: table with columns: Test, Input, Expected, Actual, Pass/Fail. Include all endpoint tests, error cases, and security probes from step 2.
5. **Critical bugs**: each with the candidate's actual code (use `<span class="line-bad">` for problematic lines), explanation of what goes wrong, and a corrected version (use `<span class="line-good">`)
6. **Testing analysis**: mock quality issues, coverage gaps table, weak assertions
7. **Architecture & Design**: DI problems, ID generation critique, validation gaps
8. **Code Quality**: idiom violations, naming issues, debug leftovers
9. **Security concerns**: open redirects, missing validation, data leaks
10. **Container & Infra**: Dockerfile review, docker-compose presence, k8s manifests, graceful shutdown, health probes, env-based config. This is a key section — call out what's present and what's missing, with examples of what a good setup looks like.
11. **Documentation & README**: what the README covers, what's missing, quality of explanations
11. **Strengths**: use `strength-item` class, always include positives
12. **Follow-up questions** in 4 sub-sections:
    - **Low-level code understanding** (7+ questions): mutex usage, hashing mechanics, operation ordering, error handling, race conditions, algorithm tracing, slice/reference semantics. Each question must include: the exact question to ask, a table with good/bad answer signals, and a code snippet from their actual code.
    - **Mid-level design extensions** (5+ questions): add Redis backend, configuration management, TTL/expiration, rate limiting, health checks. Each with expected answer points.
    - **Architecture & scale** (3+ questions): multi-instance deployment, scaling to high throughput, analytics pipeline.
    - **Process & quality** (3+ questions): "did you run it?", priorities with more time, live coding exercise.
12. **Recommendation**: Strong Hire / Hire / Lean No Hire / No Hire with rationale

### 4. Generate PDF, Open HTML, and Summarize

After writing report.html:
1. Convert to PDF (silently, in the same directory):
   ```bash
   python3 ~/.claude/skills/interview-report/scripts/html_to_pdf.py report.html report.pdf
   ```
   Requires Chrome/Chromium. If Chrome is not available, skip PDF and warn the user.
2. Open only the HTML: `open report.html` (macOS) or `xdg-open report.html` (Linux). Do NOT open the PDF.
3. Print a concise summary to the user covering: overall score, top 3 issues, recommendation. Mention that a PDF version is also available at report.pdf.

## Key Patterns to Always Check in URL Shortener Submissions

These are the most common bugs and anti-patterns across submissions:

- **Hash collision silent overwrite**: MD5/SHA truncation without collision detection
- **Store before validate**: caching the URL before checking if it's valid
- **TOCTOU in resolve**: separate Exists() then Load() calls instead of single atomic Load()
- **Broken top-K / metrics**: heap implementations that accumulate duplicates
- **Empty response on error**: handler returns without writing HTTP status
- **Mock Exists() always true**: tests pass for wrong code paths
- **url.Parse is too lenient**: Go's url.Parse accepts almost anything; need scheme/host validation
- **Logging entire data store**: debug statements dumping all cached data on every operation
- **No graceful shutdown**: using framework's Run() shortcut instead of http.Server with Shutdown()
- **Non-existent Go version in go.mod**: suggests code was never built/run
- **Mutex instead of RWMutex**: exclusive lock for read operations
- **Returning internal slice/map references**: leaking mutable internal state
