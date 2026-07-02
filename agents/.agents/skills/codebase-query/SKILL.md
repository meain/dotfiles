---
name: codebase-query
aliases: [cbm, code-graph]
description: >
  Query and analyze codebases semantically using codebase-memory-mcp graph database.
  Use to explore code structure, complexity, dependencies, call graphs, and architecture.
  Triggers: /codebase-query, /cbm, /code-graph, "show me high complexity functions",
  "what calls X", "find implementations of", "map service entry points",
  "analyze code complexity", "find technical debt", "what implements this interface"
user_invocable: true
---

# codebase-query

Query and analyze codebases using codebase-memory-mcp semantic graph.

## Triggers

- `/codebase-query`, `/cbm`, `/code-graph`
- "query the codebase graph"
- "find functions that call X"
- "show me high complexity functions"
- "what implements this interface"
- "analyze code complexity"
- "map the architecture"
- "find technical debt"

## Instructions

You are a codebase graph query assistant. Use `codebase-memory-mcp` to answer questions about code structure, complexity, dependencies, and architecture.

### Core Capabilities

1. **Search & Discovery**
   - Find functions/methods, interfaces, types by name pattern
   - Get function implementations and signatures
   - Discover entry points and exported APIs
   - Grep-like code search via `search_code`

2. **Call Graph Analysis**
   - Trace callers and callees
   - Map execution flows
   - Find hot paths and bottlenecks

3. **Complexity & Quality**
   - Identify high-complexity functions
   - Find functions with many parameters
   - Detect unguarded recursion
   - Locate potential code smells

4. **Architecture Insights**
   - High-level overview via `get_architecture`
   - Schema discovery via `get_graph_schema`
   - Map service boundaries
   - Find cross-service dependencies
   - Discover interface implementations
   - Track HTTP endpoints and async patterns

### Query Patterns

**Note:** In Claude Code, `codebase-memory-mcp` MCP tools are available natively. Prefer calling them directly over CLI. CLI examples use `2>&1 | grep -v "^level="` to suppress logs.

#### For Quick Overview (unfamiliar codebase)
Use `get_architecture` and `get_graph_schema`:
```bash
codebase-memory-mcp cli get_architecture '{"project": "...", "aspects": ["entry_points"]}' 2>&1 | grep -v "^level="
codebase-memory-mcp cli get_graph_schema '{"project": "..."}' 2>&1 | grep -v "^level=" | jq '.node_labels[] | {label: .label, count: .count}'
```

#### For Simple Lookups
Use `search_graph` or `trace_path`:
```bash
codebase-memory-mcp cli search_graph '{"project": "...", "name_pattern": ".*Router.*", "label": "Function"}' 2>&1 | grep -v "^level="
codebase-memory-mcp cli trace_path '{"project": "...", "function_name": "CreateEvent", "direction": "both", "max_depth": 2}' 2>&1 | grep -v "^level="
```

#### For Grep-Like Search
Use `search_code` when looking for string literals or patterns:
```bash
codebase-memory-mcp cli search_code '{"project": "...", "pattern": "GetOrgRegion", "file_pattern": "*.go", "limit": 10}' 2>&1 | grep -v "^level="
```

#### For Complex Analysis
Use `query_graph` with Cypher:
```bash
codebase-memory-mcp cli query_graph '{
  "project": "...",
  "query": "MATCH (f:Function) WHERE f.complexity > 15 AND f.is_test = false RETURN f.name, f.complexity, f.file_path ORDER BY f.complexity DESC LIMIT 10"
}' 2>&1 | grep -v "^level="
```

### Project Name Detection

When in a repository:
1. First try `list_projects` to find the indexed project name
2. Project names follow pattern: `Users-meain-dev-folder-repo` (path with slashes replaced by dashes)
3. For `/Users/acme/dev/myapp/backend` → `Users-acme-dev-myapp-backend`
4. On an unfamiliar project, follow with `get_graph_schema` to discover available node types and properties

### Common Queries

**Find high complexity:**
```cypher
MATCH (f:Function) 
WHERE f.complexity > 15 AND f.is_test = false 
RETURN f.name, f.complexity, f.file_path 
ORDER BY f.complexity DESC LIMIT 10
```

**Find interface implementations:**
```cypher
MATCH (s)-[:IMPLEMENTS]->(i:Interface {name: 'InterfaceName'}) 
RETURN s.name, s.file_path
```

**Find functions with many parameters:**
```cypher
MATCH (f:Function) 
WHERE f.param_count > 7 AND f.is_test = false 
RETURN f.name, f.param_count, f.file_path 
ORDER BY f.param_count DESC LIMIT 10
```

**Find entry points:**
```cypher
MATCH (f:Function) 
WHERE f.is_entry_point = true 
RETURN f.name, f.file_path
```

**Find hot paths (most-called functions):**
```cypher
MATCH ()-[:CALLS]->(f:Function)
WHERE f.is_test = false
WITH f, count(*) as call_count
RETURN f.name, call_count, f.file_path
ORDER BY call_count DESC LIMIT 15
```

**Find recursive functions:**
```cypher
MATCH (f:Function) 
WHERE f.recursive = true 
RETURN f.name, f.file_path
```

**Find HTTP endpoints:**
```cypher
MATCH (f:Function)-[h:HTTP_CALLS]->(target) 
RETURN f.name, h.url_path, f.file_path 
LIMIT 15
```

**Find external dependencies:**
```cypher
MATCH (p:Package) 
WHERE p.external = true 
RETURN p.name 
ORDER BY p.name
```

**Find similar functions (duplication candidates):**
```cypher
MATCH (f1:Function)-[s:SIMILAR_TO]->(f2:Function)
WHERE s.same_file = false AND s.jaccard > 0.7
RETURN f1.name, f2.name, s.jaccard, f1.file_path, f2.file_path
LIMIT 10
```

### Cypher Query Tips

**Boolean comparisons:**
- ✓ Use `f.is_test = false`
- ✗ Don't use `NOT f.is_test` (syntax error)

**String operations:**
- `f.file_path STARTS WITH 'services/'`
- `f.file_path ENDS WITH '_test.go'`
- `f.file_path CONTAINS 'organchor'`
- `toLower(f.name) CONTAINS 'anchor'`

**Aggregation:**
```cypher
MATCH (f:Function)-[:CALLS]->(target)
WITH f, count(*) as call_count
RETURN f.name, call_count
ORDER BY call_count DESC
```

**Limitations:**
- Variable-length paths (`*1..3`) not supported
- Some boolean operators cause errors
- Keep queries focused with `WHERE` and `LIMIT`

### Available Properties

**Function / Method nodes** (both types carry the same properties):
- `name`, `qualified_name`, `file_path`, `signature`
- `complexity`, `cognitive` (complexity metrics)
- `param_count`, `loop_count`, `loop_depth`
- `is_exported`, `is_test`, `is_entry_point`
- `recursive`, `unguarded_recursion`
- `lines`, `docstring`

**Relationships:**
- `:CALLS` - function calls
- `:IMPLEMENTS` - interface implementation
- `:IMPORTS` - module imports
- `:USAGE` - symbol usage (largest edge set)
- `:TESTS` - test relationships (may be sparse)
- `:SIMILAR_TO` - code similarity (has `.jaccard` and `.same_file` properties)
- `:HTTP_CALLS` - HTTP endpoints (has `.url_path` property)
- `:ASYNC_CALLS` - async patterns (has `.broker` property)
- `:CONFIGURES` - config access (has `.config_key` property)

### Output Processing

Filter stderr and parse JSON:
```bash
codebase-memory-mcp cli query_graph '...' 2>&1 | grep -v "^level=" | jq .
# For user-friendly tabular output:
| jq -r '.rows[] | "\(.[0]): \(.[1]) (\(.[2]))"'
```

### Workflow

1. **Understand the question** - what is the user asking?
2. **Check if project is indexed** - `list_projects`
3. **Explore schema if unfamiliar** - `get_graph_schema` to see available node types and counts
4. **Choose the right tool:**
   - High-level overview → `get_architecture`
   - Simple name search → `search_graph`
   - Call chains → `trace_path`
   - Complex queries → `query_graph`
   - Code snippets → `get_code_snippet`
   - Pattern/string search → `search_code`
5. **Execute the query** with proper filtering
6. **Format and explain results** with context

### When to Use vs. Grep/Ripgrep

**Use codebase-memory-mcp when:**
- Finding call relationships
- Analyzing complexity
- Discovering interfaces/implementations
- Understanding architecture
- Identifying technical debt

**Use `search_code` (CBM grep-like) when:**
- Pattern search but want to stay in the same tool
- Searching within already-indexed scope

**Use grep/ripgrep when:**
- Searching for string literals in comments/docs
- Patterns outside indexed scope
- Fastest possible raw text search

**Combine both:**
- CBM finds high-complexity function names → `rg` locates their usage in docs/config

### Example Interactions

**Q: "Show me the most complex functions in the earn service"**

A: Query with:
```cypher
MATCH (f:Function) 
WHERE f.file_path STARTS WITH 'services/earn/' AND f.is_test = false 
RETURN f.name, f.complexity, f.cognitive, f.file_path 
ORDER BY f.complexity DESC LIMIT 10
```

**Q: "What implements the EventRouter interface?"**

A: Query with:
```cypher
MATCH (s)-[:IMPLEMENTS]->(i:Interface {name: 'EventRouter'}) 
RETURN s.name, s.file_path
```

**Q: "Find all functions that call GetOrgRegion"**

A: Use trace_path:
```bash
codebase-memory-mcp cli trace_path '{
  "project": "...",
  "function_name": "GetOrgRegion",
  "direction": "callers",
  "max_depth": 2
}'
```

### Integration with Other Skills

- Combine with `/refactorability` to validate complexity findings
- Use before `/create-commit` to understand change impact
- Reference in `/backlog` for technical debt tasks
- Export to `/vault` for documentation

### Error Handling

- If project not found: run `list_projects` first
- If data seems stale: run `detect_changes` then `index_repository` to reindex
- If query syntax error: simplify, avoid `NOT`, use `= false`
- If timeout: add more `WHERE` filters and reduce `LIMIT`
- If empty results: check `get_graph_schema` for available properties and edge counts
- If `Function` query returns nothing: try `Method` (methods have nearly as many nodes as functions in Go repos)

### References

See `references/` for:
- `cookbook.md` — 50+ query recipes by category
- `integrations.md` — integration patterns with rg, jj, lint, gh, fzf, emacs

---

When user invokes this skill, identify what they want to analyze, construct the appropriate query, execute it, and present results with actionable insights.
