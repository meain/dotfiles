# Codebase Memory MCP Cookbook

Practical recipes for using `codebase-memory-mcp` to explore and analyze codebases.

## Setup

```bash
PROJECT="Users-acme-dev-myapp-backend"

# Helper function for cleaner output
cbm_query() {
  codebase-memory-mcp cli query_graph "{\"project\": \"$PROJECT\", \"query\": \"$1\"}" 2>&1 | grep -v "^level="
}
```

## Discovery & Navigation

### Find All Entry Points
```bash
cbm_query "MATCH (f:Function) WHERE f.is_entry_point = true RETURN f.name, f.file_path"
```

### Find All Exported Interfaces
```bash
cbm_query "MATCH (i:Interface) WHERE i.is_exported = true RETURN i.name, i.file_path LIMIT 20"
```

### Find Interface Implementations
```bash
cbm_query "MATCH (s)-[:IMPLEMENTS]->(i:Interface {name: 'EventRouter'}) RETURN s.name, s.file_path"
```

### Find All Functions in a Package
```bash
cbm_query "MATCH (f:Function) WHERE f.file_path STARTS WITH 'services/earn/internal/organchor' RETURN f.name, f.signature"
```

### Find Similar Function Names (potential duplication)
```bash
cbm_query "MATCH (f1:Function)-[s:SIMILAR_TO]->(f2:Function) WHERE s.same_file = false RETURN f1.name, f2.name, f1.file_path, f2.file_path LIMIT 10"
```

## Call Graph Analysis

### Find Direct Callers
```bash
codebase-memory-mcp cli trace_path '{"project": "'$PROJECT'", "function_name": "GetOrgRegion", "direction": "callers", "max_depth": 1}' 2>&1 | grep -v "^level="
```

### Find All Callees (What a function calls)
```bash
codebase-memory-mcp cli trace_path '{"project": "'$PROJECT'", "function_name": "CreateEvent", "direction": "callees", "max_depth": 2}' 2>&1 | grep -v "^level="
```

### Find Hot Paths (Most Called Functions)
```bash
cbm_query "MATCH ()-[c:CALLS]->(f:Function) WITH f, count(c) as call_count WHERE f.is_test = false RETURN f.name, call_count, f.file_path ORDER BY call_count DESC LIMIT 15"
```

Note: Some queries may return empty if the call graph isn't fully connected for the filtered scope.

## Complexity & Quality Metrics

### Find High Complexity Functions
```bash
cbm_query "MATCH (f:Function) WHERE f.complexity > 15 AND f.is_test = false RETURN f.name, f.complexity, f.cognitive, f.file_path ORDER BY f.complexity DESC LIMIT 15"
```

### Find High Cognitive Complexity
```bash
cbm_query "MATCH (f:Function) WHERE f.cognitive > 20 AND f.is_test = false RETURN f.name, f.cognitive, f.file_path ORDER BY f.cognitive DESC LIMIT 15"
```

### Find Functions with Many Parameters (Code Smell)
```bash
cbm_query "MATCH (f:Function) WHERE f.param_count > 7 AND f.is_test = false RETURN f.name, f.param_count, f.file_path ORDER BY f.param_count DESC LIMIT 15"
```

### Find Complexity Hotspots by File
```bash
cbm_query "MATCH (f:Function) WHERE f.file_path STARTS WITH 'services/earn/' AND f.is_test = false WITH f.file_path as file, sum(f.complexity) as total_complexity, count(f) as func_count RETURN file, total_complexity, func_count ORDER BY total_complexity DESC LIMIT 15"
```

### Find Recursive Functions
```bash
cbm_query "MATCH (f:Function) WHERE f.recursive = true RETURN f.name, f.file_path LIMIT 15"
```

### Find Unguarded Recursion (Risky Patterns)
```bash
cbm_query "MATCH (f:Function) WHERE f.unguarded_recursion = true RETURN f.name, f.file_path"
```

### Find Functions with Loops
```bash
cbm_query "MATCH (f:Function) WHERE f.loop_count > 2 AND f.is_test = false RETURN f.name, f.loop_count, f.loop_depth, f.file_path ORDER BY f.loop_depth DESC LIMIT 15"
```

## Architecture & Dependencies

### Find External Package Dependencies
```bash
cbm_query "MATCH (p:Package) WHERE p.external = true RETURN p.name ORDER BY p.name LIMIT 25"
```

### Find Cross-Service Imports
```bash
cbm_query "MATCH (f:File)-[:IMPORTS]->(m:Module) WHERE f.file_path STARTS WITH 'services/earn/' AND m.name CONTAINS 'services/' AND NOT m.name CONTAINS 'earn' RETURN DISTINCT m.name"
```

### Find HTTP Calls in Code
```bash
cbm_query "MATCH (f:Function)-[h:HTTP_CALLS]->(target) RETURN f.name, h.url_path, f.file_path LIMIT 15"
```

### Find Async/Concurrent Patterns
```bash
cbm_query "MATCH (f:Function)-[a:ASYNC_CALLS]->(target) RETURN f.name, a.broker, a.callee, f.file_path"
```

### Find Configuration Access
```bash
cbm_query "MATCH (f:Function)-[c:CONFIGURES]->(target) RETURN f.name, c.config_key, f.file_path LIMIT 20"
```

### Map Service Entry Points
```bash
cbm_query "MATCH (f:Function) WHERE f.is_entry_point = true AND f.file_path CONTAINS '/cmd/' RETURN f.file_path ORDER BY f.file_path"
```

## Testing & Coverage

### Find All Test Functions
```bash
cbm_query "MATCH (f:Function) WHERE f.is_test = true RETURN f.name, f.file_path LIMIT 20"
```

### Find Test Files
```bash
cbm_query "MATCH (f:File) WHERE f.file_path ENDS WITH '_test.go' RETURN f.file_path LIMIT 20"
```

### Find Functions That Write (Mutation Analysis)
```bash
cbm_query "MATCH (f:Function)-[w:WRITES]->() RETURN f.name, count(w) as writes, f.file_path ORDER BY writes DESC LIMIT 15"
```

## Code Search & Snippets

### Find Functions by Name Pattern
```bash
codebase-memory-mcp cli search_graph '{"project": "'$PROJECT'", "name_pattern": ".*Anchor.*", "label": "Function"}' 2>&1 | grep -v "^level="
```

### Get Function Implementation
```bash
codebase-memory-mcp cli get_code_snippet '{"project": "'$PROJECT'", "function_name": "GetOrgRegion"}' 2>&1 | grep -v "^level=" | jq -r '.snippet'
```

### Search Code (Grep-like)
```bash
codebase-memory-mcp cli search_code '{"project": "'$PROJECT'", "pattern": "organchor\\.GetOrgRegion", "file_pattern": "*.go", "limit": 10}' 2>&1 | grep -v "^level="
```

## Advanced Patterns

### Find Inheritance Chains
```bash
cbm_query "MATCH (c:Class)-[:INHERITS]->(parent:Class) RETURN c.name, parent.name, c.file_path LIMIT 15"
```

### Find Override Relationships
```bash
cbm_query "MATCH (m:Method)-[:OVERRIDE]->(base:Method) RETURN m.name, base.name, m.file_path LIMIT 15"
```

### Find Semantically Related Functions
```bash
cbm_query "MATCH (f1:Function)-[s:SEMANTICALLY_RELATED]->(f2:Function) WHERE s.same_file = false RETURN f1.name, f2.name, f1.file_path, f2.file_path LIMIT 10"
```

### Find Decorator Usage
```bash
cbm_query "MATCH (f:Function)-[:DECORATES]->(target) RETURN f.name, f.file_path LIMIT 15"
```

### Find Error Handling Patterns
```bash
cbm_query "MATCH (f:Function)-[:THROWS]->(err) RETURN f.name, f.file_path LIMIT 15"
```

## Incremental Analysis

### Check Index Status
```bash
codebase-memory-mcp cli index_status '{"project": "'$PROJECT'"}' 2>&1 | grep -v "^level="
```

### Detect Changed Files
```bash
codebase-memory-mcp cli detect_changes '{"project": "'$PROJECT'"}' 2>&1 | grep -v "^level="
```

### Reindex Repository
```bash
codebase-memory-mcp cli index_repository '{"repo_path": "/path/to/repo"}' 2>&1 | grep -v "^level="
```

## Project Insights

### Get Graph Schema
```bash
codebase-memory-mcp cli get_graph_schema '{"project": "'$PROJECT'"}' 2>&1 | grep -v "^level=" | jq '.node_labels[] | {label: .label, count: .count}'
```

### Get Architecture Overview
```bash
codebase-memory-mcp cli get_architecture '{"project": "'$PROJECT'", "aspects": ["entry_points"]}' 2>&1 | grep -v "^level=" | jq .
```

### List All Projects
```bash
codebase-memory-mcp cli list_projects 2>&1 | grep -v "^level=" | jq .
```

## Cypher Query Tips

### Boolean Comparisons
```cypher
# Use = instead of <>
WHERE f.is_test = false    # ✓
WHERE NOT f.is_test        # ✗ (syntax error)
```

### String Operations
```cypher
WHERE f.file_path STARTS WITH 'services/'
WHERE f.file_path ENDS WITH '_test.go'
WHERE f.file_path CONTAINS 'organchor'
WHERE toLower(f.name) CONTAINS 'anchor'
```

### Aggregation
```cypher
WITH f, count(*) as cnt
RETURN f.name, cnt
ORDER BY cnt DESC
```

### Pattern Matching
```cypher
# Variable-length paths don't work yet
MATCH (f1)-[:CALLS*1..3]->(f2)  # ✗ syntax error

# Use WITH and multiple MATCH instead
MATCH (f1:Function {name: 'CreateEvent'})-[:CALLS]->(f2)
WITH f2
MATCH (f2)-[:CALLS]->(f3)
RETURN f3.name
```

## Common Gotchas

1. **Project parameter is mandatory** in CLI mode
2. **Filter stderr** for clean output: `2>&1 | grep -v "^level="`
3. **Complex queries may timeout** — narrow scope with WHERE clauses
4. **Some edge types may be sparse** — check `get_graph_schema` first
5. **Variable-length path syntax** (`*1..3`) not supported yet
6. **Property access** — use `f.is_test = false` not `NOT f.is_test`

## Real-World Use Cases

### Identify Refactoring Candidates
```bash
# High complexity + many parameters
cbm_query "MATCH (f:Function) WHERE f.complexity > 15 AND f.param_count > 7 AND f.is_test = false RETURN f.name, f.complexity, f.param_count, f.file_path ORDER BY f.complexity DESC"
```

### Find Security-Sensitive Functions
```bash
# Functions handling HTTP and configuration
cbm_query "MATCH (f:Function)-[:HTTP_CALLS]->() WHERE f.file_path CONTAINS 'auth' RETURN f.name, f.file_path"
```

### Track Technical Debt
```bash
# Unguarded recursion + high cognitive complexity
cbm_query "MATCH (f:Function) WHERE f.unguarded_recursion = true AND f.cognitive > 15 RETURN f.name, f.cognitive, f.file_path"
```

### Audit Service Boundaries
```bash
# Services importing from other services (tight coupling)
cbm_query "MATCH (f:File)-[:IMPORTS]->(m:Module) WHERE f.file_path STARTS WITH 'services/user-management/' AND m.name STARTS WITH 'services/' AND NOT m.name CONTAINS 'user-management' RETURN DISTINCT m.name, f.file_path"
```

## Further Reading

- [Using codebase-memory-mcp Guide](using-codebase-memory-mcp.md)
- [GitHub: DeusData/codebase-memory-mcp](https://github.com/DeusData/codebase-memory-mcp)
- [Neo4j Cypher Documentation](https://neo4j.com/docs/cypher-manual/current/)
