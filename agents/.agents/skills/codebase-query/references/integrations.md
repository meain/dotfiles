# Combining Codebase Memory MCP with Other Tools

How to integrate `codebase-memory-mcp` with existing development workflows.

## With ripgrep (rg)

Use CBM to find functions, then grep for usage:

```bash
PROJECT="Users-acme-dev-myapp-backend"

# Find high-complexity functions
COMPLEX_FUNCS=$(codebase-memory-mcp cli query_graph "{
  \"project\": \"$PROJECT\",
  \"query\": \"MATCH (f:Function) WHERE f.complexity > 20 AND f.is_test = false RETURN f.name LIMIT 10\"
}" 2>&1 | grep -v "^level=" | jq -r '.rows[][0]')

# Search for their usage in comments/docs
for func in $COMPLEX_FUNCS; do
  echo "=== $func ==="
  rg --heading "\\b$func\\b" --type go -g '!*_test.go' | head -5
done
```

## With jq for Analysis

Extract and analyze graph data:

```bash
# Find functions and analyze parameter patterns
codebase-memory-mcp cli query_graph "{
  \"project\": \"$PROJECT\",
  \"query\": \"MATCH (f:Function) WHERE f.param_count > 5 RETURN f.name, f.param_count, f.file_path\"
}" 2>&1 | grep -v "^level=" | jq -r '
  .rows[] | 
  "File: \(.[2])\nFunction: \(.[0]) (\(.[1]) params)\n"
'
```

## With jj/git for Change Impact

Find changed files and their affected functions:

```bash
# Get changed files from jj
CHANGED_FILES=$(jj diff --git | grep '^diff --git' | sed 's/.*b\///')

# For each changed file, find defined functions
for file in $CHANGED_FILES; do
  echo "=== Functions in $file ==="
  codebase-memory-mcp cli query_graph "{
    \"project\": \"$PROJECT\",
    \"query\": \"MATCH (f:Function) WHERE f.file_path = '$file' RETURN f.name\"
  }" 2>&1 | grep -v "^level=" | jq -r '.rows[][0]'
  
  echo ""
done
```

## With golangci-lint

Compare CBM complexity metrics with linter findings:

```bash
# Find CBM-identified complex functions
CBM_COMPLEX=$(codebase-memory-mcp cli query_graph "{
  \"project\": \"$PROJECT\",
  \"query\": \"MATCH (f:Function) WHERE f.cognitive > 20 AND f.file_path STARTS WITH 'services/earn/' RETURN f.file_path, f.name, f.cognitive\"
}" 2>&1 | grep -v "^level=" | jq -r '.rows[] | "\(.[0]):\(.[1]) (\(.[2]))"')

# Run linter on those files
echo "$CBM_COMPLEX" | cut -d: -f1 | sort -u | while read file; do
  echo "=== Linting $file ==="
  golangci-lint run --disable-all --enable gocognit,gocyclo "$file"
done
```

## With gh CLI for PR Context

Analyze changed functions in a PR:

```bash
# Get PR diff
PR_NUMBER=1234
gh pr diff $PR_NUMBER --name-only | while read file; do
  echo "=== $file ==="
  
  # Find functions in changed file
  codebase-memory-mcp cli query_graph "{
    \"project\": \"$PROJECT\",
    \"query\": \"MATCH (f:Function) WHERE f.file_path = '$file' RETURN f.name, f.complexity\"
  }" 2>&1 | grep -v "^level=" | jq -r '.rows[] | "\(.[0]): complexity \(.[1])"'
done
```

## With fzf for Interactive Exploration

```bash
# Interactive function browser
function_browser() {
  codebase-memory-mcp cli query_graph "{
    \"project\": \"$PROJECT\",
    \"query\": \"MATCH (f:Function) WHERE f.is_exported = true RETURN f.name, f.file_path, f.signature\"
  }" 2>&1 | grep -v "^level=" | jq -r '.rows[] | "\(.[0]) (\(.[1])) \(.[2])"' | \
  fzf --preview 'echo {} | cut -d" " -f1 | xargs -I{} codebase-memory-mcp cli get_code_snippet "{\"project\": \"'$PROJECT'\", \"function_name\": \"{}\"}" 2>&1 | grep -v "^level=" | jq -r .snippet'
}
```

## With Taskwarrior

Track refactoring tasks for complex functions:

```bash
# Find technical debt
codebase-memory-mcp cli query_graph "{
  \"project\": \"$PROJECT\",
  \"query\": \"MATCH (f:Function) WHERE f.complexity > 20 AND f.param_count > 8 RETURN f.name, f.file_path, f.complexity\"
}" 2>&1 | grep -v "^level=" | jq -r '.rows[] | 
  "task add project:refactoring +tech-debt \"Refactor \(.[0]) in \(.[1]) (complexity: \(.[2]))\""' | sh
```

## With Make for CI Integration

Add to `Makefile`:

```makefile
.PHONY: analyze-complexity
analyze-complexity:
	@echo "=== High Complexity Functions ==="
	@codebase-memory-mcp cli query_graph '{"project": "$(PROJECT)", "query": "MATCH (f:Function) WHERE f.complexity > 15 AND f.is_test = false RETURN f.name, f.complexity, f.file_path ORDER BY f.complexity DESC LIMIT 20"}' 2>&1 | grep -v "^level=" | jq -r '.rows[] | "\(.[1])\t\(.[0])\t\(.[2])"'

.PHONY: find-entry-points
find-entry-points:
	@codebase-memory-mcp cli query_graph '{"project": "$(PROJECT)", "query": "MATCH (f:Function) WHERE f.is_entry_point = true RETURN f.file_path"}' 2>&1 | grep -v "^level=" | jq -r '.rows[][0]'

.PHONY: index-codebase
index-codebase:
	@codebase-memory-mcp cli index_repository '{"repo_path": "$(PWD)"}' 2>&1 | grep -v "^level="
```

## With pre-commit Hooks

`.git/hooks/pre-commit`:

```bash
#!/bin/bash
PROJECT="Users-acme-dev-myapp-backend"

# Get staged files
STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACMR | grep '\.go$')

if [ -z "$STAGED_FILES" ]; then
  exit 0
fi

# Check complexity of changed functions
for file in $STAGED_FILES; do
  HIGH_COMPLEX=$(codebase-memory-mcp cli query_graph "{
    \"project\": \"$PROJECT\",
    \"query\": \"MATCH (f:Function) WHERE f.file_path = '$file' AND f.complexity > 20 RETURN f.name, f.complexity\"
  }" 2>&1 | grep -v "^level=" | jq -r '.rows[]')
  
  if [ ! -z "$HIGH_COMPLEX" ]; then
    echo "⚠️  High complexity functions in $file:"
    echo "$HIGH_COMPLEX" | jq -r '"\(.[0]): \(.[1])"'
    echo ""
  fi
done
```

## With Obsidian/Vault

Generate documentation:

```bash
# Create architecture map
mkdir -p ~/.local/share/sbdb/Reference/Architecture

codebase-memory-mcp cli get_architecture "{
  \"project\": \"$PROJECT\",
  \"aspects\": [\"entry_points\"]
}" 2>&1 | grep -v "^level=" | jq -r '
  "# Service Entry Points\n\n" +
  (.entry_points[] | 
    "- **\(.name)**: `\(.file)`\n"
  )
' > ~/.local/share/sbdb/Reference/Architecture/service-entry-points.md

# Create complexity report
codebase-memory-mcp cli query_graph "{
  \"project\": \"$PROJECT\",
  \"query\": \"MATCH (f:Function) WHERE f.cognitive > 20 RETURN f.name, f.cognitive, f.file_path ORDER BY f.cognitive DESC LIMIT 30\"
}" 2>&1 | grep -v "^level=" | jq -r '
  "# Technical Debt: High Cognitive Complexity\n\n" +
  "Functions with cognitive complexity > 20:\n\n" +
  (.rows[] | 
    "- **\(.[0])** (\(.[1])): `\(.[2])`\n"
  )
' > ~/.local/share/sbdb/Reference/technical-debt-complexity.md
```

## With Emacs/LSP

Create a command to jump to function definition:

```elisp
(defun cbm-find-function (func-name)
  "Find function using codebase-memory-mcp and jump to it."
  (interactive "sFunction name: ")
  (let* ((cmd (format "codebase-memory-mcp cli query_graph '{\"project\": \"%s\", \"query\": \"MATCH (f:Function {name: \\\"%s\\\"}) RETURN f.file_path, f.start_line\"}' 2>&1 | grep -v '^level=' | jq -r '.rows[0]'"
                     "Users-acme-dev-myapp-backend"
                     func-name))
         (result (shell-command-to-string cmd))
         (data (json-parse-string result :array-type 'list)))
    (when data
      (find-file (concat (projectile-project-root) (nth 0 data)))
      (goto-line (nth 1 data)))))
```

## With jq for Metrics Dashboard

```bash
# Generate JSON metrics for dashboard
cat > /tmp/code-metrics.json << EOF
{
  "total_functions": $(codebase-memory-mcp cli query_graph "{\"project\": \"$PROJECT\", \"query\": \"MATCH (f:Function) RETURN count(f)\"}" 2>&1 | grep -v "^level=" | jq '.rows[0][0]'),
  "high_complexity": $(codebase-memory-mcp cli query_graph "{\"project\": \"$PROJECT\", \"query\": \"MATCH (f:Function) WHERE f.complexity > 15 RETURN count(f)\"}" 2>&1 | grep -v "^level=" | jq '.rows[0][0]'),
  "recursive_functions": $(codebase-memory-mcp cli query_graph "{\"project\": \"$PROJECT\", \"query\": \"MATCH (f:Function) WHERE f.recursive = true RETURN count(f)\"}" 2>&1 | grep -v "^level=" | jq '.rows[0][0]'),
  "exported_interfaces": $(codebase-memory-mcp cli query_graph "{\"project\": \"$PROJECT\", \"query\": \"MATCH (i:Interface) WHERE i.is_exported = true RETURN count(i)\"}" 2>&1 | grep -v "^level=" | jq '.rows[0][0]')
}
EOF

cat /tmp/code-metrics.json | jq .
```

## With GitHub Actions

`.github/workflows/complexity-check.yml`:

```yaml
name: Complexity Check

on: [pull_request]

jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Install codebase-memory-mcp
        run: |
          # Installation steps here
      
      - name: Index codebase
        run: codebase-memory-mcp cli index_repository '{"repo_path": "."}'
      
      - name: Check complexity
        run: |
          PROJECT=$(pwd | sed 's/\//-/g' | sed 's/^-//')
          HIGH_COMPLEX=$(codebase-memory-mcp cli query_graph "{
            \"project\": \"$PROJECT\",
            \"query\": \"MATCH (f:Function) WHERE f.complexity > 25 RETURN f.name, f.complexity, f.file_path\"
          }" 2>&1 | grep -v "^level=" | jq -r '.rows[]')
          
          if [ ! -z "$HIGH_COMPLEX" ]; then
            echo "::error::Found functions with complexity > 25"
            echo "$HIGH_COMPLEX" | jq -r '"::error file=\(.[2])::Function \(.[0]) has complexity \(.[1])"'
            exit 1
          fi
```

## Tips for Integration

1. **Always filter stderr** — tool is verbose: `2>&1 | grep -v "^level="`
2. **Cache results** — queries can be slow, save JSON output
3. **Use jq for transformation** — raw output is JSON
4. **Combine with ripgrep** — CBM finds symbols, rg finds usage
5. **Generate reports regularly** — track complexity trends over time
6. **Integrate with CI** — fail builds on excessive complexity
7. **Create wrapper scripts** — hide common query boilerplate
8. **Export to JSON/CSV** — import into other analysis tools

## Example Wrapper Script

```bash
#!/bin/bash
# cbm-query - Simplified wrapper for codebase-memory-mcp

PROJECT="${CBM_PROJECT:-Users-acme-dev-myapp-backend}"

case "$1" in
  complex)
    codebase-memory-mcp cli query_graph "{\"project\": \"$PROJECT\", \"query\": \"MATCH (f:Function) WHERE f.complexity > ${2:-15} RETURN f.name, f.complexity, f.file_path ORDER BY f.complexity DESC LIMIT ${3:-20}\"}" 2>&1 | grep -v "^level=" | jq -r '.rows[] | "\(.[1])\t\(.[0])\t\(.[2])"'
    ;;
  
  callers)
    codebase-memory-mcp cli trace_path "{\"project\": \"$PROJECT\", \"function_name\": \"$2\", \"direction\": \"callers\", \"max_depth\": ${3:-2}}" 2>&1 | grep -v "^level=" | jq .
    ;;
  
  snippet)
    codebase-memory-mcp cli get_code_snippet "{\"project\": \"$PROJECT\", \"function_name\": \"$2\"}" 2>&1 | grep -v "^level=" | jq -r '.snippet'
    ;;
  
  *)
    echo "Usage: cbm-query {complex|callers|snippet} [args...]"
    exit 1
    ;;
esac
```

Usage:
```bash
cbm-query complex 20 10          # Top 10 functions with complexity > 20
cbm-query callers GetOrgRegion   # Who calls GetOrgRegion?
cbm-query snippet CreateEvent    # Show CreateEvent implementation
```
