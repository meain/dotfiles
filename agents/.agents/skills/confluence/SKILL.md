---
name: confluence
description: >
  Look up, read, create, update, search, and navigate Confluence pages using the confluence CLI.
  Use as fallback when Atlassian MCP tools are unavailable. Triggers: /confluence,
  "look up confluence page", "check confluence", "read confluence", "create confluence page",
  "update confluence page", "search confluence", "find in confluence", "confluence page",
  any Confluence page URL like https://veeam-vdc.atlassian.net/wiki/...
argument-hint: "[page ID, URL, or search query]"
allowed-tools:
  - Bash
---

# Confluence Page Management (CLI)

Use the `confluence` CLI to read, create, update, search, and navigate Confluence pages.
This is the fallback when Atlassian MCP tools are not available.

## Veeam Context

- Confluence instance: `veeam-vdc.atlassian.net`
- Common spaces: `DP` (Data Pipelines), `VDC`, `ENG`
- Page URLs: `https://veeam-vdc.atlassian.net/wiki/spaces/<SPACE>/pages/<pageId>`

## MCP-to-CLI Mapping

When Atlassian MCP tools (Cloud ID: `cb1bcab7-9522-4cf1-b2ac-8b141ee24914`)
are unavailable, use these CLI equivalents:

| MCP Tool | CLI Equivalent |
|----------|---------------|
| `getConfluencePageDescendants(cloudId, pageId, depth=1)` | `confluence children --show-id <pageId>` |
| `searchConfluenceUsingCql` (e.g. `ancestor = <id>`) | `confluence search --cql "ancestor = <id>"` |
| `getConfluencePage(cloudId, pageId)` | `confluence read -f markdown <pageId>` |
| `createConfluencePage(cloudId, spaceId, title, parentId, body)` | `confluence create-child "<title>" <parentId> --format markdown -f <file>` |
| `updateConfluencePage(cloudId, pageId, title, body)` | `confluence update <pageId> --format markdown -f <file>` |

**Note on folder-type pages:** MCP's `getConfluencePageDescendants` 404s on
folder-type pages (e.g. Oncall Investigations root `1293025298`). The CLI
does not have this limitation — `confluence children 1293025298` works fine.

## Command Reference

### Reading & Navigation

| Command | Description |
|---------|-------------|
| `confluence read <pageId>` | Read page content (text by default) |
| `confluence read -f markdown <pageId>` | Read page as markdown |
| `confluence read -f html <pageId>` | Read page as raw HTML |
| `confluence info <pageId>` | Get page metadata (title, ID, type) |
| `confluence children <pageId>` | List child pages |
| `confluence children --show-id <pageId>` | List children with page IDs |
| `confluence children --format tree <pageId>` | List children as tree |
| `confluence children -r <pageId>` | List all descendants recursively |

### Searching

| Command | Description |
|---------|-------------|
| `confluence search "<query>"` | Text search across all spaces |
| `confluence search --cql "<CQL>"` | Search using CQL (e.g. `ancestor = <id>`) |
| `confluence search -l 20 "<query>"` | Search with custom result limit |
| `confluence find "<title>"` | Find a page by exact title |
| `confluence find -s DP "<title>"` | Find a page by title in a specific space |

### Creating & Updating

| Command | Description |
|---------|-------------|
| `confluence create "<title>" <spaceKey>` | Create a new top-level page |
| `confluence create-child "<title>" <parentId>` | Create a child page |
| `confluence create-child "<title>" <parentId> --format markdown -f <file>` | Create child from markdown file |
| `confluence create-child "<title>" <parentId> -c "<content>" --format markdown` | Create child with inline content |
| `confluence update <pageId> -f <file> --format markdown` | Update page from file |
| `confluence update <pageId> -c "<content>" --format storage` | Update page with storage format |
| `confluence update <pageId> -t "<new title>"` | Rename a page |

### Other Operations

| Command | Description |
|---------|-------------|
| `confluence comments <pageId>` | List comments on a page |
| `confluence comment <pageId>` | Add a comment to a page |
| `confluence move <pageId> <newParentId>` | Move a page to a new parent |
| `confluence delete <pageId>` | Delete a page (use with caution) |
| `confluence attachments <pageId>` | List attachments |
| `confluence attachment-upload <pageId>` | Upload attachments |
| `confluence export <pageId>` | Export page with attachments |
| `confluence copy-tree <srcId> <dstParentId>` | Copy a page tree |
| `confluence spaces` | List all spaces |

## Navigating Page Hierarchies

Walk a page tree by listing children at each level:

```bash
confluence children --show-id 1293025298          # root -> year pages
confluence children --show-id <year-page-id>      # year -> month pages
confluence children --show-id <month-page-id>     # month -> day pages
confluence children --show-id <day-page-id>       # day -> report pages
```

**Known page IDs** (Oncall Investigations, may change over time):
- Root folder: `1293025298`
- 2026 year page: `1291845743`
- 04-April month page: `1290961023`

## Content Formats

- `--format markdown` — for creating/updating pages with markdown content
- `--format storage` — Confluence storage format (HTML-like), needed for
  advanced layouts like full-width tables with `data-layout="full-width"`
- When writing long content, write to a temp file first and use `-f <file>`
  to avoid shell escaping issues

## Instructions

1. **Extract page ID** from URL (`/pages/<pageId>`) or use search/find to locate it
2. **Run the appropriate command** from the reference above
3. **Present results**: for reads show content, for searches show titles + IDs,
   for creates/updates confirm with the page URL
