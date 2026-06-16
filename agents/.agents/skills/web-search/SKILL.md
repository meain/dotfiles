---
name: web-search
description: >
  Search the web using lynx and DuckDuckGo. Performs text-based web searches and can dump specific
  website content. Do NOT use in Claude Code — use the built-in WebSearch tool instead. Only use
  this skill in environments where WebSearch is unavailable.
triggers:
  - /web-search
  - "search the web"
  - "search for" 
  - "look up online"
  - "web search"
  - "find on the internet"
  - "search duckduckgo"
  - "search online"
  - "google this"
  - "find information about"
metadata:
  author: meain
  version: "1.0.0"
  license: MIT
---

# /web-search — Web Search Using Lynx + DuckDuckGo

Search the web using lynx text browser with DuckDuckGo. Bypasses bot protection and provides clean, text-based results.

**Note**: This skill should only be used when the built-in `WebSearch` tool is not available and no other more specific search tools exist (e.g., documentation APIs, specialized search tools). If `WebSearch` is available in the current environment, use that instead.

## Usage

```bash
# Basic web search
python3 ~/.agents/skills/web-search/scripts/search.py "search query"

# Search with more results
python3 ~/.agents/skills/web-search/scripts/search.py "search query" --limit 10

# Dump specific website content
python3 ~/.agents/skills/web-search/scripts/search.py --dump "https://example.com"

# Search and filter results
python3 ~/.agents/skills/web-search/scripts/search.py "search query" --filter "keyword"
```

## Examples

```bash
# Basic search
python3 ~/.agents/skills/web-search/scripts/search.py "best budget earbuds india under 5000"

# Technology news search
python3 ~/.agents/skills/web-search/scripts/search.py "golang 1.24 release notes"

# Get specific website content
python3 ~/.agents/skills/web-search/scripts/search.py --dump "https://www.smartprix.com/mobile_headphones/best-earbuds-under-5000-list"

# Search with filtering
python3 ~/.agents/skills/web-search/scripts/search.py "rust programming tutorial" --filter "beginner"

# Limit results
python3 ~/.agents/skills/web-search/scripts/search.py "machine learning courses" --limit 5
```

## Features

- **Text-based browsing**: Uses lynx to bypass JavaScript and bot protection
- **Clean output**: Extracts readable content without ads and distractions  
- **DuckDuckGo integration**: Privacy-focused search engine
- **Website dumping**: Can extract content from specific URLs
- **Result filtering**: Filter results by keywords
- **Configurable limits**: Control number of search results

## How It Works

1. Uses lynx to search DuckDuckGo with your query
2. Extracts clean search results with titles, descriptions, and URLs
3. Can follow specific URLs to dump full content
4. Provides structured, readable output

## Requirements

- `lynx` text browser (already available in your environment)
- Python 3 (for the search script)

## Notes

- Lynx bypasses most bot protection mechanisms
- Results are text-only (no images or JavaScript content)
- DuckDuckGo provides privacy-focused search without tracking
- Works well for technical documentation, news, and informational sites
- Some heavily JavaScript-dependent sites may not render properly
- **For fetching individual web pages**: Use `curl` and `html2markdown` instead of the `--dump` option for better compatibility and cleaner output