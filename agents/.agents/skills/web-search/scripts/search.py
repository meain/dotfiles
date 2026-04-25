#!/usr/bin/env python3
"""
Web search using lynx and DuckDuckGo
"""

import argparse
import subprocess
import sys
import re
import urllib.parse
from typing import List, Dict, Optional

def run_lynx(url: str) -> str:
    """Run lynx to dump a URL as text"""
    try:
        result = subprocess.run(
            ['lynx', '-dump', '-assume_charset=utf-8', url],
            capture_output=True,
            text=True,
            timeout=30,
            errors='replace'
        )
        return result.stdout
    except subprocess.TimeoutExpired:
        return "Error: Request timed out"
    except UnicodeDecodeError:
        # Try with different encoding handling
        try:
            result = subprocess.run(
                ['lynx', '-dump', '-assume_charset=utf-8', url],
                capture_output=True,
                timeout=30
            )
            return result.stdout.decode('utf-8', errors='replace')
        except Exception as e:
            return f"Error: {e}"
    except Exception as e:
        return f"Error: {e}"

def search_duckduckgo(query: str, limit: int = 5) -> List[Dict[str, str]]:
    """Search DuckDuckGo and parse results"""
    # URL encode the query
    encoded_query = urllib.parse.quote_plus(query)
    search_url = f"https://duckduckgo.com/?q={encoded_query}"
    
    # Get search results
    content = run_lynx(search_url)
    
    if content.startswith("Error:"):
        print(f"Search failed: {content}")
        return []
    
    results = []
    lines = content.split('\n')
    
    current_result = {}
    in_results = False
    result_count = 0
    
    for i, line in enumerate(lines):
        line = line.strip()
        
        # Skip empty lines
        if not line:
            continue
            
        # Look for numbered results (e.g., "1.", "2.", etc.)
        if re.match(r'^\d+\.\s+', line) and result_count < limit:
            # Save previous result if exists
            if current_result and current_result.get('title'):
                results.append(current_result)
                result_count += 1
                
            # Start new result
            current_result = {}
            # Extract title (remove the number prefix)
            title = re.sub(r'^\d+\.\s+', '', line)
            current_result['title'] = title
            in_results = True
            continue
            
        # Look for URLs (lines starting with www. or http)
        if in_results and (line.startswith('www.') or line.startswith('http')):
            current_result['url'] = line
            continue
            
        # Look for descriptions (non-URL lines after title)
        if in_results and current_result.get('title') and not line.startswith('[') and not line.startswith('www.') and not line.startswith('http') and len(line) > 20:
            if 'description' not in current_result:
                current_result['description'] = line
            else:
                current_result['description'] += ' ' + line
    
    # Add the last result
    if current_result and current_result.get('title') and result_count < limit:
        results.append(current_result)
    
    return results

def dump_website(url: str) -> str:
    """Dump content from a specific website"""
    print(f"Fetching content from: {url}")
    content = run_lynx(url)
    return content

def filter_results(results: List[Dict[str, str]], filter_term: str) -> List[Dict[str, str]]:
    """Filter results by a keyword"""
    filtered = []
    filter_lower = filter_term.lower()
    
    for result in results:
        title = result.get('title', '').lower()
        desc = result.get('description', '').lower()
        url = result.get('url', '').lower()
        
        if filter_lower in title or filter_lower in desc or filter_lower in url:
            filtered.append(result)
    
    return filtered

def format_results(results: List[Dict[str, str]]) -> str:
    """Format search results for display"""
    if not results:
        return "No results found."
    
    output = []
    output.append(f"Found {len(results)} results:\n")
    
    for i, result in enumerate(results, 1):
        output.append(f"{i}. **{result.get('title', 'No title')}**")
        
        if result.get('url'):
            output.append(f"   URL: {result['url']}")
            
        if result.get('description'):
            desc = result['description']
            # Limit description length
            if len(desc) > 200:
                desc = desc[:200] + "..."
            output.append(f"   {desc}")
            
        output.append("")  # Empty line between results
    
    return "\n".join(output)

def main():
    parser = argparse.ArgumentParser(description="Web search using lynx and DuckDuckGo")
    parser.add_argument("query", nargs="?", help="Search query")
    parser.add_argument("--dump", help="Dump content from a specific URL")
    parser.add_argument("--limit", type=int, default=5, help="Limit number of results (default: 5)")
    parser.add_argument("--filter", help="Filter results by keyword")
    
    args = parser.parse_args()
    
    # Check if lynx is available
    try:
        subprocess.run(['which', 'lynx'], check=True, capture_output=True)
    except subprocess.CalledProcessError:
        print("Error: lynx is not installed or not in PATH")
        sys.exit(1)
    
    if args.dump:
        # Dump specific website
        content = dump_website(args.dump)
        print(content)
        
    elif args.query:
        # Perform search
        print(f"Searching for: {args.query}")
        print("=" * 50)
        
        results = search_duckduckgo(args.query, args.limit)
        
        if args.filter:
            results = filter_results(results, args.filter)
            print(f"Filtered by: {args.filter}")
            
        print(format_results(results))
        
        # Show commands for dumping specific results
        if results:
            print("\nTo view full content of any result, use:")
            for i, result in enumerate(results, 1):
                if result.get('url'):
                    print(f"{i}. python3 ~/.agents/skills/web-search/scripts/search.py --dump \"{result['url']}\"")
        
    else:
        parser.print_help()
        sys.exit(1)

if __name__ == "__main__":
    main()