# Summarize News Feed

Fetch and summarize all articles from popular tech news aggregators like Lobsters or Hacker News.

## Instructions

1. Fetch the webpage content from the provided URL
2. Extract all article titles, URLs, vote counts, and comment counts
3. **Get detailed summaries for EVERY article linked on the page** by fetching and summarizing each individual article
4. Format the output with:
   - Short summary outside details block
   - Link in the summary line
   - Comprehensive details inside expandable HTML details blocks
   - No bold formatting for titles
   - Include vote and comment counts
   - Group by category/topic where applicable

## Supported Sites

- Lobsters (https://lobste.rs/)
- Hacker News (https://news.ycombinator.com/)

## Output Format

```html
<details>
<summary>Number. <a href="URL">Article Title</a> (X votes, Y comments) - Brief one-line summary</summary>

Full detailed summary with multiple paragraphs explaining:
- Main points and arguments
- Key technical details
- Examples and use cases
- Conclusions and takeaways
- Relevant context

Without excessive formatting or bold text in headings.
</details>
```

## Important

This prompt will fetch and summarize the actual content of every article linked on the news aggregator page, not just the headlines. Each article gets its own comprehensive summary based on the full text.

## Example Usage

User provides a URL (or just "lobsters" or "hackernews"), and you return a comprehensive summary of all articles in the structured format above.
