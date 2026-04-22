---
name: money-dashboard
description: >
  Analyze a Money Manager Android .mmbak backup file and generate an HTML insights dashboard
  with charts, trends, and actionable financial insights. Triggers: /money-dashboard,
  "analyze my money", "money dashboard", "analyze mmbak", "financial dashboard"
---

# /money-dashboard -- Money Manager Insights Dashboard

Analyze a `.mmbak` file (Money Manager Android backup) and produce a comprehensive HTML
insights dashboard with charts and actionable observations.

## Arguments

The user provides a path to a `.mmbak` file (or the file is in the current working directory).
If no path given, search for `*.mmbak` files in the cwd.

## Background

- `.mmbak` files are **SQLite databases** from the Realbyte Money Manager (Expense & Budget) Android app
- They contain all transactions, accounts, categories, budgets, and settings
- You can copy it to `/tmp` and rename to `.sqlite` for querying

## Key Schema

### INOUTCOME (transactions)
- `ZDATE` — Unix epoch **milliseconds**. Convert: `date(CAST(ZDATE AS REAL)/1000, 'unixepoch')`
- `DO_TYPE` — `0` = Income, `1` = Expense, `3` = Transfer out, `4` = Transfer in, `7`/`8` = fee-related
- `ZMONEY` — Amount (stored as VARCHAR, cast to REAL)
- `ZCONTENT` — User note/description
- `ctgUid` — FK to ZCATEGORY
- `assetUid` — FK to ASSETS (the account this txn belongs to)
- `toAssetUid` — FK to ASSETS (destination for transfers)
- `IS_DEL` — Soft delete flag. Always filter: `(IS_DEL=0 OR IS_DEL IS NULL)`
- `CATEGORY_NAME`, `ASSET_NIC` — Denormalized but often NULL; always JOIN instead

### ZCATEGORY (categories)
- `uid`, `NAME`, `TYPE` (0=income, 1=expense), `pUid` (parent category uid)
- `C_IS_DEL` — Soft delete
- Categories have parent-child hierarchy. Always JOIN both `c` and `pc` (parent):
  ```sql
  LEFT JOIN ZCATEGORY c ON i.ctgUid = c.uid
  LEFT JOIN ZCATEGORY pc ON c.pUid = pc.uid
  -- Use: COALESCE(pc.NAME, c.NAME, 'Uncategorized') for parent-level grouping
  ```

### ASSETS (accounts/wallets)
- `uid`, `NIC_NAME` (display name with emoji), `currencyUid`

### CURRENCY
- Single row typically (e.g., INR)

### Key relationships for transfers (DO_TYPE 3/4)
- DO_TYPE=3: money LEAVES `assetUid`, goes to the account in `toAssetUid`
- DO_TYPE=4: money ENTERS `assetUid`, comes from `toAssetUid`
- Transfers are recorded as pairs (3+4) for the same logical transfer

## Step-by-Step Process

### 1. Setup
```bash
cp "<path_to_mmbak>" /tmp/mm_backup.sqlite
sqlite3 /tmp/mm_backup.sqlite ".tables"
```
Verify the tables exist. Check `CURRENCY` to confirm currency (usually INR).

### 2. Run All Data Extraction Queries
Run these in parallel as separate sqlite3 commands exporting to JSON (`sqlite3 -json`):

**Core metrics:**
- Monthly income vs expense (GROUP BY month, DO_TYPE)
- Yearly totals (GROUP BY year, DO_TYPE)
- Expense by parent category (JOIN categories, GROUP BY parent_category)
- Income by parent category
- Monthly expense by category (for stacked chart)
- Spending by account
- Recent 100 transactions (with full JOINs)
- Daily spending last 90 days
- Monthly savings rate (income - expense per month)

**Deep analysis:**
- Expense excluding life events (identify one-off big categories like weddings, renovations)
- Salary progression (income category = Salary, by month)
- Investment flows (DO_TYPE=4 transfers TO investment accounts: Kite, MFs, LIC, FDs, PPF)
- Investment totals by vehicle
- Subscription/online service transactions
- Telecom costs by month
- Health/pharmacy/hospital by month
- Fuel/vehicle costs by month
- Day-of-week spending patterns
- Recurring expenses (GROUP BY ZCONTENT HAVING COUNT >= 3)
- Top 30 biggest single expenses
- "Difference"/untracked entries (Uncategorized with note "Difference")
- Relationship/shared spending if applicable

### 3. Analyze and Identify Insights

Before building the dashboard, analyze the data to surface:

- **Savings rate** — overall and by month, deficit months
- **Lifestyle burn rate** — excluding one-time life events
- **Spending trends** — categories growing/shrinking year-over-year
- **Salary progression** — raises, gaps, bonuses
- **Investment discipline** — SIP regularity, total allocation
- **Insurance gaps** — look for health/life/vehicle insurance premiums
- **Subscription audit** — active vs dropped services, total annual cost
- **Recurring obligations** — fixed monthly costs
- **Untracked money** — "Difference" entries, reconciliation gaps
- **Behavioral patterns** — day-of-week, payment method preferences

### 4. Build the HTML Dashboard

Create a single self-contained HTML file at `/tmp/money_insights.html` with:

**Technology:**
- Chart.js 4.x via CDN (`https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js`)
- All data embedded as inline JSON in a `<script>` tag
- Light mode, clean modern design (Inter font family)

**Required sections:**

1. **KPI cards** — Total income, total expenses, net savings (with %), lifestyle burn rate, recent 3-month average
2. **Key Insights** — Colored insight boxes (info/warn/good) with labeled findings. Include:
   - Life events as % of total spending
   - Grocery/food trends
   - Fuel costs
   - Savings discipline
   - Credit card usage %
   - Household cost trends
   - Day-of-week patterns
   - Recurring costs summary
   - Salary growth
   - Investment portfolio breakdown
   - Insurance gaps
   - Subscription audit
   - Untracked money
   - Vehicle costs
3. **Savings Rate chart** — Bar chart (green/red) with 3-month rolling average line
4. **Monthly Expenses: Raw vs Normalized** — Bar chart with year filter tabs, showing total vs lifestyle-only, plus income line and 3-month rolling average
5. **Lifestyle YoY** — Horizontal grouped bar comparing categories across years
6. **Food & Groceries trend** — Stacked bar
7. **Expense category doughnut** (excluding life events)
8. **Category detail table** — With share %, avg/txn, YoY trend tags
9. **Day of week** — Bar chart
10. **Credit vs Debit/UPI** — Area/line chart
11. **Daily spend last 90 days** — Bar with average line
12. **Yearly Income vs Expense** — Grouped bar with savings line
13. **Income sources doughnut**
14. **Salary progression** — Bar chart with average line, bonus months highlighted
15. **Investment portfolio doughnut** + **Monthly investment flow** stacked bar
16. **Subscriptions stacked bar** by service
17. **Telecom costs** bar
18. **Health spending** stacked bar (fitness/hospital/pharmacy)
19. **Fuel monthly** bar with average line
20. **Relationship/shared spending timeline** (if applicable, from dating start)
21. **Untracked "Difference"** by account — horizontal bar
22. **Recurring expenses table**
23. **Biggest single expenses table**
24. **Spending by account** horizontal bar
25. **Recent transactions table**

**Design guidelines:**
- Use CSS grid (2-column, with grid-full for wide charts)
- Cards with border-radius: 16px, subtle shadow
- Chart height: 320px default, 400px for tall, 240px for short
- Color palette: blue (#3b82f6), red (#ef4444), green (#22c55e), amber (#f59e0b), purple (#8b5cf6), teal (#14b8a6)
- Format amounts in INR: `₹X,XX,XXX` with `toLocaleString('en-IN')`
- Use `fmtL()` for lakhs shorthand (e.g., ₹2.83L)
- Tab bars for year filtering on the main trend chart
- Responsive: single column on mobile

### 5. Open the Dashboard
```bash
open /tmp/money_insights.html
```

## User Context

The user:
- Is based in India, currency is INR (₹)
- Got married recently (Oct 2025), moved from parents' to own flat (Kakkanad) with wife (Ann)
- Previously lived with parents — grocery/household cost increases are expected, not lifestyle creep
- Uses multiple payment methods: HDFC CC (primary), UPI, ICICI CC, Scapia, Kiwi
- Has investments in: Zerodha Kite (stocks), Coin (MFs), LIC policies, FDs, PPF, APY
- Tracks fuel for a Tiago with regular routes (Kozhikode, Chalakudy, Vellanchira)
- Categories with emoji prefixes are common

## Important Notes

- Always filter `IS_DEL=0 OR IS_DEL IS NULL` on all tables
- Always JOIN categories (don't rely on denormalized CATEGORY_NAME/ASSET_NIC — they're often NULL)
- For parent-level grouping: `COALESCE(pc.NAME, c.NAME, 'Uncategorized')`
- Transfer pairs (DO_TYPE 3+4) should NOT be counted as income/expense
- Investment analysis uses DO_TYPE=4 with `assetUid` pointing to the investment account
- "Difference" entries are reconciliation adjustments — flag them as untracked money
- Identify life events (weddings, renovations, flat) and calculate normalized lifestyle spend separately
- Prefer light mode for the dashboard
- Create the HTML in /tmp, then open it
