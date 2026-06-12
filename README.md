# SIPex Google Analytics Reporting

Automated PDF reports pulling from GA4 and the CKAN API for the [SIPex](https://sipexchangebc.com/) website.
By: [Eclipse Geomatics Ltd](https://eclipsegeomatics.com/)

---

## Files

| File | Description |
|---|---|
| `00_init.R` | Shared config — libraries, constants, colours, PDF helpers |
| `SIPex_Monthly_Report.R` | Monthly snapshot report |
| `02_sipex-ga-trends.R` | Trend analytics report |

---

## Setup

### Requirements

```r
install.packages(c("googleAnalyticsR", "dplyr", "ggplot2", "scales",
                   "tidyr", "gridExtra", "grid", "jsonlite"))
```

### Authentication

Run once to authenticate with Google Analytics:

```r
library(googleAnalyticsR)
ga_auth()
```

This opens a browser window for OAuth. Credentials are cached locally for future runs.

---

## Running the Reports

First, run `source("00_init.R")`. Each script should have one at the top.

### Monthly Snapshot

Open `SIPex_Monthly_Report.R` and set the dates:

```r
date_start <- "2026-05-01"
date_end   <- "2026-05-31"
```

Then run the full script. Output: `SIPex_Monthly_YYYYMMDD_YYYYMMDD.pdf`

### Trend Report

Open `02_sipex-ga-trends.R` and update:

```r
date_start  <- "2026-05-01"   # current month (for new search term comparison)
date_end    <- "2026-05-31"
trend_start <- "2025-11-01"   # start of trend window
trend_end   <- "2026-05-31"   # end of trend window
```

Then run the full script. Output: `SIPex_Trends_YYYYMMDD_YYYYMMDD.pdf`

---

## Report Contents

### Monthly Snapshot (`SIPex_Monthly_Report.R`)

| Section | Content |
|---|---|
| 1. Traffic Snapshot | Key metrics — sessions, users, pageviews, engagement |
| 2. Traffic Sources | Sessions by channel and source/medium |
| 3. Users by City | Geographic breakdown |
| 4. Top Search Terms | What visitors searched for on-site |
| 5a. Most Visited Pages — WordPress | Top pages on sipexchangebc.com |
| 5b. Most Visited Pages — CKAN | Top pages on resources.sipexchangebc.com |
| 6. Most Downloaded Resources | Top CKAN datasets by download count |
| 7. Outbound Links | External links clicked from CKAN dataset pages |
| Device Breakdown | Desktop / mobile / tablet split |

### Trend Report (`02_sipex-ga-trends.R`)

| Section | Content |
|---|---|
| 1. Engagement Trends | Avg session duration over time — all regions |
| 1b. Engagement Trends — US & Canada | Same metrics filtered to US/CA |
| 2. Search Trends | Top searches this month |
| 2a. New Search Terms | Terms not searched in any prior month |
| 2b. Consistently Searched Terms | Searched in 3+ months |
| 2c. Search Terms With No Matching Tag | Gaps between searches and CKAN tags |
| 3a. Consistently Downloaded Resources | Downloaded in 3+ months |

---

## Configuration

### Filtering

Set `usca_only <- TRUE` in either report script to restrict all data to US and Canada visitors only.

### Bot Filtering

Defined in `00_init.R`:

- `bot_countries` — countries excluded from all data (currently Singapore, China)
- `bot_hosts` — staging/internal hostnames excluded from page data
- `bot_patterns` — page title patterns used to filter bot traffic

---

## Notes

- **CKAN API**: Sections 6 and 7 of the monthly report make live API calls to `resources.sipexchangebc.com` to resolve dataset titles. This adds ~30–60 seconds depending on the number of unique datasets.
- **GA4 truncation**: GA4 truncates `linkUrl` at 100 characters. The `resolve_resource_url()` function automatically reconstructs full CKAN download URLs via the API.
- **PDF rendering**: On Mac, the script uses `quartz()` instead of `pdf()` to preserve hyphens correctly. On other platforms it falls back to the standard `grDevices::pdf()`.
- **`TOP_N`**: Controls the number of rows in all tables and charts. Default is 20.
