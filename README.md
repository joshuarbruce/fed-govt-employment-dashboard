# Federal Government Employment Dashboard

An interactive dashboard for exploring changes in the U.S. federal civilian workforce from 2005 to the present. Built as a portfolio project to make a large public dataset more accessible and visually useful.

**[View the live dashboard](https://josh-bruce-fed-govt-employment-dashboard.share.connect.posit.cloud)**

---

## What it shows

The dashboard draws on OPM's public employment records — roughly 420 million rows covering every federal civilian employee from March 2005 through early 2026 — and surfaces four views:

**Overview** — Total federal headcount over time with key performance indicators: current workforce size, year-over-year change, STEM share of the workforce, and STEM headcount year-over-year change.

**By Agency** — Select one or more agencies to explore headcount trends over time. Supports combined view (single summed line) or split view (one line per agency). A ranked table shows the top sub-agencies by headcount for the most recent month.

**Scientists & STEM** — Stacked area chart of the STEM workforce broken down by occupation type (Science, Technology, Engineering, Mathematics, Health). Filterable by agency. A ranked table shows which agencies employ the most STEM workers.

**Geography** — Leaflet choropleth map showing federal employment concentration by state. Three metrics available: absolute headcount, year-over-year change in count, and year-over-year change as a percentage. Filterable by agency and STEM-only.

---

## Data source

Source data is the [Federal Workforce Data](https://data.opm.gov/explore-data/data/data-downloads) published by the U.S. Office of Personnel Management (OPM). The dataset covers monthly snapshots of all federal civilian employees and includes agency, sub-agency, occupation, pay, work schedule, and duty station information.

OPM published quarterly snapshots (March/June/September/December) from 2005–2011 and monthly snapshots from approximately 2012 onward. Early "missing" months reflect this publication schedule.

---

## Architecture

Querying 420 million rows of raw data on every page interaction would be slow and expensive. Instead, three pre-aggregated BigQuery summary tables (~300MB total) power all four tabs:

| Table | Groups | Powers |
|---|---|---|
| `agency_monthly` | year, month, agency, sub-agency, occupational group, work schedule | Overview, By Agency |
| `stem_monthly` | year, month, agency, sub-agency, STEM type, state | Scientists & STEM |
| `geo_monthly` | year, month, state, agency, STEM type | Geography |

All three tables are loaded into memory at app startup. Reactive outputs use `bindCache()` for instant re-renders after the first computation.

State boundary geometries (all 50 states + DC) are sourced from the U.S. Census Bureau via the `tigris` package and cached locally.

---

## Tech stack

- **R Shiny** — app framework
- **bslib** — Bootstrap 5 UI with custom brand theme
- **Google BigQuery** — data warehouse (via `bigrquery`)
- **plotly** — interactive time series and area charts
- **leaflet** — choropleth map
- **reactable** — ranked tables with inline bar charts
- **tigris** / **sf** — Census state boundary geometries
- **renv** — reproducible package environment
- **Posit Connect Cloud** — hosting

---

## Running locally

Requires R 4.4+, a Google Cloud project with BigQuery access, and Application Default Credentials configured (`gcloud auth application-default login`).

```r
# Restore package environment
renv::restore()

# Run the app
shiny::runApp()
```

The app expects the three summary BigQuery tables described above. SQL scripts to create them from the raw OPM data are in the `sql/` directory.

---

## License and attribution

**Data**: Employment records are published by the U.S. Office of Personnel Management and are in the public domain. Source: [data.opm.gov](https://data.opm.gov/explore-data/data/data-downloads)

**Code**: Copyright &copy; Joshua Bruce. All rights reserved.
