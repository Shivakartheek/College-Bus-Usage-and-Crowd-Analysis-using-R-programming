# College Bus Usage and Crowd Analysis Using R

Analyzes college bus operation data (routes, buses, passenger counts,
delays) to answer eight operational questions using R, dplyr, and
ggplot2 — plus two interactive front ends: a Shiny dashboard and a
standalone HTML dashboard.

## Folder structure

```
bus_project/
├── data/
│   └── college_bus.csv        # dataset (synthetic, 90 days, 6 routes, 10 buses)
├── scripts/
│   ├── 01_generate_data.R     # generates the synthetic dataset
│   └── 02_analysis.R          # batch analysis: prints all 8 answers, saves 7 charts
├── plots/                     # static chart output from 02_analysis.R
├── shiny_app/
│   ├── app.R                  # interactive Shiny dashboard
│   └── college_bus.csv        # same dataset, local copy for the app
├── dashboard.html             # standalone HTML/JS dashboard (no R needed to view)
└── README.md
```

## 1. Batch analysis (core R project)

```bash
Rscript scripts/01_generate_data.R   # optional — data/college_bus.csv is already included
Rscript scripts/02_analysis.R        # prints all 8 answers, saves 7 charts to plots/
```

This is the deliverable that demonstrates the R concepts: CSV I/O,
data frames, cleaning, dplyr grouping/aggregation, correlation, and
ggplot2 charts (bar/line/heatmap).

## 2. Shiny dashboard (interactive, in R)

```r
install.packages(c("shiny", "shinythemes", "dplyr", "ggplot2", "DT", "plotly"))
shiny::runApp("shiny_app")
```

Opens in your browser at `http://127.0.0.1:PORT`. Filters (route,
weekday/weekend, departure time, date range, overcrowding threshold)
drive every KPI, chart, and table live. Five tabs: Route & Bus,
Trends, Heatmap, Overcrowded Trips, Route Summary Table.

To deploy it publicly, push `shiny_app/` to shinyapps.io via
`rsconnect::deployApp("shiny_app")`, or run it on a Shiny Server.

## 3. Standalone HTML dashboard (no R required)

Just open `dashboard.html` in any browser — the dataset is embedded,
so it works offline with no server or install. Use this version if
you want to share a link or embed it somewhere R can't run.

## Dataset

`data/college_bus.csv` — one row per bus trip.

| Column          | Description                              |
|-----------------|-------------------------------------------|
| Date            | Trip date                                  |
| Day             | Day of week                                |
| Bus_ID          | Bus identifier (B01–B10)                   |
| Route           | Route name (Route 1–6)                     |
| Departure_Time  | Scheduled departure (08:00 or 16:30)       |
| Arrival_Time    | Derived arrival time                       |
| Passenger_Count | Passengers on that trip                    |
| Bus_Capacity    | Seating/standing capacity of that bus      |
| Distance_KM     | Route distance                             |
| Delay_Minutes   | Minutes late                               |

The generator deliberately injects a handful of missing values and
one duplicate row so the cleaning step (dedup + route-median
imputation) has real work to do — the same cleaning logic is
duplicated in `02_analysis.R` and `shiny_app/app.R` so all three
outputs agree.

## What the analysis answers

1. **Highest-passenger route** — route-wise average & max passengers
2. **Peak travel timings** — total/average passengers by departure slot
3. **Most crowded buses** — average occupancy % and % of trips over threshold
4. **Average passengers per route** — route summary table
5. **Weekday vs weekend usage** — grouped comparison
6. **Overcrowded trips** — trips exceeding a configurable occupancy threshold (default 90%)
7. **Low-utilization routes** — routes averaging under 50% occupancy
8. **Monthly passenger trends** — total/average passengers by month

## R concepts demonstrated

CSV I/O, data frames, data cleaning (NA imputation, deduplication),
dplyr (`group_by`, `summarise`, `mutate`, `filter`, `arrange`),
aggregation (mean/median/max), correlation, ggplot2 (bar/line/heatmap
charts), and — in the Shiny app — reactive programming (`reactive()`,
`observeEvent`, `renderPlot`, `renderText`, `renderDT`).

## Sample findings (from the included dataset)

- **Route 3** has the highest average passenger count (~39 passengers/trip, 86.8% avg occupancy).
- **08:00** (morning) is the peak travel period.
- **Bus B03** is the most consistently crowded bus — over 90% occupancy on 53% of its trips.
- **17.1%** of all trips are overcrowded (>90% occupancy).
- **Routes 4 and 6** are under-utilized, averaging below 40% occupancy — candidates for schedule optimization.
- Occupancy % and delay minutes show a moderate positive correlation (~0.48), suggesting crowding contributes to lateness.
