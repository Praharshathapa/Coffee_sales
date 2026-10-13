# Coffee Shop Sales Dashboard

An R Shiny dashboard for exploring six months of point-of-sale data from three coffee shops in New York (Astoria, Hell's Kitchen and Lower Manhattan). The dataset covers 149,116 transactions from January to June 2023.

The app covers sales trends, busy periods, product and store performance, drink size and pricing, and products that are commonly bought together.

![Overview tab](docs/screenshots/overview.png)

Live demo: _coming soon_

## Features

- Store and date filters in the sidebar that apply to every tab
- Hourly, weekly and monthly sales trends, plus a weekday-by-hour heatmap
- Revenue and units by product category, individual product and store
- Size mix and price distribution by size and store
- Market basket analysis (Apriori) with adjustable support and confidence thresholds
- A searchable transaction table with CSV export

| Peak hours | Basket analysis |
|---|---|
| ![Peak hours](docs/screenshots/peak_hours.png) | ![Basket analysis](docs/screenshots/basket_analysis.png) |

## Findings

- Monthly revenue doubled over the period, from $81.7K in January to $166.5K in June.
- Sales are concentrated in the morning: 7 to 10 AM accounts for 45.8% of revenue.
- Coffee (38.6%) and tea (28.1%) make up most of the revenue. Bakery items come third at 11.8%.
- The three stores bring in almost the same revenue (each about 33%), but their best-selling products differ.
- The strongest product pairing is Ouro Brasileiro shot with Ginger Scone: 72% of baskets containing the shot also contained the scone.

## Getting started

Requires R 4.1 or later.

```r
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "ggplot2",
                   "readxl", "scales", "RColorBrewer", "DT", "arules"))

shiny::runApp("Dashboard")
```

The first launch reads the Excel file and writes a cached `.rds` copy, so later launches start faster.

### Analysis script

`analysis/coffee_sales_analysis.R` contains the original exploratory analysis: static ggplot charts and the first Apriori run. Open `coffee-sales-dashboard.Rproj` before running it so that file paths resolve from the project root. It needs a few extra packages: `tidyverse`, `reshape2`, `corrplot`, `ggthemes`, `knitr`, `treemapify` and `arulesViz`.

### Deployment

The `Dashboard` folder contains everything the app needs, including the data, so it can be deployed to shinyapps.io directly:

```r
rsconnect::deployApp("Dashboard", appName = "coffee-sales-dashboard")
```

## Project structure

```
Dashboard/
  global.R      data loading, cleaning and shared helpers
  ui.R          layout and styling
  server.R      reactive logic and charts
  www/          static assets
analysis/
  coffee_sales_analysis.R
docs/screenshots/
```

## Notes on the data

- Each row in the source data has its own transaction ID, even when several items were bought together. For the basket analysis, items sold at the same store at the same second are treated as one basket. This is an approximation, and it produces about 29,000 baskets with two or more items.
- Drink size comes from the `Lg`, `Rg` or `Sm` suffix on the product name. Products without a suffix, such as bakery items and packaged goods, are grouped as "Others".
- Source: Maven Analytics coffee shop sales sample dataset.
