# Stage A audit (baseline before the portfolio upgrade)

Audited commit: `ec1651c` (branch `main`). Every finding below was checked against the code or the
data; "risk" means a plausible problem that was not confirmed as a defect.

## Baseline

| Item | Finding |
|---|---|
| Entry point | `Dashboard/` Shiny app split into `global.R` (packages, data, helpers), `ui.R`, `server.R` |
| Framework | R 4.6.1, shiny 1.14, shinydashboard, plotly + ggplot2, DT, arules 1.7, readxl |
| Data | `Dashboard/Coffee Shop Sales.xlsx`, 149,116 rows x 11 columns, 2023-01-01 to 2023-06-30, 3 stores, 80 products |
| Provenance | Maven Analytics "Coffee Shop Sales" sample dataset (README). No licence statement in the repo |
| Cleaning | Excel time column re-parsed; derived hour, weekday, month, size, `Total_cost = unit_price * transaction_qty`. No validation, nothing dropped |
| Caching | Cleaned data cached to `coffee_sales_v2.rds`, rebuilt when the workbook is newer |
| Basket method | Items at the same store and same second grouped with `split()`; only baskets with 2+ distinct items mined |
| Deployment | Posit Connect Cloud from GitHub using `Dashboard/manifest.json` (packages + explicit file list with checksums) |
| Tests | None |
| Baseline numbers | Revenue $698,812.27; 214,470 units; 116,790 store+second baskets, 29,013 with 2+ distinct products |

## Findings (most severe first)

| # | Finding | Evidence | Why it matters | Severity | Fix | Files |
|---|---|---|---|---|---|---|
| 1 | "Total Orders" is actually **units sold** | `summarise_sales()` sets `total_orders = sum(transaction_qty)` | Every "Orders" chart overstates orders by ~84% (214,470 units vs 116,790 baskets) and mislabels the metric | High (confirmed) | Rename to "Units sold"; add a separate basket count | global.R, server.R, ui.R |
| 2 | "Transactions" KPI counts **line items**, and "Average transaction value" is the mean **line** value | `nrow(sales())`, `mean(Total_cost)`; each row has a unique `transaction_id` even when several items share a store and second | $4.69 per line is presented as a transaction value; the basket-level proxy is $5.98 | High (confirmed) | Report line items, baskets (labelled proxy), average basket value (proxy), average selling price separately | server.R |
| 3 | Basket support/lift computed **only over multi-item baskets** but the KPI shows all baskets | `build_baskets()` drops single-item baskets before `as(..., "transactions")` | Support is a share of 29,013 baskets, not the 116,790 shown; lift is conditional on basket size >= 2. Readers cannot tell which denominator was used | High (confirmed) | Mine over all eligible baskets, show eligible / multi-item / rules generated / rules retained | global.R, server.R |
| 4 | "7–10 AM" window actually covers **07:00–10:59** | `hour_of_day %in% c("07","08","09","10")` | The 45.8% claim in the app and README describes a 4-hour window as 3 hours | Medium (confirmed) | Label as 07:00–10:59 | server.R, README |
| 5 | Weekday and monthly totals not normalised for exposure | 181 days: Saturday occurs 25 times, other weekdays 26; February has 28 days | Weekday totals understate Saturday by ~4%; month totals mix growth with month length | Medium (confirmed) | Show mean revenue per trading day | server.R |
| 6 | CSV download ignores the table's column filters | `download_data` writes `explorer_data()`, not `input$data_table_rows_all` | "Download filtered data" can export more rows than the table shows | Medium (confirmed) | Export the rows currently passing the table filters, and say so | server.R, ui.R |
| 7 | Strong upward trend not acknowledged | Mean daily revenue $2,635 in Jan vs $5,550 in Jun | Any comparison across periods of unequal date coverage (stores, weekdays, filters) is confounded by trend | Medium (confirmed) | Trend-adjusted comparisons; caveats in text | new stats code |
| 8 | Products carried by only some stores inflate lift | Ouro Brasileiro shot is sold only at Hell's Kitchen and Lower Manhattan; top rule Ouro -> Ginger Scone has lift 9.2 | Co-availability, not customer preference, can drive part of the association | Medium (risk) | Document; allow per-store mining via filters | docs, UI text |
| 9 | 398 rows identical to another row except `transaction_id` | 743 rows in 345 groups; 96.5% of groups have IDs within 3 of each other | Could be repeat scans of the same item in one basket or duplicate entry; cannot tell from the data | Medium (risk) | Keep, flag, report; basket presence analysis unaffected (items de-duplicated per basket) | data code, docs |
| 10 | No data validation or quality report | No checks in `load_sales_data()` | Schema drift or bad rows would flow silently into KPIs | Medium (confirmed gap) | Validation + quality report table | new data code |
| 11 | No automated tests | No `tests/` directory | Metric regressions would go unnoticed | High for a portfolio (confirmed gap) | testthat suite incl. `shiny::testServer` | tests/ |
| 12 | Recurring suspicious high-quantity lines | 36 lines with qty >= 4; e.g. Civet Cat 8 x $45 at identical clock times on the 17th of several months | Pattern suggests templated/synthetic records; affects per-line outliers and price charts | Low (risk) | Flag, report revenue share, keep | data code, docs |
| 13 | 15 products sold at more than one unit price | e.g. bakery items at 1.25x price only at Lower Manhattan; Ouro shot at $2.10 and $3.00 | "Unit price" distributions mix price points; could be promotions or data artefacts | Low (risk) | Report; no correction | docs |
| 14 | Source spelling inconsistencies | "Jamacian Coffee River" vs "Jamaican Coffee River Lg"; "Columbian", "Carmel syrup" | Cosmetic; product IDs are consistent (80 IDs = 80 names) | Low (confirmed) | Keep source names, document | docs |
| 15 | Dataset licence undocumented; 9 MB workbook committed | README only names Maven Analytics | Redistribution terms must be stated | Low (risk) | State source and licence status in README | README |
| 16 | Stores have different opening hours | First/last sale: Astoria 07:00–19:59, Hell's Kitchen 06:30–20:59, Lower Manhattan 06:00–20:28 | Hourly store comparisons partly reflect opening hours | Low (confirmed) | Note in UI and README | ui.R, README |
| 17 | Date input can be cleared to NA | `filter(date >= input$dates[1])` with no NA check | Errors instead of a message | Low (risk) | Validate dates | server.R |

## Performance baseline

Reading the workbook takes ~1.4 s (cached afterwards); Apriori over all baskets takes < 0.1 s.
There was no measurable performance problem to fix, so no new caching layers were added.
