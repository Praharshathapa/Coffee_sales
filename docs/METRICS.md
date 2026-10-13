# Metric definitions

All metrics are computed by `Dashboard/lib/metrics.R` (and `baskets.R` for basket measures) on the
rows that pass the sidebar filters. The same table is shown in the app under
**Baskets & Data → Data quality & definitions**.

## Grain of the data

One row is one **line item**: a product, a quantity and a unit price, sold at one store at one
second. `transaction_id` is unique for every row, including rows rung up together, so it is **not**
an order ID. Quantities range from 1 to 8 (mean 1.44).

There is no order ID in the source. Orders are approximated by **baskets**: all line items sold at
the same store in the same second. In every one of the 29,260 multi-line baskets the transaction
IDs are consecutive, which supports the proxy, but it remains an inference. It can merge two
customers served in the same second, or split one order rung up across two seconds.

## Definitions

| Metric | Definition | Notes |
|---|---|---|
| Revenue | Σ `unit_price × transaction_qty` | Gross sales. No cost data, so nothing about profit or margin |
| Units sold | Σ `transaction_qty` | Labelled "Total Orders" in the original app; renamed because it counts items, not orders |
| Line items | Number of rows | Labelled "Transactions" in the original app |
| Baskets (proxy orders) | Distinct (store, timestamp to the second) | 116,790 over the full period |
| Average basket value | Revenue ÷ baskets | Proxy for average order value ($5.98 overall). The original app showed mean line value ($4.69) as "average transaction value" |
| Units per basket | Units ÷ baskets | Proxy for items per order |
| Average selling price | Revenue ÷ units | Mix-weighted price per unit |
| Revenue per store-day | Revenue ÷ distinct (store, date) pairs with sales | Normalises for the number of stores and days selected |
| Mean daily value (weekday, month, hour) | Total for that level ÷ number of trading days of that level in scope | 181 days include 25 Saturdays and 26 of every other weekday; months have 28–31 days |
| Revenue share | Group revenue ÷ total revenue in the same filter scope | Shares in one chart sum to 100% |
| Period change | (Selected period − preceding period of equal length) ÷ preceding period | Shown only when the preceding period lies inside the data |
| Support | Baskets containing all items of a rule ÷ eligible baskets | "Eligible" is all baskets by default, or only baskets with 2+ products |
| Confidence | Baskets with antecedent and consequent ÷ baskets with antecedent | Compare with the consequent's own share |
| Lift | Confidence ÷ support of the consequent | > 1: bought together more often than if independent |
| Consequent share | Support of the consequent alone (= confidence ÷ lift) | Shown so a high confidence for a very common product is not over-read |

## Reconciliation checks (automated)

`tests/testthat/test-app.R` checks, against the raw workbook:

- total revenue, units and line items equal the raw sums;
- the basket count equals the number of distinct (store, date, time) combinations;
- store totals add up to the overall total inside a filter;
- the CSV export contains exactly the filtered rows, and honours the table's column filters.
