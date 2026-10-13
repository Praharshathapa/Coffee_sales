# Metric definitions. Every KPI and chart in the app goes through these
# functions so a metric means the same thing on every page. See
# docs/METRICS.md for the written definitions.

#' Division that returns NA instead of Inf/NaN when the denominator is zero.
safe_divide <- function(numerator, denominator) {
  result <- numerator / denominator
  result[rep_len(is.na(denominator) | denominator == 0, length(result))] <- NA_real_
  result
}

#' Headline metrics for a (filtered) set of line items.
#'
#' baskets = distinct store + second combinations, a proxy for orders (see
#' baskets.R). store_days = distinct store + date pairs with at least one sale,
#' used to normalise for the number of stores and days selected.
kpi_summary <- function(data) {
  revenue <- sum(data$line_revenue)
  units <- sum(data$transaction_qty)
  baskets <- dplyr::n_distinct(data$basket_id)
  store_days <- nrow(dplyr::distinct(data, store_id, date))
  tibble::tibble(
    revenue = revenue,
    units = units,
    line_items = nrow(data),
    baskets = baskets,
    avg_basket_value = safe_divide(revenue, baskets),
    avg_units_per_basket = safe_divide(units, baskets),
    avg_selling_price = safe_divide(revenue, units),
    trading_days = dplyr::n_distinct(data$date),
    store_days = store_days,
    revenue_per_store_day = safe_divide(revenue, store_days)
  )
}

#' Revenue, units, line items and baskets grouped by the given columns.
summarise_sales <- function(data, ...) {
  data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      revenue = sum(line_revenue),
      units = sum(transaction_qty),
      line_items = dplyr::n(),
      baskets = dplyr::n_distinct(basket_id),
      store_days = dplyr::n_distinct(store_id, date),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      revenue_per_store_day = safe_divide(revenue, store_days),
      units_per_store_day = safe_divide(units, store_days)
    )
}

#' Add each group's share of the column total. The denominator is the total
#' of the rows passed in, i.e. the current filter scope.
add_share <- function(summary, column = "revenue") {
  summary$share <- safe_divide(summary[[column]], sum(summary[[column]]))
  summary
}

#' Daily totals with every calendar day in range present (days with no sales
#' get zero), plus a trailing 7-day mean once 7 days are available.
daily_series <- function(data) {
  if (nrow(data) == 0) {
    return(tibble::tibble(date = as.Date(character()), revenue = numeric(), units = numeric(),
                          baskets = integer(), rolling_7d = numeric()))
  }
  daily <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(revenue = sum(line_revenue), units = sum(transaction_qty),
                     baskets = dplyr::n_distinct(basket_id), .groups = "drop")
  all_days <- tibble::tibble(date = seq(min(daily$date), max(daily$date), by = "day"))
  daily <- dplyr::left_join(all_days, daily, by = "date") %>%
    dplyr::mutate(dplyr::across(c(revenue, units, baskets), ~ dplyr::coalesce(.x, 0)))
  daily$rolling_7d <- if (nrow(daily) >= 7) as.numeric(stats::filter(daily$revenue, rep(1 / 7, 7), sides = 1)) else NA_real_
  daily
}

#' Mean daily value for each level of a calendar dimension (weekday, month,
#' hour x weekday, ...). Totals are divided by the number of trading days of
#' that calendar level inside the filter, so a weekday that occurs 25 times is
#' not compared with one that occurs 26 times on raw totals. Non-calendar
#' columns in `by` (e.g. hour, store) do not change the denominator, so an
#' hour with no sales on some days still averages over all of them.
calendar_columns <- c("day_of_week", "is_weekend", "month", "week_start", "date")

per_day_average <- function(data, by, value = c("revenue", "units", "baskets")) {
  value <- match.arg(value)
  calendar_by <- intersect(by, calendar_columns)
  day_counts <- dplyr::distinct(data, dplyr::across(dplyr::all_of(c("date", calendar_by)))) %>%
    dplyr::count(dplyr::across(dplyr::all_of(calendar_by)), name = "days")
  totals <- summarise_sales(data, dplyr::across(dplyr::all_of(by)))
  totals <- if (length(calendar_by) > 0) {
    dplyr::left_join(totals, day_counts, by = calendar_by)
  } else {
    dplyr::mutate(totals, days = day_counts$days)
  }
  totals %>%
    dplyr::transmute(dplyr::across(dplyr::all_of(by)), total = .data[[value]], days,
                     mean_per_day = safe_divide(total, days))
}

#' KPIs for the selected period and for the immediately preceding period of
#' the same length. `available` is FALSE when that earlier period is not fully
#' inside the dataset, in which case no change should be shown.
period_comparison <- function(full_data, stores, date_from, date_to) {
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)
  length_days <- as.integer(date_to - date_from) + 1
  prior_to <- date_from - 1
  prior_from <- date_from - length_days
  current <- kpi_summary(filter_sales(full_data, stores, date_from, date_to))
  prior <- kpi_summary(filter_sales(full_data, stores, prior_from, prior_to))
  available <- prior_from >= min(full_data$date) && prior$line_items > 0
  metrics <- c("revenue", "units", "baskets", "avg_basket_value", "revenue_per_store_day")
  change <- if (available) {
    safe_divide(unlist(current[metrics]) - unlist(prior[metrics]), unlist(prior[metrics]))
  } else {
    stats::setNames(rep(NA_real_, length(metrics)), metrics)
  }
  list(current = current, prior = prior, prior_from = prior_from, prior_to = prior_to,
       available = available, change = change)
}

#' Written definitions shown in the app and mirrored in docs/METRICS.md.
metric_dictionary <- tibble::tribble(
  ~Metric, ~Definition, ~Notes,
  "Revenue", "Sum of unit_price x transaction_qty over line items in scope", "Gross sales; no cost data, so no profit or margin",
  "Units sold", "Sum of transaction_qty", "Formerly labelled \"Total Orders\" in the app",
  "Line items", "Number of rows", "transaction_id is unique per row, so this is not an order count",
  "Baskets (proxy orders)", "Distinct store + timestamp (to the second)", "Inferred; see Basket definition",
  "Average basket value", "Revenue / baskets", "Proxy for average order value",
  "Units per basket", "Units sold / baskets", "Proxy for items per order",
  "Average selling price", "Revenue / units sold", "Mix-weighted; differs from the mean of unit_price",
  "Revenue per store-day", "Revenue / number of store-date pairs with sales", "Normalises for the number of stores and days selected",
  "Mean daily value (weekday, month, hour)", "Total for the level / number of trading days of that level in scope", "Corrects for 25 Saturdays vs 26 of other weekdays, and month length",
  "Revenue share", "Group revenue / total revenue in the same filter scope", "Shares within a chart always sum to 100%",
  "Period change", "(Selected period - preceding period of equal length) / preceding period", "Shown only when the preceding period is inside the data",
  "Support", "Share of eligible baskets containing all items of the rule", "Denominator shown as \"eligible baskets\"",
  "Confidence", "Baskets with antecedent and consequent / baskets with antecedent", "Read alongside the consequent's own prevalence",
  "Lift", "Confidence / support of the consequent", "> 1: bought together more often than if independent"
)
