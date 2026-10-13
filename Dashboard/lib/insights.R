# Evidence-backed findings, computed from whatever data is passed in (the
# dashboard passes the filtered selection; analysis/findings.R passes the
# full dataset). Each finding states the result, the method and a caveat, and
# is skipped when the selection cannot support it.

peak_hours <- 7:10  # 07:00-10:59

finding <- function(question, result, method, caveat) {
  list(question = question, result = result, method = method, caveat = caveat)
}

insight_growth <- function(data) {
  g <- trend_growth(daily_series(data))
  if (!is_estimable(g)) return(NULL)
  monthly <- per_day_average(data, "month")
  finding(
    "Is daily revenue growing?",
    sprintf("Daily revenue grew by about %s per month (95%% CI %s to %s); mean daily revenue went from %s in %s to %s in %s.",
            fmt_pct(g$monthly_growth), fmt_pct(g$ci[1]), fmt_pct(g$ci[2]),
            fmt_money(monthly$mean_per_day[1]), monthly$month[1],
            fmt_money(utils::tail(monthly$mean_per_day, 1)), utils::tail(monthly$month, 1)),
    sprintf("Log-linear regression of daily revenue on day plus weekday, Newey-West standard errors (%d days).", g$n_days),
    "Six months cannot separate growth from seasonality (winter to summer), and the source is a sample dataset."
  )
}

insight_peak <- function(data) {
  w <- window_share(data, peak_hours)
  if (!is_estimable(w)) return(NULL)
  stores <- sort(unique(data$store_location))
  by_store <- if (length(stores) > 1) {
    shares <- vapply(stores, function(s) {
      d <- data[data$store_location == s, ]
      safe_divide(sum(d$line_revenue[d$hour %in% peak_hours]), sum(d$line_revenue))
    }, numeric(1))
    paste0(" By store: ", paste(sprintf("%s %s", names(shares), fmt_pct(shares)), collapse = ", "), ".")
  } else ""
  finding(
    "How concentrated is demand in the morning?",
    sprintf("%s of revenue falls between 07:00 and 10:59 (95%% CI %s to %s): 4 of the %d trading hours.%s",
            fmt_pct(w$share), fmt_pct(w$ci[1]), fmt_pct(w$ci[2]), w$hours_in_day, by_store),
    sprintf("Revenue share by hour; bootstrap interval resampling whole days (%d days, 2,000 resamples).", w$n_days),
    "Opening hours differ by store (Astoria opens later), so store shares partly reflect when each store is open. Sales show realised demand, not queue length or lost sales; a staffing change should be trialled and measured (e.g. wait times, sales per labour hour)."
  )
}

insight_weekend <- function(data) {
  w <- weekend_effect(daily_series(data))
  if (!is_estimable(w)) return(NULL)
  detectable <- w$ci[1] > 0 || w$ci[2] < 0
  finding(
    "Do weekends sell more than weekdays?",
    sprintf("Weekend days averaged %s revenue relative to weekdays of the same week (95%% CI %s to %s, %s): %s.",
            fmt_signed_pct(w$pct_diff), fmt_signed_pct(w$ci[1]), fmt_signed_pct(w$ci[2]), fmt_p(w$p_value),
            if (detectable) "a detectable difference" else "no detectable difference"),
    sprintf("Regression of log daily revenue on week fixed effects, a linear trend and a weekend indicator (%d weeks), so growth does not masquerade as a weekend effect.", w$n_weeks),
    "Assumes the weekend effect is the same every week; with ~50 weekend days, differences smaller than a few percent cannot be ruled out."
  )
}

insight_stores <- function(data) {
  cmp <- store_daily_comparison(data)
  if (!is_estimable(cmp)) return(NULL)
  bv <- basket_value_by_store(data)
  hi <- bv$summary[which.max(bv$summary$mean_value), ]
  lo <- bv$summary[which.min(bv$summary$mean_value), ]
  finding(
    "Do the stores perform differently?",
    sprintf(paste0("Daily revenue differences between stores are within %s of each other (smallest Holm-adjusted %s). ",
                   "Average basket value is highest at %s (%s) and lowest at %s (%s), but store explains only %s of basket-value variance."),
            fmt_pct(max(abs(cmp$pct_of_b))), fmt_p(min(cmp$p_holm)),
            hi$store_location, fmt_money(hi$mean_value, 0.01), lo$store_location, fmt_money(lo$mean_value, 0.01),
            fmt_pct(bv$eta_sq, 0.01)),
    "Paired t-tests on daily revenue (paired by date, Holm correction); Welch ANOVA and eta squared for basket value.",
    "No data on foot traffic, opening hours, floor space or costs, so this compares sales, not efficiency or profit."
  )
}

insight_basket <- function(data, support = 0.002, confidence = 0.1) {
  b <- build_baskets(data, min_items = 1)
  r <- mine_rules(b$transactions, support, confidence)
  if (is.null(r$rules)) return(NULL)
  # Rules come in symmetric pairs with equal lift; report the more confident direction
  top <- r$rules[order(-round(r$rules$lift, 6), -r$rules$confidence), ][1, ]
  lhs <- strsplit(top$LHS, " + ", fixed = TRUE)[[1]]
  rhs <- strsplit(top$RHS, " + ", fixed = TRUE)[[1]]

  # Does the association hold inside each store that sells all the items?
  # If not, the lift could just reflect where products are stocked.
  stores <- sort(unique(data$store_location))
  sells_all <- vapply(stores, function(s) all(c(lhs, rhs) %in% data$product_detail[data$store_location == s]), logical(1))
  within <- vapply(stores[sells_all], function(s) {
    store_data <- data[data$store_location == s, ]
    rule_metrics(lapply(split(store_data$product_detail, store_data$basket_id), unique), lhs, rhs)$lift
  }, numeric(1))
  availability <- if (length(stores) > 1 && length(within) > 0) {
    paste0("Sold together at ", sum(sells_all), " of ", length(stores), " selected stores; within-store lift ",
           paste(sprintf("%.1f (%s)", within, names(within)), collapse = ", "),
           ", so the pattern is not just where products are stocked. ")
  } else ""

  finding(
    "Which products are bought together?",
    sprintf("%s → %s: %s of baskets with %s also contain %s, versus %s of all baskets (lift %.1f, %s baskets). %s of baskets contain 2+ products.",
            top$LHS, top$RHS, fmt_pct(top$confidence), top$LHS, top$RHS, fmt_pct(top$rhs_support), top$lift,
            fmt_int(top$count), fmt_pct(b$n_multi / b$n_baskets)),
    sprintf("Apriori over %s store+second baskets (support >= %s, confidence >= %s), Fisher exact test with Holm adjustment.",
            fmt_int(b$n_eligible), fmt_pct(support), fmt_pct(confidence, 1)),
    paste0(availability, "Baskets are inferred, not true orders. Association is not causation; test a bundle before rolling it out.")
  )
}

build_insights <- function(data) {
  Filter(Negate(is.null), list(
    insight_growth(data), insight_peak(data), insight_weekend(data), insight_stores(data), insight_basket(data)
  ))
}
