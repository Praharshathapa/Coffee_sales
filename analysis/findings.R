# Reproduces every number quoted in the README "Findings" section, using the
# same functions as the dashboard. Run from the project root:
#   Rscript analysis/findings.R
# Output is written to docs/findings_output.txt.

suppressPackageStartupMessages({
  library(dplyr)
  library(arules)
})

lib_dir <- file.path("Dashboard", "lib")
for (f in c("data.R", "metrics.R", "baskets.R", "stats.R", "insights.R", "plots.R")) source(file.path(lib_dir, f))

bundle <- clean_sales(read_raw_sales(file.path("Dashboard", "Coffee Shop Sales.xlsx")))
sales <- bundle$data

out_file <- file.path("docs", "findings_output.txt")
sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)
options(width = 120, pillar.width = 120)

section <- function(title) cat("\n", strrep("=", 80), "\n", title, "\n", strrep("=", 80), "\n", sep = "")

section("Data quality")
print(as.data.frame(bundle$quality), right = FALSE)

section("Headline KPIs (full period, all stores)")
print(as.data.frame(kpi_summary(sales)))

section("Findings as shown on the Executive Overview")
for (f in build_insights(sales)) {
  cat("\nQ:", f$question, "\nResult:", f$result, "\nMethod:", f$method, "\nCaveat:", f$caveat, "\n")
}

section("1. Growth: mean daily revenue by month")
print(as.data.frame(per_day_average(sales, "month")))
str(trend_growth(daily_series(sales)))

section("2. Morning peak: share of revenue 07:00-10:59, overall and by store")
str(window_share(sales, peak_hours))
for (s in sort(unique(sales$store_location))) {
  w <- window_share(filter_sales(sales, s), peak_hours)
  cat(sprintf("%-16s share %s (95%% CI %s-%s)\n", s, fmt_pct(w$share), fmt_pct(w$ci[1]), fmt_pct(w$ci[2])))
}
heat <- per_day_average(sales, c("day_of_week", "hour"), "revenue")
cat("\nBusiest weekday-hour cells (mean revenue per day):\n")
print(as.data.frame(head(arrange(heat, desc(mean_per_day)), 5)))
cat("\nMean revenue per day by hour (all stores):\n")
print(as.data.frame(per_day_average(sales, "hour", "revenue")))

section("3. Weekday vs weekend")
print(as.data.frame(per_day_average(sales, "day_of_week")))
str(weekend_effect(daily_series(sales)))

section("4. Stores")
print(as.data.frame(summarise_sales(sales, store_location) %>% add_share()))
print(store_daily_comparison(sales))
bv <- basket_value_by_store(sales)
print(as.data.frame(bv$summary))
cat("Welch ANOVA", fmt_p(bv$p_value), " eta^2 =", round(bv$eta_sq, 5), "\n")
sm <- size_mix_association(sales)
print(sm$table)
cat("Size mix chi-square", fmt_p(sm$p_value), " Cramer's V =", round(sm$cramers_v, 4), "\n")
cat("\nCategory share of store revenue:\n")
print(as.data.frame(
  summarise_sales(sales, store_location, product_category) %>%
    group_by(store_location) %>% mutate(share = round(revenue / sum(revenue), 3)) %>%
    select(store_location, product_category, share) %>%
    tidyr::pivot_wider(names_from = store_location, values_from = share)
))

section("5. Basket analysis: all baskets vs multi-item baskets")
for (mi in 1:2) {
  b <- build_baskets(sales, mi)
  r <- mine_rules(b$transactions, 0.002, 0.1)
  cat(sprintf("\nmin_items = %d: %s baskets, %s with 2+ products, %s eligible, %s repeat lines de-duplicated\n",
              mi, fmt_int(b$n_baskets), fmt_int(b$n_multi), fmt_int(b$n_eligible), fmt_int(b$n_repeat_lines)))
  cat(sprintf("Rules generated: %d, retained: %d\n", r$n_generated, r$n_retained))
  print(head(r$rules[, c("LHS", "RHS", "support", "confidence", "rhs_support", "lift", "count", "p_adjusted")], 10))
}

section("5b. Top rule within each store that sells both products")
for (s in sort(unique(sales$store_location))) {
  store_data <- filter_sales(sales, s)
  b <- build_baskets(store_data)
  bl <- lapply(split(store_data$product_detail, store_data$basket_id), unique)
  if (!all(c("Ouro Brasileiro shot", "Ginger Scone") %in% store_data$product_detail)) {
    cat(sprintf("%-16s does not sell both products\n", s))
    next
  }
  m <- rule_metrics(bl, "Ouro Brasileiro shot", "Ginger Scone")
  cat(sprintf("%-16s Ouro -> Ginger: support %s, confidence %s, lift %.2f (%s baskets)\n",
              s, fmt_pct(m$support, 0.01), fmt_pct(m$confidence), m$lift, fmt_int(length(bl))))
}
cat("\nStores selling each product:\n")
print(table(sales$product_detail[sales$product_detail %in% c("Ouro Brasileiro shot", "Ginger Scone")],
            sales$store_location[sales$product_detail %in% c("Ouro Brasileiro shot", "Ginger Scone")]))

section("Session info")
cat(R.version.string, "\n")
for (p in c("dplyr", "arules", "shiny", "readxl")) cat(p, as.character(packageVersion(p)), "\n")
