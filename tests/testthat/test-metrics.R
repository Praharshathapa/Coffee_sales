test_that("KPIs match hand calculations on the fixture", {
  k <- kpi_summary(fixture)
  expect_equal(k$revenue, 9.5 + 3 + 10 + 7.5)
  expect_equal(k$units, 2 + 1 + 1 + 1 + 1 + 1 + 3)
  expect_equal(k$line_items, 7)
  expect_equal(k$baskets, 4)
  expect_equal(k$avg_basket_value, 30 / 4)
  expect_equal(k$avg_units_per_basket, 10 / 4)
  expect_equal(k$avg_selling_price, 30 / 10)
  expect_equal(k$store_days, 3)  # Alpha day 1, Beta day 1, Beta day 2
  expect_equal(k$revenue_per_store_day, 10)
})

test_that("the same second at different stores is two baskets", {
  morning <- fixture[fixture$hour == 8 & fixture$date == as.Date("2023-01-02"), ]
  expect_equal(dplyr::n_distinct(morning$basket_id), 3)
})

test_that("empty data gives zeros and NA ratios, not errors or Inf", {
  k <- kpi_summary(fixture[0, ])
  expect_equal(k$revenue, 0)
  expect_equal(k$baskets, 0)
  expect_true(is.na(k$avg_basket_value))
  expect_true(is.na(k$avg_selling_price))
  expect_true(is.na(k$revenue_per_store_day))
})

test_that("safe_divide handles zero and NA denominators", {
  expect_equal(safe_divide(1, 2), 0.5)
  expect_true(is.na(safe_divide(1, 0)))
  expect_true(is.na(safe_divide(1, NA)))
  expect_equal(safe_divide(c(1, 2), c(0, 4)), c(NA, 0.5))
  # Vector over scalar must keep every element (regression: ifelse() returned one value)
  expect_equal(safe_divide(c(1, 3), 4), c(0.25, 0.75))
  expect_equal(safe_divide(c(1, 3), 0), c(NA_real_, NA_real_))
})

test_that("group totals reconcile with the overall total", {
  k <- kpi_summary(fixture)
  by_store <- summarise_sales(fixture, store_location)
  expect_equal(sum(by_store$revenue), k$revenue)
  expect_equal(sum(by_store$units), k$units)
  expect_equal(sum(by_store$baskets), k$baskets)
  by_cat <- add_share(summarise_sales(fixture, product_category))
  expect_equal(sum(by_cat$share), 1)
  expect_equal(by_cat$revenue[by_cat$product_category == "Bakery"], 10.5)
})

test_that("per-day averages divide by trading days of each level", {
  wk <- per_day_average(fixture, "day_of_week", "revenue")
  expect_equal(wk$mean_per_day[wk$day_of_week == "Monday"], 22.5)
  expect_equal(wk$days[wk$day_of_week == "Tuesday"], 1)
  # Hour is not a calendar column: hour 15 sold on one of two days but averages over both
  hr <- per_day_average(fixture, "hour", "revenue")
  expect_equal(hr$days, c(2, 2))
  expect_equal(hr$mean_per_day[hr$hour == 15], 7.5 / 2)
})

test_that("daily series fills gaps with zero", {
  d <- fixture
  d$date[d$date == as.Date("2023-01-03")] <- as.Date("2023-01-05")
  s <- daily_series(d)
  expect_equal(s$date, seq(as.Date("2023-01-02"), as.Date("2023-01-05"), by = "day"))
  expect_equal(s$revenue, c(22.5, 0, 0, 7.5))
  expect_equal(nrow(daily_series(fixture[0, ])), 0)
})

test_that("period comparison only reports change when the prior period is in the data", {
  first_day <- period_comparison(fixture, NULL, "2023-01-02", "2023-01-02")
  expect_false(first_day$available)
  expect_true(all(is.na(first_day$change)))
  second_day <- period_comparison(fixture, NULL, "2023-01-03", "2023-01-03")
  expect_true(second_day$available)
  expect_equal(second_day$change[["revenue"]], (7.5 - 22.5) / 22.5)
})
