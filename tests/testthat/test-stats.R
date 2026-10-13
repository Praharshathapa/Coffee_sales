test_that("trend growth recovers a known growth rate", {
  g <- trend_growth(make_daily_sales(days = 120, growth = 0.005))
  true_monthly <- exp(0.005 * 365.25 / 12) - 1
  expect_lt(abs(g$monthly_growth - true_monthly), 0.01)
  expect_true(g$ci[1] < true_monthly && true_monthly < g$ci[2])
})

test_that("trend growth refuses short series", {
  expect_false(is_estimable(trend_growth(make_daily_sales(days = 10))))
})

test_that("weekend effect detects a real lift and not a null one", {
  lifted <- weekend_effect(make_daily_sales(weekend_lift = 0.2, growth = 0.01))
  expect_lt(abs(lifted$pct_diff - 0.2), 0.05)
  expect_gt(lifted$ci[1], 0)
  null <- weekend_effect(make_daily_sales(weekend_lift = 0, growth = 0.01, seed = 7))
  expect_true(null$ci[1] < 0 && null$ci[2] > 0)
})

test_that("weekend effect is not estimable without weekends", {
  expect_false(is_estimable(weekend_effect(make_daily_sales(days = 5))))
})

test_that("window share is exact, reproducible and leaves the RNG alone", {
  dates <- rep(seq(as.Date("2023-01-02"), by = "day", length.out = 10), each = 2)
  d <- tibble::tibble(date = dates, hour = rep(c(8, 14), 10), line_revenue = rep(c(3, 1), 10))
  w1 <- window_share(d, hours = 7:10, n_boot = 200)
  w2 <- window_share(d, hours = 7:10, n_boot = 200)
  expect_equal(w1$share, 0.75)
  expect_equal(w1$ci, w2$ci)
  expect_equal(w1$ci, c(0.75, 0.75))
  set.seed(5); a <- runif(1)
  set.seed(5); invisible(window_share(d, n_boot = 50)); b <- runif(1)
  expect_equal(a, b)
})

test_that("cramers_v is 0 for identical rows and 1 for perfect association", {
  expect_equal(cramers_v(matrix(c(10, 20, 10, 20), 2, byrow = TRUE)), 0)
  expect_equal(cramers_v(matrix(c(10, 0, 0, 10), 2)), 1)
})

test_that("HAC standard error with lag 0 equals the HC0 sandwich", {
  set.seed(3)
  x <- rnorm(200); y <- 1 + 2 * x + rnorm(200)
  fit <- lm(y ~ x)
  X <- model.matrix(fit); u <- residuals(fit)
  bread <- solve(crossprod(X))
  hc0 <- sqrt(diag(bread %*% crossprod(X * u) %*% bread))[["x"]]
  expect_equal(hac_se(fit, "x", lag = 0), hc0)
  expect_gt(hac_se(fit, "x"), 0)
})

test_that("store comparison needs two stores", {
  expect_false(is_estimable(store_daily_comparison(filter_sales(fixture, "Alpha"))))
})
