# Statistical analyses. Each function answers one question, returns NULL
# (with a reason attribute) when the selection cannot support it, and uses
# only base R / stats so no extra dependencies are deployed.

not_estimable <- function(reason) structure(list(), class = "not_estimable", reason = reason)
is_estimable <- function(x) !inherits(x, "not_estimable")

#' Newey-West (HAC) standard error for one coefficient of an lm fit. Daily
#' sales are autocorrelated, which makes ordinary OLS intervals too narrow.
hac_se <- function(fit, term, lag = NULL) {
  X <- stats::model.matrix(fit)
  u <- stats::residuals(fit)
  n <- nrow(X)
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
  Xu <- X * u
  meat <- crossprod(Xu)
  if (lag > 0) {
    for (l in seq_len(lag)) {
      w <- 1 - l / (lag + 1)
      cross <- crossprod(Xu[(l + 1):n, , drop = FALSE], Xu[1:(n - l), , drop = FALSE])
      meat <- meat + w * (cross + t(cross))
    }
  }
  bread <- solve(crossprod(X))
  vcov <- bread %*% meat %*% bread
  sqrt(diag(vcov))[[term]]
}

#' Average month-on-month growth in daily revenue, from
#' log(revenue) ~ day + weekday. Weekday terms stop the weekly cycle from
#' leaking into the trend. Interval uses HAC standard errors.
trend_growth <- function(daily, level = 0.95) {
  daily <- daily[daily$revenue > 0, ]
  if (nrow(daily) < 28) return(not_estimable("Needs at least 4 weeks of data."))
  daily$t <- as.numeric(daily$date - min(daily$date))
  daily$weekday <- factor(weekdays(daily$date))
  fit <- stats::lm(log(revenue) ~ t + weekday, data = daily)
  b <- stats::coef(fit)[["t"]]
  se <- hac_se(fit, "t")
  z <- stats::qnorm(1 - (1 - level) / 2)
  days_per_month <- 365.25 / 12
  list(
    monthly_growth = exp(b * days_per_month) - 1,
    ci = exp((b + c(-1, 1) * z * se) * days_per_month) - 1,
    n_days = nrow(daily),
    residual_acf1 = stats::acf(stats::residuals(fit), lag.max = 1, plot = FALSE)$acf[2]
  )
}

#' Weekend vs weekday daily revenue, compared within the same week.
#'
#' Model: log(revenue) ~ week + day + weekend. Week fixed effects absorb the
#' level of each week; the linear day term absorbs growth *within* the week,
#' which would otherwise bias the estimate upward because Saturday and Sunday
#' fall at the end of a Monday-start week. Result is a percentage difference (weekend relative to
#' weekday) with an OLS interval.
weekend_effect <- function(daily, level = 0.95) {
  daily <- daily[daily$revenue > 0, ]
  daily$week <- factor(daily$date - (as.POSIXlt(daily$date)$wday + 6) %% 7)
  daily$weekend <- as.POSIXlt(daily$date)$wday %in% c(0, 6)
  mixed_weeks <- tapply(daily$weekend, daily$week, function(w) any(w) && any(!w))
  daily <- daily[daily$week %in% names(mixed_weeks)[mixed_weeks], ]
  if (nrow(daily) < 14 || length(unique(daily$week)) < 2) {
    return(not_estimable("Needs at least two weeks containing both weekdays and weekend days."))
  }
  daily$t <- as.numeric(daily$date - min(daily$date))
  fit <- stats::lm(log(revenue) ~ week + t + weekend, data = droplevels(daily))
  ci <- stats::confint(fit, "weekendTRUE", level = level)
  list(
    pct_diff = exp(stats::coef(fit)[["weekendTRUE"]]) - 1,
    ci = as.numeric(exp(ci) - 1),
    p_value = summary(fit)$coefficients["weekendTRUE", 4],
    n_weeks = length(unique(daily$week)),
    n_days = nrow(daily)
  )
}

#' Share of revenue earned in a block of hours, with a bootstrap interval
#' that resamples whole days (keeps within-day correlation intact).
window_share <- function(data, hours = 7:10, n_boot = 2000, seed = 2023, level = 0.95) {
  if (nrow(data) == 0) return(not_estimable("No data in the selection."))
  by_day <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(in_window = sum(line_revenue[hour %in% hours]), total = sum(line_revenue), .groups = "drop")
  if (nrow(by_day) < 7) return(not_estimable("Needs at least 7 days of data."))
  share <- sum(by_day$in_window) / sum(by_day$total)
  boot <- withr_seed(seed, replicate(n_boot, {
    i <- sample.int(nrow(by_day), replace = TRUE)
    sum(by_day$in_window[i]) / sum(by_day$total[i])
  }))
  alpha <- (1 - level) / 2
  list(share = share, ci = unname(stats::quantile(boot, c(alpha, 1 - alpha))),
       hours = hours, hours_in_day = length(unique(data$hour)), n_days = nrow(by_day))
}

# Run an expression with a fixed RNG seed without disturbing the caller's RNG state
withr_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  expr
}

#' Pairwise store comparison of daily revenue, paired by date.
#'
#' All stores trade on the same days, so pairing by date removes the shared
#' trend and weekday pattern. Paired t-tests with Holm adjustment for the
#' number of pairs; effect size is the mean difference divided by the SD of
#' the differences (Cohen's d_z).
store_daily_comparison <- function(data) {
  daily <- data %>%
    dplyr::group_by(store_location, date) %>%
    dplyr::summarise(revenue = sum(line_revenue), .groups = "drop")
  stores <- sort(unique(daily$store_location))
  if (length(stores) < 2) return(not_estimable("Select at least two stores."))
  wide <- tidyr_wide(daily, stores)
  if (nrow(wide) < 7) return(not_estimable("Needs at least 7 days where all selected stores traded."))
  pairs <- utils::combn(stores, 2, simplify = FALSE)
  out <- do.call(rbind, lapply(pairs, function(p) {
    d <- wide[[p[1]]] - wide[[p[2]]]
    tt <- stats::t.test(d)
    data.frame(store_a = p[1], store_b = p[2], mean_diff = mean(d),
               ci_low = tt$conf.int[1], ci_high = tt$conf.int[2],
               pct_of_b = mean(d) / mean(wide[[p[2]]]),
               cohens_dz = mean(d) / stats::sd(d), p_value = tt$p.value, n_days = length(d))
  }))
  out$p_holm <- stats::p.adjust(out$p_value, method = "holm")
  out
}

# Days x stores matrix of daily revenue, keeping only days where every store traded
tidyr_wide <- function(daily, stores) {
  wide <- stats::reshape(as.data.frame(daily), idvar = "date", timevar = "store_location", direction = "wide")
  names(wide) <- sub("^revenue\\.", "", names(wide))
  wide[stats::complete.cases(wide[stores]), c("date", stores)]
}

#' Mean basket value (proxy order value) by store with t intervals, plus a
#' Welch ANOVA across stores. Basket values are right-skewed, but with
#' thousands of baskets per store the sampling distribution of the mean is
#' close to normal.
basket_value_by_store <- function(data, level = 0.95) {
  baskets <- data %>%
    dplyr::group_by(store_location, basket_id) %>%
    dplyr::summarise(value = sum(line_revenue), .groups = "drop")
  summary <- baskets %>%
    dplyr::group_by(store_location) %>%
    dplyr::summarise(
      baskets = dplyr::n(), mean_value = mean(value), sd = stats::sd(value),
      median_value = stats::median(value), .groups = "drop"
    ) %>%
    dplyr::mutate(
      half_width = stats::qt(1 - (1 - level) / 2, pmax(baskets - 1, 1)) * sd / sqrt(baskets),
      ci_low = mean_value - half_width, ci_high = mean_value + half_width
    )
  test <- if (nrow(summary) >= 2 && all(summary$baskets >= 2)) {
    stats::oneway.test(value ~ store_location, data = baskets, var.equal = FALSE)
  } else NULL
  # Share of basket-value variance explained by store (eta squared)
  eta_sq <- if (!is.null(test)) {
    grand <- mean(baskets$value)
    sum(summary$baskets * (summary$mean_value - grand)^2) / sum((baskets$value - grand)^2)
  } else NA_real_
  list(summary = summary, p_value = if (is.null(test)) NA_real_ else test$p.value, eta_sq = eta_sq)
}

#' Is the drink-size mix associated with the store? Chi-square test of
#' independence on sized line items, with Cramer's V as the effect size
#' (0 = no association, 1 = perfect). With ~100k lines almost any difference
#' is "significant", so V is the number to read.
size_mix_association <- function(data) {
  sized <- data[data$size != "Not sized", ]
  tab <- table(factor(sized$store_location), droplevels(sized$size))
  if (any(dim(tab) < 2)) return(not_estimable("Needs at least two stores and two sizes."))
  test <- suppressWarnings(stats::chisq.test(tab))
  list(table = tab, p_value = test$p.value, cramers_v = cramers_v(tab, test$statistic),
       n = sum(tab), min_expected = min(test$expected))
}

cramers_v <- function(tab, chisq = suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic) {
  unname(sqrt(chisq / (sum(tab) * (min(dim(tab)) - 1))))
}
