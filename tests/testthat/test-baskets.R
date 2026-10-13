basket_list <- function(data) lapply(split(data$product_detail, data$basket_id), unique)

test_that("baskets group by store and second, de-duplicating repeat lines", {
  b <- build_baskets(fixture)
  expect_equal(b$n_baskets, 4)
  expect_equal(b$n_multi, 2)
  expect_equal(b$n_eligible, 4)
  expect_equal(b$n_repeat_lines, 1)   # the second Scone line in basket 3
  expect_equal(length(b$transactions), 4)
  expect_equal(build_baskets(fixture, min_items = 2)$n_eligible, 2)
})

test_that("empty input yields an empty, safe result", {
  b <- build_baskets(fixture[0, ])
  expect_equal(b$n_baskets, 0)
  expect_null(b$transactions)
  r <- mine_rules(b$transactions, 0.1, 0.1)
  expect_null(r$rules)
  expect_equal(r$n_generated, 0)
})

test_that("support, confidence and lift follow their definitions", {
  m <- rule_metrics(basket_list(fixture), "Scone", "Latte Rg")
  # Scone in baskets 1 and 3; Latte in baskets 1, 2 and 3 of 4
  expect_equal(m$support, 2 / 4)
  expect_equal(m$confidence, 1)
  expect_equal(m$lift, 1 / (3 / 4))
})

test_that("arules output agrees with the direct calculation", {
  r <- mine_rules(build_baskets(fixture)$transactions, support = 0.25, confidence = 0.5)
  rule <- r$rules[r$rules$LHS == "Scone" & r$rules$RHS == "Latte Rg", ]
  expect_equal(nrow(rule), 1)
  expect_equal(rule$support, 0.5)
  expect_equal(rule$confidence, 1)
  expect_equal(rule$lift, 4 / 3)
  expect_equal(rule$count, 2)
  expect_equal(rule$rhs_support, 3 / 4)
  expect_true(r$n_retained <= r$n_generated)
})

test_that("restricting to multi-item baskets changes the denominators", {
  all_b <- mine_rules(build_baskets(fixture, 1)$transactions, 0.25, 0.5)$rules
  multi_b <- mine_rules(build_baskets(fixture, 2)$transactions, 0.25, 0.5, min_lift = 0)$rules
  expect_equal(all_b$support[all_b$LHS == "Scone" & all_b$RHS == "Latte Rg"], 0.5)
  expect_equal(multi_b$support[multi_b$LHS == "Scone" & multi_b$RHS == "Latte Rg"], 1)
})

test_that("thresholds and the lift filter are applied, bad inputs rejected", {
  trans <- build_baskets(fixture)$transactions
  expect_null(mine_rules(trans, support = 0.9, confidence = 0.1)$rules)
  high_lift <- mine_rules(trans, 0.25, 0.5, min_lift = 2)
  expect_null(high_lift$rules)
  expect_gt(high_lift$n_generated, 0)
  expect_error(mine_rules(trans, 0, 0.5), "support")
  expect_error(mine_rules(trans, 0.1, 2), "confidence")
})

test_that("Fisher p-values test positive association", {
  set.seed(42)
  n <- 400
  a <- runif(n) < 0.3
  c_item <- ifelse(a, runif(n) < 0.8, runif(n) < 0.1)
  baskets <- lapply(seq_len(n), function(i) c("filler", if (a[i]) "A", if (c_item[i]) "C"))
  trans <- methods::as(baskets, "transactions")
  r <- mine_rules(trans, 0.01, 0.1)$rules
  rule <- r[r$LHS == "A" & r$RHS == "C", ]
  one_sided <- fisher.test(table(a, c_item), alternative = "greater")$p.value
  expect_lt(rule$p_adjusted, 0.001)
  expect_gte(rule$p_adjusted, one_sided * (1 - 1e-8))  # Holm never lowers a p-value
})
