test_that("schema check rejects missing and mistyped columns", {
  expect_error(check_schema(dplyr::select(fixture_raw, -unit_price)), "unit_price")
  bad <- fixture_raw
  bad$unit_price <- as.character(bad$unit_price)
  expect_error(check_schema(bad), "unit_price")
  expect_true(check_schema(fixture_raw))
})

test_that("timestamps combine date and Excel time without timezone shifts", {
  ts <- parse_timestamp(fixture_raw$transaction_date, fixture_raw$transaction_time)
  expect_equal(format(ts[7], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2023-01-03 15:30:10")
  expect_equal(attr(ts, "tzone"), "UTC")
  expect_equal(fixture$hour, c(8, 8, 8, 8, 8, 8, 15))
})

test_that("derived calendar fields are correct", {
  expect_equal(as.character(fixture$day_of_week[1]), "Monday")   # 2023-01-02
  expect_equal(fixture$week_start[7], as.Date("2023-01-02"))
  expect_false(any(fixture$is_weekend))
  expect_equal(as.character(fixture$month[1]), "Jan")
  sunday <- clean_sales(make_raw(dplyr::mutate(fixture_rows[1, ], date = "2023-01-08")))$data
  expect_equal(as.character(sunday$day_of_week), "Sunday")
  expect_true(sunday$is_weekend)
  expect_equal(sunday$week_start, as.Date("2023-01-02"))
})

test_that("size parsing uses the trailing token only", {
  s <- parse_size(c("Latte Rg", "Tea Lg", "Ethiopia Sm", "Organic Decaf Blend", "Lg Special", "Scone"))
  expect_equal(as.character(s), c("Regular", "Large", "Small", "Not sized", "Not sized", "Not sized"))
})

test_that("invalid rows are excluded and counted, never silently dropped", {
  raw <- fixture_raw
  raw$transaction_qty[1] <- 0
  raw$unit_price[2] <- -1
  raw$product_detail[3] <- NA
  raw$transaction_qty[4] <- 1.5
  res <- clean_sales(raw)
  expect_equal(nrow(res$data), nrow(raw) - 4)
  expect_equal(nrow(res$excluded), 4)
  q <- setNames(res$quality$affected_rows, res$quality$check)
  expect_equal(q[["Quantity missing, non-positive or fractional"]], 2)
  expect_equal(q[["Unit price missing or non-positive"]], 1)
  expect_equal(q[["Missing values in required columns"]], 1)
  expect_equal(q[["Rows in analysis set"]], nrow(raw) - 4)
})

test_that("rows identical except transaction_id are flagged but kept", {
  res <- clean_sales(fixture_raw)
  expect_equal(nrow(res$data), 7)
  expect_equal(which(res$data$flag_duplicate_except_id), c(5, 6))
  q <- setNames(res$quality$affected_rows, res$quality$check)
  expect_equal(q[["Rows identical to an earlier row except transaction_id"]], 1)
})

test_that("whitespace is trimmed and reported", {
  raw <- fixture_raw
  raw$product_detail[3] <- "Latte Rg "
  res <- clean_sales(raw)
  expect_equal(res$data$product_detail[3], "Latte Rg")
  expect_equal(res$quality$affected_rows[res$quality$check == "Leading/trailing whitespace in text fields"], 1)
})

test_that("high quantity and non-modal prices are flagged", {
  raw <- fixture_raw
  raw$transaction_qty[7] <- 8
  raw$unit_price[3] <- 4  # Latte Rg is usually 3
  d <- clean_sales(raw)$data
  expect_equal(which(d$flag_high_qty), 7)
  expect_equal(which(d$flag_nonmodal_price), 3)
})

test_that("filters are inclusive and handle empty results", {
  expect_equal(nrow(filter_sales(fixture, "Alpha")), 3)
  expect_equal(nrow(filter_sales(fixture, NULL, "2023-01-03", "2023-01-03")), 1)
  expect_equal(nrow(filter_sales(fixture, c("Alpha", "Beta"), "2023-01-02", "2023-01-03")), 7)
  expect_equal(nrow(filter_sales(fixture, "Gamma")), 0)
  expect_equal(nrow(filter_sales(fixture, NULL, NA, NA)), 7)
})

test_that("load_sales reads a fresh cache and rebuilds a stale or invalid one", {
  src <- tempfile(fileext = ".xlsx")
  file.create(src)
  cache <- tempfile(fileext = ".rds")
  saveRDS(list(data = fixture, quality = tibble::tibble()), cache)
  Sys.setFileTime(cache, Sys.time() + 3600)
  expect_equal(nrow(load_sales(src, cache)$data), 7)
  # A stale cache triggers a re-read of the (here unreadable) source
  Sys.setFileTime(cache, Sys.time() - 3600)
  expect_error(load_sales(src, cache))
  expect_error(load_sales(tempfile(fileext = ".xlsx")), "not found")
})
