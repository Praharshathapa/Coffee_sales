suppressPackageStartupMessages({
  library(dplyr)
  library(arules)
})

app_dir <- normalizePath(file.path(testthat::test_path(), "..", "..", "Dashboard"))
for (f in c("data.R", "metrics.R", "baskets.R", "stats.R", "insights.R", "plots.R")) {
  source(file.path(app_dir, "lib", f), local = FALSE)
}

# Build a raw table shaped like the workbook (Excel time on 1899-12-31)
make_raw <- function(rows) {
  tibble::tibble(
    transaction_id = as.numeric(seq_len(nrow(rows))),
    transaction_date = as.POSIXct(rows$date, tz = "UTC"),
    transaction_time = as.POSIXct(paste("1899-12-31", rows$time), tz = "UTC"),
    transaction_qty = as.numeric(rows$qty),
    store_id = as.numeric(c(A = 1, B = 2)[rows$store]),
    store_location = unname(c(A = "Alpha", B = "Beta")[rows$store]),
    product_id = as.numeric(match(rows$product, unique(rows$product))),
    unit_price = as.numeric(rows$price),
    product_category = rows$category,
    product_type = rows$category,
    product_detail = rows$product
  )
}

# Hand-checkable fixture: 7 lines, 2 stores, 2 days, 4 baskets
#   basket 1 (A, d1 08:00:00): Latte Rg x2 @3, Scone x1 @3.5           -> 9.50
#   basket 2 (A, d1 08:05:00): Latte Rg x1 @3                           -> 3.00
#   basket 3 (B, d1 08:00:00): Latte Rg x1 @3, Scone @3.5 on two lines  -> 10.00
#   basket 4 (B, d2 15:30:10): Tea Lg x3 @2.5                           -> 7.50
fixture_rows <- tibble::tribble(
  ~date,        ~time,      ~store, ~product,   ~category, ~qty, ~price,
  "2023-01-02", "08:00:00", "A",    "Latte Rg", "Coffee",  2,    3,
  "2023-01-02", "08:00:00", "A",    "Scone",    "Bakery",  1,    3.5,
  "2023-01-02", "08:05:00", "A",    "Latte Rg", "Coffee",  1,    3,
  "2023-01-02", "08:00:00", "B",    "Latte Rg", "Coffee",  1,    3,
  "2023-01-02", "08:00:00", "B",    "Scone",    "Bakery",  1,    3.5,
  "2023-01-02", "08:00:00", "B",    "Scone",    "Bakery",  1,    3.5,
  "2023-01-03", "15:30:10", "B",    "Tea Lg",   "Tea",     3,    2.5
)
fixture_raw <- make_raw(fixture_rows)
fixture <- clean_sales(fixture_raw)$data

# Longer synthetic daily data for the statistical functions
make_daily_sales <- function(days = 84, growth = 0.01, weekend_lift = 0, seed = 1) {
  set.seed(seed)
  dates <- seq(as.Date("2023-01-02"), by = "day", length.out = days)  # starts on a Monday
  weekend <- as.POSIXlt(dates)$wday %in% c(0, 6)
  revenue <- 1000 * exp(growth * seq_along(dates)) * (1 + weekend_lift * weekend) * exp(rnorm(days, 0, 0.05))
  tibble::tibble(date = dates, revenue = revenue)
}
