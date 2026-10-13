# Data loading, validation and feature engineering.
#
# Grain: one row = one line item (a product and a quantity). transaction_id is
# unique per row, so it is NOT an order ID. Baskets are inferred separately in
# baskets.R.

required_columns <- c(
  transaction_id = "numeric", transaction_date = "POSIXct", transaction_time = "POSIXct",
  transaction_qty = "numeric", store_id = "numeric", store_location = "character",
  product_id = "numeric", unit_price = "numeric", product_category = "character",
  product_type = "character", product_detail = "character"
)

weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
size_levels <- c("Small", "Regular", "Large", "Not sized")

# Lines with this many units or more are flagged for review (not removed)
high_qty_threshold <- 4

#' Read the raw workbook and check its schema. The file itself is never modified.
read_raw_sales <- function(path) {
  if (!file.exists(path)) stop("Data file not found: ", path, call. = FALSE)
  raw <- readxl::read_excel(path)
  check_schema(raw)
  raw
}

#' Stop with a clear message if columns are missing or have unexpected types.
check_schema <- function(raw) {
  missing <- setdiff(names(required_columns), names(raw))
  if (length(missing) > 0) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  wrong <- names(required_columns)[!mapply(inherits, raw[names(required_columns)], required_columns)]
  if (length(wrong) > 0) {
    stop("Unexpected column type(s): ", paste(wrong, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Combine the Excel date and time columns into one store-local timestamp.
#'
#' Excel stores times as datetimes on 1899-12-31; only the clock part is kept.
#' UTC is used purely as a fixed container so the viewer's timezone or DST
#' cannot shift the store-local clock times.
parse_timestamp <- function(date, time) {
  # 1899-12-31 00:00 UTC is a whole number of days before 1970-01-01, so the
  # clock time is the number of seconds modulo one day
  seconds <- round(as.numeric(time) %% 86400)
  as.POSIXct(as.Date(date, tz = "UTC")) + seconds
}

#' Key identifying rows that are identical in every column except transaction_id.
duplicate_key <- function(data) {
  do.call(paste, c(unname(as.list(data[setdiff(names(required_columns), "transaction_id")])), sep = "\u001f"))
}

#' Size is the trailing " Lg"/" Rg"/" Sm" token of product_detail. A plain
#' substring match would wrongly tag e.g. "Organic Decaf Blend" as Regular.
parse_size <- function(product_detail) {
  factor(dplyr::case_when(
    grepl(" Lg$", product_detail) ~ "Large",
    grepl(" Rg$", product_detail) ~ "Regular",
    grepl(" Sm$", product_detail) ~ "Small",
    TRUE ~ "Not sized"
  ), levels = size_levels)
}

#' Validate and enrich the raw data.
#'
#' Rows that fail a hard rule (missing key field, unparseable timestamp,
#' non-positive quantity or price) are excluded from the analysis set and
#' counted in the quality report. Suspicious but plausible rows are kept and
#' flagged. Text fields are trimmed; nothing is imputed.
#'
#' @return list(data = cleaned tibble, quality = tibble of checks, excluded = excluded rows)
clean_sales <- function(raw) {
  check_schema(raw)
  text_cols <- c("store_location", "product_category", "product_type", "product_detail")
  whitespace_issues <- sum(vapply(raw[text_cols], function(x) sum(x != trimws(x), na.rm = TRUE), numeric(1)))

  df <- raw %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(text_cols), trimws),
      transaction_datetime = parse_timestamp(transaction_date, transaction_time),
      date = as.Date(transaction_date, tz = "UTC")
    )

  missing_key <- !stats::complete.cases(df[names(required_columns)])
  bad_time <- is.na(df$transaction_datetime)
  bad_qty <- is.na(df$transaction_qty) | df$transaction_qty <= 0 | df$transaction_qty != round(df$transaction_qty)
  bad_price <- is.na(df$unit_price) | df$unit_price <= 0
  exclude <- missing_key | bad_time | bad_qty | bad_price

  excluded <- df[exclude, ]
  df <- df[!exclude, ]
  key <- duplicate_key(df)

  df <- df %>%
    dplyr::mutate(
      line_revenue = unit_price * transaction_qty,
      hour = as.POSIXlt(transaction_datetime)$hour,
      day_of_week = factor(weekday_levels[(as.POSIXlt(transaction_datetime)$wday + 6) %% 7 + 1],
                           levels = weekday_levels),
      is_weekend = day_of_week %in% c("Saturday", "Sunday"),
      month = factor(month.abb[as.POSIXlt(date)$mon + 1], levels = month.abb),
      week_start = date - (as.POSIXlt(date)$wday + 6) %% 7,
      size = parse_size(product_detail),
      basket_id = paste(store_id, as.numeric(transaction_datetime)),
      flag_duplicate_except_id = duplicated(key) | duplicated(key, fromLast = TRUE),
      flag_high_qty = transaction_qty >= high_qty_threshold
    ) %>%
    dplyr::group_by(product_id) %>%
    dplyr::mutate(flag_nonmodal_price = unit_price != modal_value(unit_price)) %>%
    dplyr::ungroup()

  quality <- quality_report(raw, df, list(
    missing_key = missing_key, bad_time = bad_time, bad_qty = bad_qty, bad_price = bad_price
  ), whitespace_issues)

  list(data = df, quality = quality, excluded = excluded)
}

modal_value <- function(x) {
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

#' One row per check: what was tested, how many rows it affected, and what was done.
quality_report <- function(raw, df, hard, whitespace_issues) {
  store_map <- dplyr::distinct(df, store_id, store_location)
  product_map <- dplyr::distinct(df, product_id, product_detail, product_type, product_category)
  dup_rows <- sum(duplicated(duplicate_key(raw)))

  tibble::tribble(
    ~check, ~affected_rows, ~treatment,
    "Rows in source file", nrow(raw), "Raw workbook kept unchanged",
    "Missing values in required columns", sum(hard$missing_key), "Excluded and counted",
    "Unparseable date/time", sum(hard$bad_time), "Excluded and counted",
    "Quantity missing, non-positive or fractional", sum(hard$bad_qty), "Excluded and counted",
    "Unit price missing or non-positive", sum(hard$bad_price), "Excluded and counted",
    "Duplicate transaction_id", sum(duplicated(raw$transaction_id)), "Reported; IDs are not used as order keys",
    "Rows identical to an earlier row except transaction_id", dup_rows,
    "Kept and flagged: may be repeat scans of one item in a basket, or double entry",
    "Leading/trailing whitespace in text fields", whitespace_issues, "Trimmed",
    "Store IDs mapping to more than one store name", nrow(store_map) - dplyr::n_distinct(store_map$store_id), "None expected",
    "Product IDs mapping to more than one name/type/category", nrow(product_map) - dplyr::n_distinct(product_map$product_id), "None expected",
    "Lines priced differently from the product's usual price", sum(df$flag_nonmodal_price),
    "Kept and flagged: price changes, promotions or data artefacts",
    paste0("Lines with quantity >= ", high_qty_threshold), sum(df$flag_high_qty),
    "Kept and flagged: plausible bulk purchases, but several recur at identical clock times",
    "Rows in analysis set", nrow(df), "Used for every chart and KPI"
  )
}

#' Load cleaned data, using an .rds cache that is rebuilt whenever the workbook
#' is newer than the cache or the cache version changes.
load_sales <- function(path, cache_path = NULL) {
  if (!is.null(cache_path) && file.exists(cache_path) && file.mtime(cache_path) >= file.mtime(path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (is.list(cached) && all(c("data", "quality") %in% names(cached))) return(cached)
  }
  result <- clean_sales(read_raw_sales(path))
  if (!is.null(cache_path)) {
    tryCatch(saveRDS(result, cache_path),
             error = function(e) message("Could not write cache (continuing without it): ", conditionMessage(e)))
  }
  result
}

#' Apply the global sidebar filters. Dates are inclusive.
filter_sales <- function(data, stores = NULL, date_from = NULL, date_to = NULL) {
  if (!is.null(stores)) data <- data[data$store_location %in% stores, ]
  if (!is.null(date_from) && !is.na(date_from)) data <- data[data$date >= as.Date(date_from), ]
  if (!is.null(date_to) && !is.na(date_to)) data <- data[data$date <= as.Date(date_to), ]
  data
}
