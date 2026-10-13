# Integration tests against the real app and data via shiny::testServer.
skip_if_not(file.exists(file.path(app_dir, "Coffee Shop Sales.xlsx")), "dataset not available")

app <- shiny::shinyAppDir(app_dir)

test_that("full-data KPIs reconcile with the raw workbook", {
  raw <- readxl::read_excel(file.path(app_dir, "Coffee Shop Sales.xlsx"))
  shiny::testServer(app, {
    session$setInputs(stores = store_locations, dates = date_range)
    k <- kpis()
    expect_equal(k$revenue, sum(raw$unit_price * raw$transaction_qty))
    expect_equal(k$units, sum(raw$transaction_qty))
    expect_equal(k$line_items, nrow(raw))
    expect_equal(k$baskets, nrow(dplyr::distinct(raw, store_id, transaction_date, transaction_time)))
  })
})

test_that("store totals add up to the overall total within a filter", {
  shiny::testServer(app, {
    session$setInputs(stores = store_locations, dates = as.Date(c("2023-03-01", "2023-03-31")))
    by_store <- summarise_sales(sales(), store_location)
    expect_equal(sum(by_store$revenue), kpis()$revenue)
    expect_true(all(sales()$date >= as.Date("2023-03-01") & sales()$date <= as.Date("2023-03-31")))
  })
})

test_that("invalid filters give validation messages rather than errors", {
  shiny::testServer(app, {
    session$setInputs(stores = character(0), dates = date_range)
    expect_error(sales(), class = "shiny.silent.error")
    session$setInputs(stores = store_locations, dates = as.Date(c("2023-05-01", "2023-04-01")))
    expect_error(sales(), class = "shiny.silent.error")
  })
})

test_that("CSV export matches the filtered rows", {
  shiny::testServer(app, {
    session$setInputs(stores = "Astoria", dates = as.Date(c("2023-02-01", "2023-02-07")))
    exported <- read.csv(output$download_data, check.names = FALSE)
    expected <- filter_sales(df, "Astoria", "2023-02-01", "2023-02-07")
    expect_equal(nrow(exported), nrow(expected))
    expect_equal(sum(exported$`Line Revenue`), sum(expected$line_revenue))
    expect_setequal(exported$`Transaction ID`, expected$transaction_id)
    # Column filters in the table narrow the export to the rows shown
    session$setInputs(data_table_rows_all = 1:5)
    exported_subset <- read.csv(output$download_data, check.names = FALSE)
    expect_equal(nrow(exported_subset), 5)
  })
})

test_that("basket rules react to the basket definition", {
  shiny::testServer(app, {
    session$setInputs(stores = store_locations, dates = date_range, basket_min_items = "1",
                      min_support = 0.002, min_confidence = 0.1, min_lift = 1)
    all_rules <- rules()
    expect_equal(baskets()$n_eligible, baskets()$n_baskets)
    session$setInputs(basket_min_items = "2")
    expect_equal(baskets()$n_eligible, baskets()$n_multi)
    expect_gt(max(rules()$rules$support), max(all_rules$rules$support))
  })
})

test_that("every output renders without error on the default selection", {
  shiny::testServer(app, {
    session$setInputs(stores = store_locations, dates = date_range, heat_metric = "revenue",
                      trend_metric = "revenue", trend_grain = "week_start", product_metric = "revenue",
                      product_category = "Coffee", price_category = "Coffee", price_measure = "unit_price",
                      basket_min_items = "1", min_support = 0.002, min_confidence = 0.1, min_lift = 1)
    for (id in c("kpi_revenue", "kpi_baskets", "kpi_units", "kpi_basket_value", "kpi_footnote", "dailyTrendPlot",
                 "storeRevenuePlot", "insights", "heatmapPlot", "peak_summary", "trendPlot", "weekdayPlot",
                 "weekend_note", "hourlyPlot", "categoryPlot", "productPlot", "store_table", "store_tests",
                 "storeMixPlot", "sizeMixPlot", "size_test_note", "pricePlot", "baskets_box", "multi_item_box",
                 "rules_generated_box", "rules_box", "rulesPlot", "rules_table", "data_table", "quality_table",
                 "metric_table", "scope_overview")) {
      expect_no_error(output[[id]], message = id)
    }
    session$setInputs(trend_grain = "month", heat_metric = "baskets")
    expect_no_error(output$trendPlot)
    expect_no_error(output$heatmapPlot)
  })
})

test_that("a one-store, one-week selection still renders", {
  shiny::testServer(app, {
    session$setInputs(stores = "Hell's Kitchen", dates = as.Date(c("2023-06-01", "2023-06-07")),
                      basket_min_items = "1", min_support = 0.002, min_confidence = 0.1, min_lift = 1,
                      trend_metric = "units", trend_grain = "week_start")
    expect_no_error(output$insights)
    expect_no_error(output$store_tests)
    expect_no_error(output$trendPlot)
    expect_no_error(output$rules_box)
  })
})
