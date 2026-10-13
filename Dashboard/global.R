# Shared setup. Shiny sources this file once, before ui.R and server.R.
# Logic lives in lib/ so it can be tested without starting the app
# (see tests/). Every chart and KPI goes through the functions there.

library(shiny)
library(shinydashboard)
library(arules)   # loaded before dplyr so dplyr's verbs take precedence
library(plotly)
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)
library(RColorBrewer)
library(DT)

for (f in c("data.R", "metrics.R", "baskets.R", "stats.R", "insights.R", "plots.R")) {
  source(file.path("lib", f), local = TRUE)
}

# *************************Configuration*************************
# COFFEE_DATA_PATH overrides the workbook location (e.g. for a newer extract)
data_file <- Sys.getenv("COFFEE_DATA_PATH", "Coffee Shop Sales.xlsx")
# Bump the version suffix whenever clean_sales() changes shape
cache_file <- "coffee_sales_v3.rds"

# *************************Data*************************
sales_bundle <- load_sales(data_file, cache_file)
df <- sales_bundle$data
quality_table <- sales_bundle$quality

store_locations <- sort(unique(df$store_location))
product_categories <- sort(unique(df$product_category))
date_range <- range(df$date)
store_colors <- make_store_colors(store_locations)
