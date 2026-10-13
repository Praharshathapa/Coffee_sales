# Shared setup for the dashboard. Shiny sources this file once, before ui.R
# and server.R, so data loading and helpers live here instead of being
# duplicated in both files.

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

# *************************Data Loading & Cleaning*************************
# Parsing the 9 MB workbook is slow, so the cleaned data is cached as .rds
# and rebuilt only when the workbook is newer than the cache
data_file <- "Coffee Shop Sales.xlsx"
# Bump the version suffix whenever load_sales_data() changes shape
cache_file <- "coffee_sales_v2.rds"

load_sales_data <- function(path) {
  df <- read_excel(path)

  # Excel stores times as 1899-12-31 datetimes; keep only the time part
  df$transaction_time <- sub("1899-12-31 ", "", df$transaction_time)
  datetime_string <- paste(df$transaction_date, df$transaction_time)
  # Store-local clock times; UTC avoids shifts from the viewer's timezone or DST
  df$transaction_datetime <- as.POSIXct(datetime_string, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  df %>%
    mutate(
      date = as.Date(transaction_date),
      hour_of_day = format(transaction_datetime, "%H"),
      month = factor(month.abb[as.integer(format(transaction_datetime, "%m"))], levels = month.abb),
      # Locale-independent weekday names, ordered Monday..Sunday
      day_of_week = factor(
        c("Sunday", weekday_levels[1:6])[as.POSIXlt(transaction_datetime)$wday + 1],
        levels = weekday_levels
      ),
      Total_cost = unit_price * transaction_qty,
      # Size is the trailing "Lg"/"Rg"/"Sm" token; a plain substring match would
      # wrongly tag e.g. "Organic Decaf Blend" as Regular
      size = factor(case_when(
        grepl(" Lg$", product_detail) ~ "Large",
        grepl(" Rg$", product_detail) ~ "Regular",
        grepl(" Sm$", product_detail) ~ "Small",
        TRUE ~ "Others"
      ), levels = c("Small", "Regular", "Large", "Others"))
    )
}

weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

if (file.exists(cache_file) && file.mtime(cache_file) >= file.mtime(data_file)) {
  df <- readRDS(cache_file)
} else {
  df <- load_sales_data(data_file)
  try(saveRDS(df, cache_file), silent = TRUE)
}

store_locations <- sort(unique(df$store_location))
product_categories <- sort(unique(df$product_category))
date_range <- range(df$date)

# *************************Theme For the plots*************************
bbc_style <- function() {
  font <- "Helvetica"

  ggplot2::theme(
    # Text format
    plot.title = ggplot2::element_text(family = font, size = 16, face = "bold", color = "#222222",
                                       hjust = 0.5, margin = ggplot2::margin(15, 0, 10, 0)),
    plot.subtitle = ggplot2::element_text(family = font, size = 13, margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_blank(),

    # Legend format
    legend.position = "top",
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = 10, color = "#222222"),

    # Axis format
    axis.text = ggplot2::element_text(family = font, size = 10, color = "#222222"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10), angle = 45, hjust = 1),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "#cbcbcb", linewidth = 0.5),

    # Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linetype = "dashed"),
    panel.grid.major.x = ggplot2::element_blank(),

    # Blank background
    panel.background = ggplot2::element_rect(fill = "white", color = NA),

    # Strip background
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 13, hjust = 0)
  )
}

# Palette: one colour per store used consistently across every chart, plus
# coffee tones for sizes
store_colors <- setNames(brewer.pal(3, "Dark2"), store_locations)
size_colors <- c(Large = "#6F4E37", Regular = "#C8A27A", Small = "#4A9C8C", Others = "#9E9E9E")

# Horizontal axis labels for short categories (hours, months, store names)
flat_x_labels <- function() theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Shared plotly look: centred title, horizontal legend between title and plot
# (ggplotly otherwise stacks them on top of each other), trimmed toolbar
style_plotly <- function(p, title = NULL, show_legend = TRUE) {
  p %>%
    layout(
      title = list(text = title, x = 0.5, xanchor = "center", y = 0.98, yref = "container", yanchor = "top",
                   font = list(family = "Helvetica", size = 18, color = "#222222")),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02, yanchor = "bottom",
                    title = list(text = "")),
      showlegend = show_legend,
      margin = list(t = if (show_legend) 90 else 65),
      hoverlabel = list(font = list(family = "Helvetica"))
    ) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverClosestCartesian",
                                      "hoverCompareCartesian", "toggleSpikelines"))
}

# Convert a ggplot, moving its title into the plotly layout
to_plotly <- function(plot, show_legend = TRUE, ...) {
  title <- plot$labels$title
  ggplotly(plot + labs(title = NULL), ...) %>% style_plotly(title, show_legend)
}

# *************************Helpers*************************
# Orders and revenue grouped by the given columns
summarise_sales <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      total_orders = sum(transaction_qty),
      total_sales = sum(Total_cost),
      .groups = "drop"
    )
}

# Map the "Total Orders"/"Total Sales" selector to a column name
metric_column <- function(metric) {
  if (metric == "Total Orders") "total_orders" else "total_sales"
}

# Line or lollipop chart of a metric over a time dimension, one series per store
trend_plot <- function(data, x, metric, plot_type, x_label) {
  y <- metric_column(metric)
  title <- paste(if (metric == "Total Orders") "Order" else "Sales", "Trends by", x_label)

  plot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], color = store_location, group = store_location))
  plot <- if (plot_type == "Line Plot") {
    plot + geom_line() + geom_point()
  } else {
    plot +
      geom_segment(aes(xend = .data[[x]], y = 0, yend = .data[[y]]), position = position_dodge(width = 0.6)) +
      geom_point(position = position_dodge(width = 0.6))
  }

  plot +
    labs(x = x_label, y = metric, title = title) +
    scale_color_manual(values = store_colors) +
    scale_y_continuous(labels = if (y == "total_sales") dollar else comma) +
    bbc_style() +
    flat_x_labels()
}

# Market-basket analysis. Every row has a unique transaction_id, so items rung
# up at the same store at the exact same second are treated as one basket.
build_baskets <- function(data) {
  baskets <- split(data$product_detail, paste(data$store_location, data$transaction_datetime))
  baskets <- lapply(baskets, unique)
  multi_item <- baskets[lengths(baskets) > 1]
  list(
    n_baskets = length(baskets),
    n_multi = length(multi_item),
    transactions = if (length(multi_item) > 0) as(multi_item, "transactions") else NULL
  )
}

mine_rules <- function(transactions, support, confidence) {
  if (is.null(transactions)) return(NULL)
  rules <- apriori(transactions,
                   parameter = list(supp = support, conf = confidence, minlen = 2, maxlen = 3),
                   control = list(verbose = FALSE))
  if (length(rules) == 0) return(NULL)
  rules <- rules[!is.redundant(rules)]
  DATAFRAME(rules, separate = TRUE, setStart = "", setEnd = "") %>%
    mutate(across(c(LHS, RHS), as.character)) %>%
    arrange(desc(lift))
}
