# Shared chart styling and formatting. Colours were checked with a
# colour-vision-deficiency validator: the store palette (Dark2) passes the
# categorical checks; sizes use a single-hue light-to-dark ramp because size
# is ordered.

font_family <- "Helvetica, Arial, sans-serif"
accent <- "#6F4E37"
accent_light <- "#d8c3ae"

size_colors <- c(Small = "#C08A5B", Regular = "#8B5A35", Large = "#4E2E17", `Not sized` = "#9E9E9E")

make_store_colors <- function(stores) stats::setNames(RColorBrewer::brewer.pal(3, "Dark2")[seq_along(stores)], stores)

fmt_money <- function(x, accuracy = 1) ifelse(is.na(x), "n/a", scales::dollar(x, accuracy = accuracy))
fmt_pct <- function(x, accuracy = 0.1) ifelse(is.na(x), "n/a", scales::percent(x, accuracy = accuracy))
fmt_int <- function(x) ifelse(is.na(x), "n/a", scales::comma(x, accuracy = 1))
fmt_signed_pct <- function(x, accuracy = 0.1) {
  ifelse(is.na(x), "n/a", paste0(ifelse(x >= 0, "+", ""), scales::percent(x, accuracy = accuracy)))
}
fmt_p <- function(p) ifelse(is.na(p), "p = n/a", ifelse(p < 0.001, "p < 0.001", paste("p =", formatC(p, format = "f", digits = 3))))
fmt_hour <- function(h) sprintf("%02d:00", h)

#' Label and axis format for a metric key used across the app.
metric_info <- function(metric) {
  switch(metric,
    revenue = list(label = "Revenue", tick = "$~s", hover = "$,.0f"),
    units = list(label = "Units sold", tick = ",.0f", hover = ",.0f"),
    baskets = list(label = "Baskets (proxy orders)", tick = ",.0f", hover = ",.0f"),
    stop("Unknown metric: ", metric, call. = FALSE)
  )
}

theme_dashboard <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "Helvetica") +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e3e0dc"),
      axis.text = ggplot2::element_text(color = "#444444"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

#' Common plotly layout: legend above the plot, recessive grid, trimmed toolbar.
#' Chart titles live in the surrounding box header, not inside the plot.
style_plotly <- function(p, show_legend = TRUE) {
  p %>%
    plotly::layout(
      font = list(family = font_family, color = "#333333"),
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom", title = list(text = "")),
      showlegend = show_legend,
      margin = list(t = if (show_legend) 40 else 10, r = 10),
      hoverlabel = list(font = list(family = font_family)),
      xaxis = list(gridcolor = "#eeeeee", zeroline = FALSE),
      yaxis = list(gridcolor = "#e3e0dc", zeroline = FALSE)
    ) %>%
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverClosestCartesian",
                                              "hoverCompareCartesian", "toggleSpikelines"))
}

to_plotly <- function(plot, show_legend = TRUE, ...) {
  plotly::ggplotly(plot, ...) %>% style_plotly(show_legend)
}

# Category-mix chart: five largest categories by overall revenue plus "Other".
# Hues are the first five slots of a CVD-validated categorical palette;
# segments carry direct % labels because three slots are below 3:1 contrast.
mix_categories <- c("Coffee", "Tea", "Bakery", "Drinking Chocolate", "Coffee beans")
mix_colors <- c(Coffee = "#2a78d6", Tea = "#eb6834", Bakery = "#1baf7a", `Drinking Chocolate` = "#eda100",
                `Coffee beans` = "#e87ba4", Other = "#5c5c5c")
