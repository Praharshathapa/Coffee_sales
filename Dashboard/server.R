# Data, packages, theme and helpers are loaded in global.R

server <- function(input, output, session) {
  # *************************Global Filters*************************
  # Every page works off this one filtered dataset, driven by the sidebar
  sales <- reactive({
    validate(
      need(length(input$stores) > 0, "Select at least one store in the sidebar."),
      need(length(input$dates) == 2 && !anyNA(input$dates), "Choose a start and end date."),
      need(input$dates[1] <= input$dates[2], "The start date must be on or before the end date.")
    )
    data <- filter_sales(df, input$stores, input$dates[1], input$dates[2])
    validate(need(nrow(data) > 0, "No transactions match the selected filters."))
    data
  })

  kpis <- reactive(kpi_summary(sales()))

  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "stores", selected = store_locations)
    updateDateRangeInput(session, "dates", start = date_range[1], end = date_range[2])
  })

  scope_text <- reactive({
    req(length(input$stores) > 0, length(input$dates) == 2, !anyNA(input$dates))
    days <- as.integer(diff(as.Date(input$dates))) + 1
    stores <- if (length(input$stores) == length(store_locations)) "All 3 stores" else paste(input$stores, collapse = ", ")
    tagList(icon("filter"), " ", tags$b(stores), " · ",
            format(as.Date(input$dates[1]), "%d %b %Y"), " to ", format(as.Date(input$dates[2]), "%d %b %Y"),
            paste0(" (", days, " days)"))
  })
  for (id in c("scope_overview", "scope_demand", "scope_products", "scope_baskets")) {
    local({
      out_id <- id
      output[[out_id]] <- renderUI(scope_text())
    })
  }

  # *************************Page 1: Executive Overview*************************
  comparison <- reactive({
    req(length(input$dates) == 2, !anyNA(input$dates))
    period_comparison(df, input$stores, input$dates[1], input$dates[2])
  })

  change_line <- function(metric) {
    cmp <- comparison()
    if (!cmp$available) return(tags$span(class = "kpi-sub", "No equal-length earlier period in the data"))
    tags$span(class = "kpi-sub", paste0(fmt_signed_pct(cmp$change[[metric]]), " vs ",
                                         format(cmp$prior_from, "%d %b"), "–", format(cmp$prior_to, "%d %b")))
  }

  kpi_box <- function(value, label, metric, icon_name, color) {
    valueBox(value, tags$span(label, tags$br(), change_line(metric)), icon = icon(icon_name), color = color)
  }

  output$kpi_revenue <- renderValueBox(kpi_box(fmt_money(kpis()$revenue), "Revenue", "revenue", "chart-line", "green"))
  output$kpi_baskets <- renderValueBox(kpi_box(fmt_int(kpis()$baskets), "Baskets (proxy orders)", "baskets", "receipt", "purple"))
  output$kpi_units <- renderValueBox(kpi_box(fmt_int(kpis()$units), "Units sold", "units", "mug-hot", "light-blue"))
  output$kpi_basket_value <- renderValueBox(
    kpi_box(fmt_money(kpis()$avg_basket_value, 0.01), "Average basket value", "avg_basket_value", "dollar-sign", "yellow")
  )

  output$kpi_footnote <- renderUI({
    k <- kpis()
    tags$p(class = "stat-note",
           fmt_int(k$line_items), " line items · ", sprintf("%.2f", k$avg_units_per_basket), " units per basket · ",
           fmt_money(k$avg_selling_price, 0.01), " average selling price · ",
           fmt_money(k$revenue_per_store_day), " revenue per store-day. ",
           "Baskets group items sold at the same store in the same second; there is no order ID in the data.")
  })

  output$dailyTrendPlot <- renderPlotly({
    daily <- daily_series(sales())
    plot_ly(daily, x = ~date) %>%
      add_lines(y = ~revenue, name = "Daily revenue", line = list(color = accent_light, width = 1.5),
                hovertemplate = "%{x|%a %d %b %Y}<br>%{y:$,.0f}<extra></extra>") %>%
      add_lines(y = ~rolling_7d, name = "Trailing 7-day mean", line = list(color = accent, width = 2.5),
                hovertemplate = "%{x|%a %d %b %Y}<br>7-day mean: %{y:$,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Revenue", tickformat = "$,.0f", rangemode = "tozero")) %>%
      style_plotly()
  })

  output$storeRevenuePlot <- renderPlotly({
    by_store <- add_share(summarise_sales(sales(), store_location))
    plot_ly(by_store, x = ~revenue, y = ~reorder(store_location, revenue), type = "bar", orientation = "h",
            marker = list(color = unname(store_colors[by_store$store_location])),
            text = ~paste0(fmt_money(revenue), " (", fmt_pct(share), ")"), textposition = "auto",
            hovertemplate = "%{y}<br>%{x:$,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Revenue", tickformat = "$,.0s"), yaxis = list(title = "")) %>%
      style_plotly(show_legend = FALSE)
  })

  output$insights <- renderUI({
    items <- build_insights(sales())
    if (length(items) == 0) return(tags$p("The current selection is too small for the summary findings. Widen the date range."))
    tagList(lapply(items, function(f) {
      tags$div(class = "insight",
               tags$h4(f$question),
               tags$p(f$result),
               tags$p(class = "method", tags$b("Method: "), f$method),
               tags$p(class = "caveat", tags$b("Caveat: "), f$caveat))
    }))
  })

  # *************************Page 2: Demand & Timing*************************
  output$heatmapPlot <- renderPlotly({
    info <- metric_info(input$heat_metric)
    heat <- per_day_average(sales(), c("day_of_week", "hour"), input$heat_metric)
    plot_ly(heat, x = ~fmt_hour(hour), y = ~day_of_week, z = ~mean_per_day, type = "heatmap",
            colors = c("#f3e9dc", accent), xgap = 2, ygap = 2,
            hovertemplate = paste0("%{y} %{x}<br>", info$label, " per day: %{z:", info$hover, "}<extra></extra>"),
            colorbar = list(title = "", tickformat = info$tick)) %>%
      layout(xaxis = list(title = "Hour starting"), yaxis = list(title = "", autorange = "reversed")) %>%
      style_plotly(show_legend = FALSE)
  })

  output$peak_summary <- renderUI({
    heat <- per_day_average(sales(), c("day_of_week", "hour"), "revenue")
    busiest <- heat[which.max(heat$mean_per_day), ]
    w <- window_share(sales(), peak_hours)
    tagList(
      tags$p(class = "stat-note", "Busiest slot: ", tags$b(paste(busiest$day_of_week, fmt_hour(busiest$hour))),
             " (", fmt_money(busiest$mean_per_day), " revenue per day)."),
      if (is_estimable(w)) tags$p(class = "stat-note",
        "07:00–10:59 brings in ", tags$b(fmt_pct(w$share)), " of revenue (95% CI ", fmt_pct(w$ci[1]), "–", fmt_pct(w$ci[2]),
        ", bootstrap over days) in 4 of ", w$hours_in_day, " trading hours.")
    )
  })

  output$trendPlot <- renderPlotly({
    info <- metric_info(input$trend_metric)
    trend <- per_day_average(sales(), c(input$trend_grain, "store_location"), input$trend_metric)
    trend$x <- trend[[input$trend_grain]]
    hover_x <- if (input$trend_grain == "week_start") "Week of %{x|%d %b}" else "%{x}"
    plot_ly(trend, x = ~x, y = ~mean_per_day, color = ~store_location, colors = store_colors,
            type = "scatter", mode = "lines+markers", marker = list(size = 8),
            text = ~paste0(days, " trading days"),
            hovertemplate = paste0("%{fullData.name}<br>", hover_x, "<br>", info$label, " per day: %{y:", info$hover,
                                   "}<br>%{text}<extra></extra>")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = paste(info$label, "per day"), tickformat = info$tick,
                                                    rangemode = "tozero")) %>%
      style_plotly()
  })

  output$weekdayPlot <- renderPlotly({
    wk <- per_day_average(sales(), "day_of_week", "revenue")
    wk$type <- ifelse(wk$day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
    plot_ly(wk, x = ~day_of_week, y = ~mean_per_day, color = ~type, colors = c(Weekday = accent, Weekend = "#C08A5B"),
            type = "bar", text = ~paste0(days, " days"),
            hovertemplate = "%{x}<br>%{y:$,.0f} per day<br>%{text}<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Revenue per day", tickformat = "$,.0f"), bargap = 0.3) %>%
      style_plotly()
  })

  output$weekend_note <- renderUI({
    w <- weekend_effect(daily_series(sales()))
    if (!is_estimable(w)) return(attr(w, "reason"))
    paste0("Within-week comparison, adjusted for trend: weekends ", fmt_signed_pct(w$pct_diff),
           " vs weekdays, 95% CI ", fmt_signed_pct(w$ci[1]), " to ", fmt_signed_pct(w$ci[2]), ", p = ", fmt_p(w$p_value), ".")
  })

  output$hourlyPlot <- renderPlotly({
    hourly <- per_day_average(sales(), c("hour", "store_location"), "revenue")
    plot_ly(hourly, x = ~hour, y = ~mean_per_day, color = ~store_location, colors = store_colors,
            type = "scatter", mode = "lines+markers", marker = list(size = 8),
            hovertemplate = "%{fullData.name}<br>%{x}:00<br>%{y:$,.0f} per day<extra></extra>") %>%
      layout(xaxis = list(title = "Hour starting", dtick = 2), yaxis = list(title = "Revenue per day", tickformat = "$,.0f")) %>%
      style_plotly()
  })

  # *************************Page 3: Products & Stores*************************
  ranking_plot <- function(summary, label_col, metric) {
    info <- metric_info(metric)
    summary <- add_share(summary, metric)
    summary$label <- factor(summary[[label_col]], levels = summary[[label_col]][order(summary[[metric]])])
    plot_ly(summary, x = summary[[metric]], y = ~label, type = "bar", orientation = "h", marker = list(color = accent),
            text = ~fmt_pct(share, 1), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = paste0("%{y}<br>%{x:", info$hover, "}<br>%{text} of total<extra></extra>")) %>%
      layout(xaxis = list(title = info$label, tickformat = info$tick), yaxis = list(title = "")) %>%
      style_plotly(show_legend = FALSE)
  }

  output$categoryPlot <- renderPlotly({
    ranking_plot(summarise_sales(sales(), product_category), "product_category", input$product_metric) %>%
      layout(title = list(text = "All categories", font = list(size = 14), x = 0), margin = list(t = 35))
  })

  output$productPlot <- renderPlotly({
    products <- summarise_sales(filter(sales(), product_category == input$product_category), product_detail)
    validate(need(nrow(products) > 0, "No sales for this category in the current selection."))
    ranking_plot(products, "product_detail", input$product_metric) %>%
      layout(title = list(text = paste("Products in", input$product_category, "(share of category)"),
                          font = list(size = 14), x = 0), margin = list(t = 35))
  })

  basket_stats <- reactive(basket_value_by_store(sales()))

  output$store_table <- renderDT({
    by_store <- summarise_sales(sales(), store_location)
    top <- sales() %>% count(store_location, product_detail, wt = transaction_qty) %>%
      group_by(store_location) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
    bv <- basket_stats()$summary
    table <- by_store %>%
      left_join(bv, by = "store_location", suffix = c("", ".bv")) %>%
      left_join(select(top, store_location, top_product = product_detail), by = "store_location") %>%
      add_share() %>%
      transmute(Store = store_location, Revenue = revenue, `Revenue share` = share,
                `Revenue per day` = revenue_per_store_day, Baskets = baskets,
                `Avg basket value` = mean_value,
                `95% CI` = paste0(fmt_money(ci_low, 0.01), "–", fmt_money(ci_high, 0.01)),
                `Units per basket` = units / baskets, `Top product (units)` = top_product)
    datatable(table, rownames = FALSE, options = list(dom = "t", ordering = FALSE)) %>%
      formatCurrency(c("Revenue", "Revenue per day"), digits = 0) %>%
      formatCurrency("Avg basket value", digits = 2) %>%
      formatPercentage("Revenue share", digits = 1) %>%
      formatRound("Baskets", digits = 0) %>%
      formatRound("Units per basket", digits = 2)
  })

  output$store_tests <- renderUI({
    cmp <- store_daily_comparison(sales())
    if (!is_estimable(cmp)) return(tags$p(class = "stat-note", attr(cmp, "reason")))
    rows <- lapply(seq_len(nrow(cmp)), function(i) {
      r <- cmp[i, ]
      tags$li(sprintf("%s vs %s: %s per day (%s; 95%% CI %s to %s), Holm-adjusted p = %s, d_z = %.2f",
                      r$store_a, r$store_b, fmt_money(r$mean_diff), fmt_signed_pct(r$pct_of_b),
                      fmt_money(r$ci_low), fmt_money(r$ci_high), fmt_p(r$p_holm), r$cohens_dz))
    })
    bs <- basket_stats()
    tags$div(class = "stat-note",
             tags$b("Daily revenue, paired by date:"), tags$ul(rows),
             tags$p(tags$b("Basket value: "), "Welch ANOVA p = ", fmt_p(bs$p_value), "; store explains ",
                    fmt_pct(bs$eta_sq, 0.01), " of basket-value variance (eta squared). ",
                    "With tens of thousands of baskets, small differences reach significance; the effect size shows how much they matter."))
  })

  output$storeMixPlot <- renderPlotly({
    mix <- summarise_sales(sales(), store_location, product_category) %>%
      group_by(store_location) %>% mutate(share = revenue / sum(revenue)) %>% ungroup()
    plot_ly(mix, x = ~share, y = ~store_location, color = ~product_category, type = "bar", orientation = "h",
            colors = RColorBrewer::brewer.pal(9, "Set3")[seq_along(unique(mix$product_category))],
            hovertemplate = "%{fullData.name}<br>%{y}: %{x:.1%} of store revenue<extra></extra>") %>%
      layout(barmode = "stack", xaxis = list(title = "", tickformat = ".0%"), yaxis = list(title = "")) %>%
      style_plotly()
  })

  output$sizeMixPlot <- renderPlotly({
    mix <- sales() %>% filter(size != "Not sized") %>% count(product_category, size) %>%
      group_by(product_category) %>% mutate(share = n / sum(n)) %>% ungroup()
    validate(need(nrow(mix) > 0, "No sized products in the current selection."))
    plot_ly(mix, x = ~product_category, y = ~share, color = ~size, colors = size_colors[levels(droplevels(mix$size))],
            type = "bar",
            hovertemplate = "%{fullData.name}<br>%{x}: %{y:.1%}<extra></extra>") %>%
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "", tickformat = ".0%")) %>%
      style_plotly()
  })

  output$size_test_note <- renderUI({
    a <- size_mix_association(sales())
    if (!is_estimable(a)) return(attr(a, "reason"))
    paste0("Does size mix differ by store? Chi-square test on ", fmt_int(a$n), " sized line items: p = ", fmt_p(a$p_value),
           ", Cramér's V = ", sprintf("%.3f", a$cramers_v), " (0 = identical mix). Only beverage lines carry a size.")
  })

  output$pricePlot <- renderPlotly({
    selected <- filter(sales(), product_category == input$price_category)
    validate(need(nrow(selected) > 0, "No sales for this category in the current selection."))
    label <- c(unit_price = "Unit price", line_revenue = "Line revenue", transaction_qty = "Quantity per line")[[input$price_measure]]
    plot <- ggplot(selected, aes(x = size, y = .data[[input$price_measure]], fill = size)) +
      geom_boxplot(outlier.size = 1, outlier.alpha = 0.5, color = "#333333", linewidth = 0.3) +
      facet_wrap(~ store_location) +
      scale_fill_manual(values = size_colors) +
      scale_y_continuous(labels = if (input$price_measure == "transaction_qty") comma else dollar) +
      labs(x = NULL, y = label) +
      theme_dashboard()
    to_plotly(plot, show_legend = FALSE)
  })

  # *************************Page 4: Baskets & Data*************************
  # Baskets depend only on the filters and basket definition; the sliders
  # re-mine rules without rebuilding them
  baskets <- reactive(build_baskets(sales(), as.integer(input$basket_min_items)))
  rules <- reactive(mine_rules(baskets()$transactions, input$min_support, input$min_confidence, input$min_lift))

  output$baskets_box <- renderValueBox({
    b <- baskets()
    valueBox(fmt_int(b$n_eligible), paste("Eligible baskets of", fmt_int(b$n_baskets)),
             icon = icon("shopping-basket"), color = "purple")
  })
  output$multi_item_box <- renderValueBox({
    valueBox(fmt_pct(safe_divide(baskets()$n_multi, baskets()$n_baskets)), "Baskets with 2+ products",
             icon = icon("layer-group"), color = "olive")
  })
  output$rules_generated_box <- renderValueBox({
    valueBox(fmt_int(rules()$n_generated), "Rules meeting support & confidence", icon = icon("filter"), color = "light-blue")
  })
  output$rules_box <- renderValueBox({
    valueBox(fmt_int(rules()$n_retained), "Rules kept (non-redundant, lift filter)", icon = icon("link"), color = "maroon")
  })

  output$rulesPlot <- renderPlotly({
    validate(need(!is.null(rules()$rules), "No rules at these thresholds. Try lowering support, confidence or lift."))
    r <- rules()$rules
    # Marker size encodes basket count, colour encodes lift
    marker_size <- if (nrow(r) > 1 && diff(range(r$count)) > 0) scales::rescale(r$count, c(8, 28)) else 14
    plot_ly(r, x = ~support, y = ~confidence, type = "scatter", mode = "markers",
            marker = list(size = marker_size, color = r$lift, colorscale = list(c(0, "#e6d5c3"), c(1, "#3b2414")),
                          showscale = TRUE, colorbar = list(title = "Lift"), line = list(color = "#ffffff", width = 1)),
            text = ~paste0(LHS, " → ", RHS, "<br>Support: ", fmt_pct(support, 0.01), "<br>Confidence: ", fmt_pct(confidence),
                           "<br>Lift: ", sprintf("%.2f", lift), "<br>Baskets: ", fmt_int(count)),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Support (share of eligible baskets)", tickformat = ".1%"),
             yaxis = list(title = "Confidence", tickformat = ".0%")) %>%
      style_plotly(show_legend = FALSE)
  })

  output$rules_table <- renderDT({
    validate(need(!is.null(rules()$rules), "No rules at these thresholds."))
    datatable(
      rules()$rules %>% transmute(`If basket contains` = LHS, `It also contains` = RHS, Baskets = count,
                                  Support = support, Confidence = confidence, `Consequent share` = rhs_support,
                                  Lift = lift, `Adj. p (Fisher, Holm)` = p_adjusted),
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(6, "desc")))
    ) %>%
      formatPercentage(c("Support", "Consequent share"), digits = 2) %>%
      formatPercentage("Confidence", digits = 1) %>%
      formatRound("Lift", digits = 2) %>%
      formatSignif("Adj. p (Fisher, Holm)", digits = 2)
  })

  explorer_data <- reactive({
    sales() %>%
      transmute(`Transaction ID` = transaction_id,
                `Date & Time` = format(transaction_datetime, "%Y-%m-%d %H:%M:%S"),
                Store = store_location, Category = product_category, Type = product_type,
                Product = product_detail, Size = size, Quantity = transaction_qty,
                `Unit Price` = unit_price, `Line Revenue` = line_revenue,
                `Basket ID` = basket_id,
                `Flag: duplicate except ID` = flag_duplicate_except_id,
                `Flag: qty 4+` = flag_high_qty,
                `Flag: non-standard price` = flag_nonmodal_price)
  })

  output$data_table <- renderDT({
    datatable(explorer_data(), filter = "top", rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatCurrency(c("Unit Price", "Line Revenue"))
  })

  # Rows passing the sidebar filters and the table's own column/search filters
  export_rows <- reactive({
    rows <- input$data_table_rows_all
    data <- explorer_data()
    if (is.null(rows)) data else data[rows, ]
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("coffee_sales_", input$dates[1], "_to_", input$dates[2], ".csv"),
    content = function(file) write.csv(export_rows(), file, row.names = FALSE)
  )

  output$quality_table <- renderDT({
    datatable(quality_table, rownames = FALSE, colnames = c("Check", "Rows affected", "Treatment"),
              options = list(dom = "t", ordering = FALSE, pageLength = 50)) %>%
      formatRound("affected_rows", digits = 0)
  })

  output$metric_table <- renderDT({
    datatable(metric_dictionary, rownames = FALSE, options = list(dom = "t", ordering = FALSE, pageLength = 50))
  })
}
