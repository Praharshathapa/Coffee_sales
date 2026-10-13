# Data, packages, theme and helpers are loaded in global.R

server <- function(input, output, session) {
  # *************************Global Filters*************************
  # Every tab works off this one filtered dataset, driven by the sidebar
  sales <- reactive({
    validate(need(length(input$stores) > 0, "Select at least one store in the sidebar."))
    data <- df %>%
      filter(store_location %in% input$stores,
             date >= input$dates[1], date <= input$dates[2])
    validate(need(nrow(data) > 0, "No transactions match the selected filters."))
    data
  })

  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "stores", selected = store_locations)
    updateDateRangeInput(session, "dates", start = date_range[1], end = date_range[2])
  })

  # *************************KPIs*************************
  output$total_sales_box <- renderValueBox({
    valueBox(dollar(sum(sales()$Total_cost)), "Total revenue", icon = icon("chart-line"), color = "green")
  })

  output$transactions_box <- renderValueBox({
    valueBox(comma(nrow(sales())), "Transactions", icon = icon("receipt"), color = "purple")
  })

  output$avg_transaction_box <- renderValueBox({
    valueBox(dollar(mean(sales()$Total_cost), accuracy = 0.01), "Average transaction value",
             icon = icon("dollar-sign"), color = "yellow")
  })

  output$top_product_box <- renderValueBox({
    top_product <- sales() %>%
      count(product_detail, wt = transaction_qty, sort = TRUE) %>%
      slice(1) %>%
      pull(product_detail)
    valueBox(tags$span(top_product, style = "font-size: 24px;"), "Best-selling product (units)",
             icon = icon("mug-hot"), color = "light-blue")
  })

  # *************************Overview Charts*************************
  output$dailyTrendPlot <- renderPlotly({
    daily <- summarise_sales(sales(), date) %>%
      arrange(date) %>%
      mutate(rolling_avg = as.numeric(stats::filter(total_sales, rep(1 / 7, 7), sides = 1)))

    plot_ly(daily, x = ~date) %>%
      add_lines(y = ~total_sales, name = "Daily revenue", line = list(color = "#c8b6a6"),
                hovertemplate = "%{x}<br>%{y:$,.0f}<extra></extra>") %>%
      add_lines(y = ~rolling_avg, name = "7-day average", line = list(color = "#6F4E37", width = 3),
                hovertemplate = "%{x}<br>7-day avg: %{y:$,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Revenue", tickformat = "$,.0f")) %>%
      style_plotly("Daily Revenue")
  })

  output$storeSharePlot <- renderPlotly({
    location_sales <- summarise_sales(sales(), store_location)
    plot_ly(location_sales, labels = ~store_location, values = ~total_sales, type = "pie", hole = 0.5,
            marker = list(colors = unname(store_colors[location_sales$store_location])),
            textinfo = "percent", hovertemplate = "%{label}<br>%{value:$,.0f}<extra></extra>") %>%
      style_plotly("Revenue Share by Store") %>%
      layout(legend = list(y = -0.05, yanchor = "top"), margin = list(t = 60))
  })

  # *************************Time Trends*************************
  output$hourlySalesPlot <- renderPlotly({
    hourly_sales <- summarise_sales(sales(), hour_of_day, store_location)
    to_plotly(trend_plot(hourly_sales, "hour_of_day", input$hourly_sales_type,
                        input$hourly_sales_plot_type, "Hour of the Day"))
  })

  output$weeklySalesPlot <- renderPlotly({
    sales_by_weekday <- summarise_sales(sales(), day_of_week, store_location)
    to_plotly(trend_plot(sales_by_weekday, "day_of_week", input$weekly_sales_type,
                        input$weekly_sales_plot_type, "Day of the Week"))
  })

  output$monthlySalesPlot <- renderPlotly({
    sales_by_month <- summarise_sales(sales(), month, store_location)
    to_plotly(trend_plot(sales_by_month, "month", input$monthly_sales_type,
                        input$monthly_sales_plot_type, "Month"))
  })

  # Hour x weekday heatmap, useful for staffing decisions
  output$peakHoursPlot <- renderPlotly({
    y <- metric_column(input$peak_metric)
    heat <- summarise_sales(sales(), day_of_week, hour_of_day)

    plot <- ggplot(heat, aes(x = hour_of_day, y = day_of_week, fill = .data[[y]],
                             text = paste0(day_of_week, " ", hour_of_day, ":00<br>", input$peak_metric, ": ",
                                           if (y == "total_sales") dollar(.data[[y]]) else comma(.data[[y]])))) +
      geom_tile(color = "white") +
      scale_y_discrete(limits = rev(weekday_levels)) +
      scale_fill_gradient(low = "#f3e9dc", high = "#6F4E37", labels = if (y == "total_sales") dollar else comma) +
      labs(x = "Hour of the Day", y = NULL, fill = NULL, title = paste(input$peak_metric, "by Weekday and Hour")) +
      bbc_style() +
      theme(legend.position = "right", axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.grid.major.y = element_blank())

    to_plotly(plot, show_legend = FALSE, tooltip = "text")
  })

  output$peak_summary <- renderUI({
    busiest <- summarise_sales(sales(), day_of_week, hour_of_day) %>% slice_max(total_sales, n = 1)
    hourly <- summarise_sales(sales(), hour_of_day)
    morning_share <- sum(hourly$total_sales[hourly$hour_of_day %in% c("07", "08", "09", "10")]) / sum(hourly$total_sales)
    tags$p(
      "Busiest slot: ", tags$b(paste0(busiest$day_of_week, " at ", busiest$hour_of_day, ":00")),
      paste0(" (", dollar(busiest$total_sales), "). "),
      "The 7–10 AM window brings in ", tags$b(percent(morning_share, accuracy = 0.1)), " of revenue."
    )
  })

  # *************************Product Category*************************
  output$categorySalesPlot <- renderPlotly({
    sales_by_category <- summarise_sales(sales(), product_category)
    y <- metric_column(input$category_sales_type)
    title <- paste(input$category_sales_type, "by Product Category")

    if (input$category_sales_plot_type == "Bar Plot") {
      plot <- ggplot(sales_by_category, aes(x = reorder(product_category, .data[[y]]), y = .data[[y]])) +
        geom_col(fill = "#6F4E37") +
        labs(x = NULL, y = input$category_sales_type, title = title) +
        bbc_style() +
        flat_x_labels() +
        theme(legend.position = "none") +
        coord_flip() +
        scale_y_continuous(labels = if (y == "total_sales") dollar else comma)
      to_plotly(plot, show_legend = FALSE)
    } else {
      plot_ly(
        sales_by_category,
        type = "treemap",
        labels = ~product_category,
        parents = "",
        values = sales_by_category[[y]],
        branchvalues = "total",
        textinfo = "label+value+percent root",
        marker = list(
          colors = brewer.pal(n = 8, name = "Set2"),
          line = list(color = "white", width = 1)
        )
      ) %>%
        layout(font = list(family = "Helvetica", size = 14)) %>%
        style_plotly(title, show_legend = FALSE)
    }
  })

  # *************************Store Performance*************************
  output$locationSalesPlot <- renderPlotly({
    location_sales <- summarise_sales(sales(), store_location)
    y <- metric_column(input$location_sales_type)
    title <- paste(input$location_sales_type, "by Store Location")

    if (input$location_sales_plot_type == "Bar Plot") {
      plot <- ggplot(location_sales, aes(x = store_location, y = .data[[y]], fill = store_location)) +
        geom_col() +
        labs(x = "Store Location", y = input$location_sales_type, title = title) +
        scale_fill_manual(values = store_colors) +
        scale_y_continuous(labels = if (y == "total_sales") dollar else comma) +
        bbc_style() +
        flat_x_labels() +
        theme(legend.position = "none")
      to_plotly(plot, show_legend = FALSE)
    } else {
      plot_ly(location_sales, labels = ~store_location, values = location_sales[[y]], type = "pie",
              marker = list(colors = unname(store_colors[location_sales$store_location])),
              textinfo = "label+percent", insidetextorientation = "radial") %>%
        style_plotly(title, show_legend = FALSE)
    }
  })

  output$store_table <- renderDT({
    store_summary <- sales() %>%
      group_by(Store = store_location) %>%
      summarise(
        Revenue = sum(Total_cost),
        Transactions = n(),
        `Units Sold` = sum(transaction_qty),
        `Avg Transaction` = mean(Total_cost),
        `Top Product` = names(which.max(tapply(transaction_qty, product_detail, sum))),
        .groups = "drop"
      )
    datatable(store_summary, rownames = FALSE, options = list(dom = "t")) %>%
      formatCurrency(c("Revenue", "Avg Transaction")) %>%
      formatRound(c("Transactions", "Units Sold"), digits = 0)
  })

  # *************************Size Mix*************************
  output$orderSizePlot <- renderPlotly({
    y <- metric_column(input$size_mix_metric)
    size_mix <- sales() %>%
      filter(size != "Others") %>%
      summarise_sales(product_category, size)
    validate(need(nrow(size_mix) > 0, "No sized products in the current selection."))

    plot <- ggplot(size_mix, aes(x = product_category, y = .data[[y]], fill = size)) +
      geom_col(position = if (input$size_mix_share) "fill" else "stack") +
      scale_fill_manual(values = size_colors) +
      scale_y_continuous(labels = if (input$size_mix_share) percent else comma) +
      labs(x = "Product Category", y = if (input$size_mix_share) "Share" else input$size_mix_metric,
           title = paste("Size Mix by Category:", input$size_mix_metric)) +
      bbc_style() +
      flat_x_labels()

    to_plotly(plot)
  })

  # *************************Product Purchase Frequency*************************
  output$productFrequencyPlot <- renderPlotly({
    product_frequency <- summarise_sales(filter(sales(), product_category == input$product_category), product_detail)
    validate(need(nrow(product_frequency) > 0, "No sales for this category in the current selection."))
    y <- metric_column(input$product_frequency_type)

    plot <- if (input$product_frequency_plot_type == "Bar Plot") {
      ggplot(product_frequency, aes(x = reorder(product_detail, .data[[y]]), y = .data[[y]])) +
        geom_col(fill = "#6F4E37")
    } else {
      ggplot(product_frequency, aes(x = reorder(product_detail, .data[[y]]), y = .data[[y]])) +
        geom_segment(aes(xend = product_detail, yend = 0), linewidth = 1, color = "#C8A27A") +
        geom_point(color = "#6F4E37", size = 4)
    }

    plot <- plot +
      labs(title = paste(input$product_frequency_type, "by Product:", input$product_category),
           x = NULL, y = input$product_frequency_type) +
      scale_y_continuous(labels = if (y == "total_sales") dollar else comma) +
      coord_flip() +
      bbc_style() +
      flat_x_labels() +
      theme(legend.position = "none", panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#cbcbcb", linetype = "dashed"))

    to_plotly(plot, show_legend = FALSE)
  })

  # *************************Boxplot Analysis*************************
  output$boxplot <- renderPlotly({
    filtered_data <- filter(sales(), product_category == input$product_category_boxplot)
    validate(need(nrow(filtered_data) > 0, "No sales for this category in the current selection."))

    y_var <- switch(input$boxplot_type,
                    "Order Size" = "transaction_qty",
                    "Unit Price" = "unit_price",
                    "Total Price" = "Total_cost")

    plot <- ggplot(filtered_data, aes(x = size, y = .data[[y_var]], fill = size)) +
      geom_boxplot() +
      facet_wrap(~ store_location) +
      labs(x = "Size", y = input$boxplot_type,
           title = paste0(input$boxplot_type, " by Size: ", input$product_category_boxplot)) +
      scale_fill_manual(values = size_colors) +
      scale_y_continuous(labels = if (y_var == "transaction_qty") comma else dollar) +
      bbc_style() +
      flat_x_labels()

    to_plotly(plot, show_legend = FALSE)
  })

  # *************************Size Analysis*************************
  output$size_plot <- renderPlotly({
    filtered_data <- filter(sales(), as.character(size) == input$size_select)
    validate(need(nrow(filtered_data) > 0, "No sales for this size in the current selection."))

    y_var <- switch(input$indicator_size,
                    "Total Cost" = "Total_cost",
                    "Unit Price" = "unit_price",
                    "Order Quantity" = "transaction_qty")

    plot <- switch(input$plot_type_size,
      trend = filtered_data %>%
        group_by(date = as.Date(cut(date, "week", start.on.monday = TRUE)), store_location) %>%
        summarise(value = mean(.data[[y_var]]), .groups = "drop") %>%
        ggplot(aes(x = date, y = value, color = store_location)) +
        geom_line() +
        scale_color_manual(values = store_colors) +
        labs(x = "Week", title = paste0("Weekly Average ", input$indicator_size, ": ", input$size_select, " Size")),
      bar = ggplot(filtered_data, aes(x = store_location, y = .data[[y_var]], fill = store_location)) +
        geom_bar(stat = "summary", fun = "mean") +
        scale_fill_manual(values = store_colors) +
        labs(x = "Store Location", title = paste0("Average ", input$indicator_size, " by Store: ", input$size_select, " Size")),
      box = ggplot(filtered_data, aes(x = store_location, y = .data[[y_var]], fill = store_location)) +
        geom_boxplot() +
        scale_fill_manual(values = store_colors) +
        labs(x = "Store Location", title = paste0(input$indicator_size, " by Store: ", input$size_select, " Size"))
    )

    to_plotly(plot +
                labs(y = input$indicator_size) +
                scale_y_continuous(labels = if (y_var == "transaction_qty") comma else dollar) +
                bbc_style() +
                flat_x_labels(),
              show_legend = input$plot_type_size == "trend")
  })

  # *************************Basket Analysis*************************
  # Baskets depend only on the filters; the sliders re-mine rules without rebuilding them
  baskets <- reactive(build_baskets(sales()))
  rules <- reactive(mine_rules(baskets()$transactions, input$min_support, input$min_confidence))

  output$baskets_box <- renderValueBox({
    valueBox(comma(baskets()$n_baskets), "Baskets analysed", icon = icon("shopping-basket"), color = "purple")
  })

  output$multi_item_box <- renderValueBox({
    valueBox(percent(baskets()$n_multi / baskets()$n_baskets, accuracy = 0.1), "Baskets with 2+ items",
             icon = icon("layer-group"), color = "olive")
  })

  output$rules_box <- renderValueBox({
    valueBox(if (is.null(rules())) 0 else nrow(rules()), "Association rules found",
             icon = icon("link"), color = "maroon")
  })

  output$rulesPlot <- renderPlotly({
    validate(need(!is.null(rules()), "No rules at these thresholds. Try lowering support or confidence."))
    plot_ly(rules(), x = ~support, y = ~confidence, color = ~lift, size = ~count,
            type = "scatter", mode = "markers", colors = c("#f3e9dc", "#6F4E37"),
            text = ~paste0(LHS, " → ", RHS, "<br>Lift: ", round(lift, 2), "<br>Baskets: ", count),
            hoverinfo = "text", marker = list(line = list(color = "#555", width = 0.5))) %>%
      layout(xaxis = list(title = "Support", tickformat = ".1%"),
             yaxis = list(title = "Confidence", tickformat = ".0%")) %>%
      style_plotly("Rules: Support vs Confidence (colour = lift)", show_legend = FALSE)
  })

  output$rules_table <- renderDT({
    validate(need(!is.null(rules()), "No rules at these thresholds."))
    datatable(
      rules() %>% transmute(`If customer buys` = LHS, `They also buy` = RHS,
                            Support = support, Confidence = confidence, Lift = lift, Baskets = count),
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(4, "desc")))
    ) %>%
      formatPercentage(c("Support", "Confidence"), digits = 1) %>%
      formatRound("Lift", digits = 2)
  })

  # *************************Data Explorer*************************
  explorer_data <- reactive({
    sales() %>%
      transmute(`Transaction ID` = transaction_id,
                `Date & Time` = format(transaction_datetime, "%Y-%m-%d %H:%M:%S"),
                Store = store_location, Category = product_category, Type = product_type,
                Product = product_detail, Size = size, Quantity = transaction_qty,
                `Unit Price` = unit_price, Total = Total_cost)
  })

  output$data_table <- renderDT({
    datatable(explorer_data(), filter = "top", rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatCurrency(c("Unit Price", "Total"))
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("coffee_sales_", input$dates[1], "_to_", input$dates[2], ".csv"),
    content = function(file) write.csv(explorer_data(), file, row.names = FALSE)
  )
}
