# Data, packages and helpers are loaded in global.R

metric_choices <- c("Total Sales", "Total Orders")

# A titled box holding inputs on the left and a chart on the right
control_box <- function(title, controls, output_ui, width = 12) {
  box(
    title = title, status = "primary", solidHeader = TRUE, width = width,
    fluidRow(
      column(width = 3, controls),
      column(width = 9, output_ui)
    )
  )
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = tags$span(icon("mug-hot"), "Coffee Sales")),
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem(
        "Sales Trends", icon = icon("chart-line"), startExpanded = TRUE,
        menuSubItem("Hourly", tabName = "hourly_sales", icon = icon("clock")),
        menuSubItem("Weekly", tabName = "weekly_sales", icon = icon("calendar-week")),
        menuSubItem("Monthly", tabName = "monthly_sales", icon = icon("calendar")),
        menuSubItem("Peak Hours", tabName = "peak_hours", icon = icon("fire"))
      ),
      menuItem("Product Categories", tabName = "product_analysis", icon = icon("tag")),
      menuItem("Products", tabName = "product_type", icon = icon("coffee")),
      menuItem("Store Performance", tabName = "store_performance", icon = icon("map-marker-alt")),
      menuItem("Size Mix", tabName = "order_size", icon = icon("list-alt")),
      menuItem("Price Distribution", tabName = "boxplot_analysis", icon = icon("chart-bar")),
      menuItem("Size Analysis", tabName = "size_analysis", icon = icon("chart-area")),
      menuItem("Basket Analysis", tabName = "basket_analysis", icon = icon("shopping-basket")),
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("database"))
    ),
    hr(),
    # Global filters applied to every tab
    selectizeInput("stores", "Stores", choices = store_locations, selected = store_locations,
                   multiple = TRUE, options = list(plugins = list("remove_button"))),
    dateRangeInput("dates", "Date range", start = date_range[1], end = date_range[2],
                   min = date_range[1], max = date_range[2]),
    actionButton("reset_filters", "Reset filters", icon = icon("undo"), class = "reset-btn")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-box h3 { font-size: 30px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .content-wrapper { background-color: #f7f3ef; }
      .box.box-solid.box-primary > .box-header { background: #6F4E37; }
      .box.box-solid.box-primary { border-color: #6F4E37; }
      .box.box-primary { border-top-color: #6F4E37; }
      .skin-black .main-header .logo, .skin-black .main-header .logo:hover { background: #4b3324; color: #fff; border-right: none; }
      .skin-black .main-header .navbar { background: #6F4E37; }
      .skin-black .main-header .navbar .sidebar-toggle { color: #fff; border-right: none; }
      .skin-black .main-header .navbar .sidebar-toggle:hover { background: #4b3324; }
      .sidebar-menu li > a > i:first-child { width: 22px; margin-right: 6px; text-align: center; }
      .reset-btn { display: block; width: calc(100% - 30px); margin: 5px 15px 15px; }
      .irs--shiny .irs-bar { background: #6F4E37; border-color: #6F4E37; }
      .irs--shiny .irs-single { background: #6F4E37; }
    "))),
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales_box", width = 3),
                valueBoxOutput("transactions_box", width = 3),
                valueBoxOutput("avg_transaction_box", width = 3),
                valueBoxOutput("top_product_box", width = 3)
              ),
              fluidRow(
                box(width = 8, status = "primary", plotlyOutput("dailyTrendPlot", height = "350px")),
                box(width = 4, status = "primary", plotlyOutput("storeSharePlot", height = "350px"))
              ),
              fluidRow(
                box(
                  title = "About this dashboard",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 9,
                           p("Interactive analysis of 149,116 transactions from three New York coffee shops ",
                             "(Astoria, Hell's Kitchen, Lower Manhattan), January to June 2023. ",
                             "Use the store and date filters in the sidebar; every tab updates to match."),
                           tags$ul(
                             tags$li(tags$b("Sales Trends:"), " hourly, weekly and monthly patterns, plus a weekday-by-hour heatmap for staffing."),
                             tags$li(tags$b("Products and Stores:"), " category, product and store comparisons."),
                             tags$li(tags$b("Size and Price:"), " size preferences and price distributions."),
                             tags$li(tags$b("Basket Analysis:"), " products that are bought together, mined with the Apriori algorithm."),
                             tags$li(tags$b("Data Explorer:"), " search, filter and download the underlying transactions.")
                           )
                    ),
                    column(width = 3, style = "text-align: center;", img(src = "coffee.png", height = 130))
                  )
                )
              )
      ),
      # Sales Trend Tabs
      tabItem(tabName = "hourly_sales",
              fluidRow(control_box(
                "Hourly Sales",
                tagList(
                  selectInput("hourly_sales_type", "Metric:", choices = metric_choices),
                  radioButtons("hourly_sales_plot_type", "Plot type:", choices = c("Line Plot", "Lollipop Plot"))
                ),
                plotlyOutput("hourlySalesPlot", height = "450px")
              ))
      ),
      tabItem(tabName = "weekly_sales",
              fluidRow(control_box(
                "Weekly Sales",
                tagList(
                  selectInput("weekly_sales_type", "Metric:", choices = metric_choices),
                  radioButtons("weekly_sales_plot_type", "Plot type:", choices = c("Line Plot", "Lollipop Plot"))
                ),
                plotlyOutput("weeklySalesPlot", height = "450px")
              ))
      ),
      tabItem(tabName = "monthly_sales",
              fluidRow(control_box(
                "Monthly Sales",
                tagList(
                  selectInput("monthly_sales_type", "Metric:", choices = metric_choices),
                  radioButtons("monthly_sales_plot_type", "Plot type:", choices = c("Line Plot", "Lollipop Plot"))
                ),
                plotlyOutput("monthlySalesPlot", height = "450px")
              ))
      ),
      tabItem(tabName = "peak_hours",
              fluidRow(control_box(
                "Peak Hours",
                tagList(
                  selectInput("peak_metric", "Metric:", choices = metric_choices),
                  uiOutput("peak_summary")
                ),
                plotlyOutput("peakHoursPlot", height = "450px")
              ))
      ),
      # Product Category Tab
      tabItem(tabName = "product_analysis",
              fluidRow(control_box(
                "Sales by Product Category",
                tagList(
                  selectInput("category_sales_type", "Metric:", choices = metric_choices),
                  radioButtons("category_sales_plot_type", "Plot type:", choices = c("Bar Plot", "Treemap"))
                ),
                plotlyOutput("categorySalesPlot", height = "450px")
              ))
      ),
      # Products Tab
      tabItem(tabName = "product_type",
              fluidRow(control_box(
                "Product Sales within a Category",
                tagList(
                  selectInput("product_category", "Category:", choices = product_categories, selected = "Coffee"),
                  selectInput("product_frequency_type", "Metric:", choices = metric_choices),
                  radioButtons("product_frequency_plot_type", "Plot type:", choices = c("Bar Plot", "Lollipop Plot"))
                ),
                plotlyOutput("productFrequencyPlot", height = "500px")
              ))
      ),
      # Store Performance Tab
      tabItem(tabName = "store_performance",
              fluidRow(control_box(
                "Sales by Store Location",
                tagList(
                  selectInput("location_sales_type", "Metric:", choices = metric_choices),
                  radioButtons("location_sales_plot_type", "Plot type:", choices = c("Bar Plot", "Pie Chart"))
                ),
                plotlyOutput("locationSalesPlot", height = "400px")
              )),
              fluidRow(box(title = "Store Summary", status = "primary", solidHeader = TRUE, width = 12,
                           DTOutput("store_table")))
      ),
      # Size Mix Tab
      tabItem(tabName = "order_size",
              fluidRow(control_box(
                "Size Mix by Category",
                tagList(
                  selectInput("size_mix_metric", "Metric:", choices = metric_choices, selected = "Total Orders"),
                  checkboxInput("size_mix_share", "Show as share of category", value = TRUE)
                ),
                plotlyOutput("orderSizePlot", height = "450px")
              ))
      ),
      # Price Distribution Tab
      tabItem(tabName = "boxplot_analysis",
              fluidRow(control_box(
                "Price Distribution by Size and Store",
                tagList(
                  selectInput("product_category_boxplot", "Category:", choices = product_categories, selected = "Coffee"),
                  selectInput("boxplot_type", "Measure:", choices = c("Unit Price", "Total Price", "Order Size"))
                ),
                plotlyOutput("boxplot", height = "450px")
              ))
      ),
      # Size Analysis Tab
      tabItem(tabName = "size_analysis",
              fluidRow(control_box(
                "Size Analysis",
                tagList(
                  selectInput("size_select", "Size:", choices = c("Large", "Regular", "Small", "Others")),
                  selectInput("indicator_size", "Measure:", choices = c("Total Cost", "Unit Price", "Order Quantity")),
                  radioButtons("plot_type_size", "Plot type:",
                               choices = list("Weekly Trend" = "trend", "Bar Plot" = "bar", "Box Plot" = "box"))
                ),
                plotlyOutput("size_plot", height = "500px")
              ))
      ),
      # Basket Analysis Tab
      tabItem(tabName = "basket_analysis",
              fluidRow(
                valueBoxOutput("baskets_box", width = 4),
                valueBoxOutput("multi_item_box", width = 4),
                valueBoxOutput("rules_box", width = 4)
              ),
              fluidRow(control_box(
                "Products Bought Together (Apriori)",
                tagList(
                  sliderInput("min_support", "Minimum support:", min = 0.001, max = 0.02, value = 0.002, step = 0.001, ticks = FALSE),
                  sliderInput("min_confidence", "Minimum confidence:", min = 0.05, max = 0.8, value = 0.1, step = 0.05, ticks = FALSE),
                  helpText("Each row in the data has its own transaction ID, so items rung up at the same store ",
                           "at the same second are grouped into one basket. Only baskets with 2+ items are mined. ",
                           "Lift above 1 means the items are bought together more often than chance.")
                ),
                plotlyOutput("rulesPlot", height = "400px")
              )),
              fluidRow(box(title = "Association Rules", status = "primary", solidHeader = TRUE, width = 12,
                           DTOutput("rules_table")))
      ),
      # Data Explorer Tab
      tabItem(tabName = "data_explorer",
              fluidRow(
                box(
                  title = "Data Explorer",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  downloadButton("download_data", "Download filtered data (CSV)"),
                  br(), br(),
                  DTOutput("data_table")
                )
              )
      )
    )
  )
)
