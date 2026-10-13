# Data, packages and helpers are loaded in global.R

metric_choices <- c("Revenue" = "revenue", "Baskets (proxy orders)" = "baskets", "Units sold" = "units")

# A titled box holding inputs on the left and output on the right
control_box <- function(title, controls, output_ui, width = 12, footer = NULL) {
  box(
    title = title, status = "primary", solidHeader = TRUE, width = width, footer = footer,
    fluidRow(
      column(width = 3, controls),
      column(width = 9, output_ui)
    )
  )
}

chart_box <- function(title, output_ui, width = 6, footer = NULL) {
  box(title = title, status = "primary", solidHeader = TRUE, width = width, footer = footer, output_ui)
}

# Every page opens with the active filter scope so numbers are never read out of context
scope_row <- function(id) fluidRow(column(12, uiOutput(id, class = "scope-banner")))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = tags$span(icon("mug-hot"), "Coffee Sales")),
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Executive Overview", tabName = "overview", icon = icon("home")),
      menuItem("Demand & Timing", tabName = "demand", icon = icon("clock")),
      menuItem("Products & Stores", tabName = "products", icon = icon("store")),
      menuItem("Baskets & Data", tabName = "baskets", icon = icon("shopping-basket"))
    ),
    hr(),
    # Global filters applied to every page
    selectizeInput("stores", "Stores", choices = store_locations, selected = store_locations,
                   multiple = TRUE, options = list(plugins = list("remove_button"))),
    dateRangeInput("dates", "Date range", start = date_range[1], end = date_range[2],
                   min = date_range[1], max = date_range[2], format = "d M yyyy"),
    actionButton("reset_filters", "Reset filters", icon = icon("undo"), class = "reset-btn")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-box h3 { font-size: 28px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .small-box p { font-size: 14px; }
      .small-box .kpi-sub { font-size: 12px; opacity: 0.9; }
      .content-wrapper { background-color: #f7f3ef; }
      .box.box-solid.box-primary > .box-header { background: #6F4E37; }
      .box.box-solid.box-primary { border-color: #6F4E37; }
      .box.box-primary { border-top-color: #6F4E37; }
      .box-footer { font-size: 12px; color: #555; }
      .skin-black .main-header .logo, .skin-black .main-header .logo:hover { background: #4b3324; color: #fff; border-right: none; }
      .skin-black .main-header .navbar { background: #6F4E37; }
      .skin-black .main-header .navbar .sidebar-toggle { color: #fff; border-right: none; }
      .skin-black .main-header .navbar .sidebar-toggle:hover { background: #4b3324; }
      .sidebar-menu li > a > i:first-child { width: 22px; margin-right: 6px; text-align: center; }
      .reset-btn { display: block; width: calc(100% - 30px); margin: 5px 15px 15px; }
      .scope-banner { background: #fff; border-left: 4px solid #6F4E37; padding: 8px 12px; margin-bottom: 12px; font-size: 13px; }
      .insight { border-left: 3px solid #d8c3ae; padding: 4px 0 4px 12px; margin-bottom: 14px; }
      .insight h4 { margin: 0 0 4px; font-size: 15px; font-weight: bold; }
      .insight p { margin: 0 0 3px; }
      .insight .method, .insight .caveat { font-size: 12px; color: #666; }
      .stat-note { font-size: 13px; color: #333; margin-top: 8px; }
      .irs--shiny .irs-bar { background: #6F4E37; border-color: #6F4E37; }
      .irs--shiny .irs-single { background: #6F4E37; }
      .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #6F4E37; }
    "))),
    tabItems(
      # *************************Page 1: Executive Overview*************************
      tabItem(tabName = "overview",
              scope_row("scope_overview"),
              fluidRow(
                valueBoxOutput("kpi_revenue", width = 3),
                valueBoxOutput("kpi_baskets", width = 3),
                valueBoxOutput("kpi_units", width = 3),
                valueBoxOutput("kpi_basket_value", width = 3)
              ),
              fluidRow(column(12, uiOutput("kpi_footnote"))),
              fluidRow(
                chart_box("Daily revenue and 7-day average", plotlyOutput("dailyTrendPlot", height = "320px"), width = 8),
                chart_box("Revenue by store", plotlyOutput("storeRevenuePlot", height = "320px"), width = 4,
                          footer = "Share of revenue within the current filters.")
              ),
              fluidRow(
                box(title = "Key findings for the current selection", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("insights"),
                    footer = "Findings are recalculated for the selected stores and dates. Associations and differences are observational, not causal.")
              )
      ),

      # *************************Page 2: Demand & Timing*************************
      tabItem(tabName = "demand",
              scope_row("scope_demand"),
              fluidRow(control_box(
                "When is demand highest? Mean per day by weekday and hour",
                tagList(
                  selectInput("heat_metric", "Metric:", choices = metric_choices),
                  uiOutput("peak_summary")
                ),
                plotlyOutput("heatmapPlot", height = "380px"),
                footer = "Each cell is the total for that weekday and hour divided by the number of those weekdays in the selection."
              )),
              fluidRow(
                control_box(
                  "Trend over time by store",
                  tagList(
                    selectInput("trend_metric", "Metric:", choices = metric_choices),
                    radioButtons("trend_grain", "Granularity:", choices = c("Week" = "week_start", "Month" = "month")),
                    helpText("Mean per trading day, so partial weeks and months of different lengths are comparable.")
                  ),
                  plotlyOutput("trendPlot", height = "340px"),
                  width = 12
                )
              ),
              fluidRow(
                chart_box("Weekday vs weekend: mean daily revenue", plotlyOutput("weekdayPlot", height = "300px"),
                          footer = uiOutput("weekend_note")),
                chart_box("Hourly profile by store: mean revenue per day", plotlyOutput("hourlyPlot", height = "300px"),
                          footer = "Opening hours differ: Astoria trades ~07:00-20:00, Hell's Kitchen ~06:30-21:00, Lower Manhattan ~06:00-20:30.")
              )
      ),

      # *************************Page 3: Products & Stores*************************
      tabItem(tabName = "products",
              scope_row("scope_products"),
              fluidRow(
                control_box(
                  "Category and product ranking",
                  tagList(
                    selectInput("product_metric", "Metric:", choices = metric_choices[c(1, 3)]),
                    selectInput("product_category", "Products in category:", choices = product_categories, selected = "Coffee")
                  ),
                  fluidRow(
                    column(6, plotlyOutput("categoryPlot", height = "380px")),
                    column(6, plotlyOutput("productPlot", height = "380px"))
                  )
                )
              ),
              fluidRow(
                box(title = "Store comparison", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("store_table"),
                    uiOutput("store_tests"),
                    footer = paste("Basket value intervals are 95% t intervals. Daily revenue comparisons are paired by date with Holm adjustment.",
                                   "No foot-traffic, opening-hours or cost data, so differences describe sales, not efficiency or profit."))
              ),
              fluidRow(
                chart_box("Category mix by store (share of store revenue)", plotlyOutput("storeMixPlot", height = "340px")),
                chart_box("Size mix by category (share of sized line items)", plotlyOutput("sizeMixPlot", height = "340px"),
                          footer = uiOutput("size_test_note"))
              ),
              fluidRow(control_box(
                "Price distribution by size and store",
                tagList(
                  selectInput("price_category", "Category:", choices = product_categories, selected = "Coffee"),
                  selectInput("price_measure", "Measure:", choices = c("Unit price" = "unit_price", "Line revenue" = "line_revenue",
                                                                       "Quantity per line" = "transaction_qty")),
                  helpText("Points beyond the whiskers are shown, not removed. Lines with 4+ units and non-standard prices are flagged in the Data Explorer.")
                ),
                plotlyOutput("pricePlot", height = "380px")
              ))
      ),

      # *************************Page 4: Baskets & Data*************************
      tabItem(tabName = "baskets",
              scope_row("scope_baskets"),
              fluidRow(
                tabBox(
                  id = "basket_tabs", width = 12,
                  tabPanel(
                    "Basket insights", icon = icon("link"),
                    fluidRow(
                      valueBoxOutput("baskets_box", width = 3),
                      valueBoxOutput("multi_item_box", width = 3),
                      valueBoxOutput("rules_generated_box", width = 3),
                      valueBoxOutput("rules_box", width = 3)
                    ),
                    fluidRow(
                      column(3,
                             radioButtons("basket_min_items", "Eligible baskets:",
                                          choices = c("All baskets" = 1, "Only baskets with 2+ products" = 2)),
                             sliderInput("min_support", "Minimum support:", min = 0.0005, max = 0.02, value = 0.002,
                                         step = 0.0005, ticks = FALSE),
                             sliderInput("min_confidence", "Minimum confidence:", min = 0.05, max = 0.8, value = 0.1,
                                         step = 0.05, ticks = FALSE),
                             sliderInput("min_lift", "Minimum lift:", min = 1, max = 10, value = 1, step = 0.5, ticks = FALSE)
                      ),
                      column(9, plotlyOutput("rulesPlot", height = "380px"))
                    ),
                    DTOutput("rules_table"),
                    tags$div(class = "stat-note",
                      tags$p(tags$b("Basket definition. "),
                             "The data has one row per line item and a unique transaction_id on every row, so there is no order ID. ",
                             "Line items sold at the same store in the same second are grouped as one basket. In every such group ",
                             "the transaction IDs are consecutive, which supports the proxy, but it is still an inference. ",
                             "A product appearing on several lines of one basket counts once."),
                      tags$p(tags$b("Reading the measures. "),
                             "Support: share of eligible baskets containing the rule's items. ",
                             "Confidence: share of baskets with the first item(s) that also contain the second. ",
                             "Lift: confidence divided by the second item's overall share (\"Consequent share\"); above 1 means bought together more than chance. ",
                             "Restricting to 2+ product baskets raises support and confidence because single-item baskets are dropped from the denominators."),
                      tags$p(tags$b("Caveats. "),
                             "Association is not causation. Products sold at only some stores (e.g. Ouro Brasileiro shot) can show high lift ",
                             "because of where they are sold; use the store filter to mine one store at a time. A bundle idea should be tested ",
                             "(e.g. a time-boxed offer in one store compared with the others) before it is rolled out.")
                    )
                  ),
                  tabPanel(
                    "Data explorer", icon = icon("database"),
                    downloadButton("download_data", "Download rows shown (CSV)"),
                    helpText("The download contains every row that passes the sidebar filters and the column filters below, across all table pages."),
                    DTOutput("data_table")
                  ),
                  tabPanel(
                    "Data quality & definitions", icon = icon("clipboard-check"),
                    h4("Data quality checks"),
                    DTOutput("quality_table"),
                    h4("Metric definitions"),
                    DTOutput("metric_table"),
                    tags$p(class = "stat-note",
                           "Source: Maven Analytics \"Coffee Shop Sales\" sample dataset (fictitious New York coffee chain, Jan-Jun 2023).")
                  )
                )
              )
      )
    )
  )
)
