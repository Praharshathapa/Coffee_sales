# *************************Dataset and Package Import*************************
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(corrplot)
library(RColorBrewer)
library(ggthemes)
library(knitr)
library(treemapify)
library(scales)




# Load data
# Run from the project root (open coffee-sales-dashboard.Rproj); the data lives with the app
df <- read_excel(file.path("Dashboard", "Coffee Shop Sales.xlsx"))
# *************************Data Cleaning*************************
# Data Cleaning and format datetime columns
missing_values <- colSums(is.na(df))
print(missing_values)

df$transaction_time <- sub("1899-12-31 ", "", df$transaction_time)
datetime_string <- paste(df$transaction_date, df$transaction_time)
df$transaction_datetime <- as.POSIXct(datetime_string, format = "%Y-%m-%d %H:%M:%S")

# Add additional columns
df <- df %>%
  mutate(
    hour_of_day = format(transaction_datetime, "%H"),
    month = format(transaction_datetime, "%m"),
    day_of_week = weekdays(transaction_datetime),
    Total_cost = unit_price * transaction_qty,
    DateTime = as.POSIXct(paste(transaction_date, transaction_time), format = "%Y-%m-%d %H:%M:%S"),
    size = case_when(
      # Match the trailing size token only; "RG" alone would match "Organic"
      grepl(" Lg$", product_detail) ~ "Large",
      grepl(" Rg$", product_detail) ~ "Regular",
      grepl(" Sm$", product_detail) ~ "Small",
      TRUE ~ "Others"
    )
  )
# *************************Feature Engineering*************************
# Aggregate sales function
aggregate_sales <- function(data, category) {
  data %>%
    filter(product_category == category) %>%
    group_by(hour_of_day, store_location) %>%
    summarize(transaction_qty = sum(transaction_qty), .groups = 'drop') %>%
    mutate(product_category = category)
}

# Apply aggregation function to each category
hourly_coffee_sales <- aggregate_sales(df, "Coffee")
hourly_tea_sales <- aggregate_sales(df, "Tea")
hourly_chocolate_sales <- aggregate_sales(df, "Drinking Chocolate")
hourly_bakery_sales <- aggregate_sales(df, "Bakery")

# Combine aggregated data
combined_data <- bind_rows(hourly_coffee_sales, hourly_tea_sales, hourly_chocolate_sales, hourly_bakery_sales)

# *************************Theme For the plots*************************
bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    # Text format
    plot.title = ggplot2::element_text(family = font,
                                       size = 24, 
                                       face = "bold", 
                                       color = "#222222", 
                                       hjust = 0.5,  # Center the title horizontally
                                       margin = ggplot2::margin(15, 0, 10, 0)), # Add more space around title
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = 15,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_blank(),
    
    # Legend format
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_rect(fill = "white", color = NA),  # Add a white background to the legend
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font,
                                        size = 10, 
                                        color = "#222222"),
    
    # Axis format
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family = font,
                                      size = 10, 
                                      color = "#222222"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10), angle = 45, hjust = 1),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "#cbcbcb", size = 0.5), # Add a light grey axis line
    
    # Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linetype = "dashed"), # Make gridlines dashed
    panel.grid.major.x = ggplot2::element_blank(),
    
    # Blank background
    panel.background = ggplot2::element_rect(fill = "white", color = NA), # Add a white background to the plot
    
    # Strip background 
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 13, hjust = 0)
  )
}
# ....................Plot hourly order trends....................
ggplot(combined_data, aes(x = hour_of_day, y = transaction_qty, color = product_category, group = product_category)) +
  geom_line() +
  geom_point() +
  labs(x = "Hour of the Day", y = "Total Orders", title = "Order Trends by Hour of the Day Across Locations") +
  facet_wrap(~ store_location, ncol = 2, nrow = 2)+
  bbc_style()



# ....................Monthly sales....................
monthly_sales <- df %>%
  group_by(month) %>%
  summarise(total_sales = sum(unit_price * transaction_qty))

# Plot monthly sales
ggplot(monthly_sales, aes(x = month, y = total_sales, group = 1)) +
  geom_line(color = "blue") +
  geom_segment(aes(xend = month, yend = 0), color = "blue", size = 1)+
  geom_point(color = "skyblue",size=3) +
  labs(title = "Seasonal Sales Pattern", x = "Month", y = "Total Sales") +
  bbc_style()+
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))+
  geom_label(
    aes(label = dollar_format(prefix = "$", digits = 2)(total_sales)),
    nudge_x = 0.25,
    nudge_y = 0.25,
    check_overlap = TRUE,size = 2.5
  )+
  theme(
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank()    
  )



# ....................Sales by day of the week....................
sales_by_weekday <- df %>%
  group_by(day_of_week, store_location) %>%
  summarise(total_sales = round(sum(Total_cost),0))

sales_by_weekday$day_of_week <- factor(sales_by_weekday$day_of_week, 
                                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot sales by day of the week
ggplot(sales_by_weekday, aes(x = day_of_week, y = total_sales, color=store_location,group = 1)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_label(
    aes(label = dollar_format(prefix = "$", digits = 2)(total_sales)),
    nudge_x = 0.25,
    nudge_y = 0.25,
    check_overlap = TRUE,size = 2.5
  )+
  labs(title = "Sales Variation by Day of the Week", x = "Day of the Week", y = "Total Sales") +
  facet_wrap(~ store_location, ncol = 2 ,scales = "free") +
  bbc_style()+
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank()    
  )
  

# ....................Average transaction value per hour....................
transaction_value_hour <- df %>%
  group_by(hour_of_day, store_location) %>%
  summarise(average_value = mean(Total_cost), .groups = "drop")

# Plot average transaction value per hour
ggplot(transaction_value_hour, aes(x = hour_of_day, y = average_value,color=store_location)) +
  geom_line(group = 1) +
  geom_point(size=2) +
  labs(title = "Average Transaction Value by Hour", x = "Hour of the Day", y = "Average Transaction Value (USD)") +
bbc_style() +
  facet_wrap(~ store_location, ncol = 2,scales = "free")+
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank()    
  )


# ....................Sales by product category....................
sales_by_category <- df %>%
  group_by(product_category) %>%
  summarize(total_sales = sum(Total_cost))



# Plot sales by product category
ggplot(sales_by_category, aes(x = reorder(product_category, -total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  geom_text(aes(label = dollar_format(prefix = "$", digits = 2)(total_sales), y = total_sales*1),  
            position = position_dodge(width = 0.9), 
            size = 4,
            vjust = -0.3,  
            color = "black") +
  labs(title = "Sales by Product Category", x = "Product Category", y = "Total Sales") +
  bbc_style()+
  theme(
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank()    
  )

# ....................Sales by store location....................
sales_by_location <- df %>%
  group_by(store_location) %>%
  summarize(total_sales = sum(Total_cost)) %>%
  mutate(percentage = total_sales / sum(total_sales) * 100)

# Create a pie chart for sales by store location
ggplot(sales_by_location, aes(x = "", y = percentage, fill = store_location)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Total Sales by Store Location", fill = "Store Location") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),size = 5)




# ....................Most popular products in each location....................
product_location_popularity <- df %>%
  group_by(store_location, product_detail,product_category) %>%
  summarise(total_qty = sum(transaction_qty))

top_products <- product_location_popularity %>%
  arrange(store_location, -(total_qty)) %>%
  group_by(store_location) %>%
  top_n(5, total_qty)

ggplot(top_products, aes(x = reorder(product_detail, -total_qty), y = total_qty, fill = product_category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = total_qty, y = total_qty/2),  
            position = position_dodge(width = 0.9), 
            size = 4,
            vjust = 0.5,  
            color = "white") +
  labs(title = "Top 5 Most Popular Products in Each Location", 
       x = "Product Detail", 
       y = "Total Quantity Sold", 
       fill = "Product Category") +
  bbc_style()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 0)) +
  facet_wrap(~ store_location, scales = "free_x")+
  scale_fill_brewer(palette = "Set2") 


# ....................Order size preferences....................
order_size_preferences <- df %>%
  group_by(size, product_detail, store_location) %>%
  summarise(total_orders = n())
order_size_preferences

# Top product preferences by order size
top_products <- order_size_preferences %>%
  group_by(store_location,size) %>%
  top_n(5, total_orders)
top_products <- top_products %>%
  mutate(product_detail = reorder(product_detail, total_orders))
order_size_preferences <- order_size_preferences[order_size_preferences$size != "Others", ]


ggplot(order_size_preferences, aes(x = total_orders, fill = size)) +
  geom_density(alpha = 0.7, color = "black") +
  scale_fill_tableau("Superfishel Stone") +
  labs(title = "Density of Total Orders by Product Detail by Size", 
       x = "Total Orders", y = "Density", fill = "Order Size") +
  # facet_wrap(~ store_location, scales = "free", ncol = 2) +
  bbc_style() 

# ....................Popular coffee drinks....................
coffee_drinks <- df %>%
  filter(product_category == "Coffee") %>%
  group_by(store_location, product_type, product_detail) %>%  
  summarise(total_sold = sum(transaction_qty)) %>%
  group_by(store_location) %>%
  slice_max(total_sold, n = 5)
ggplot(coffee_drinks, aes(area = total_sold, fill = product_type, label = product_detail)) +
  geom_treemap(layout = "squarified") +
  labs(title = "Top 5 Coffee Products by Location", fill = "Coffee Type") +
  facet_wrap(~ store_location, ncol = 3) +
  bbc_style() +
  theme(legend.position = "bottom") + 
  geom_treemap_text(color = "white", place = "topleft", size = 10)




# ....................Table: Transaction Summary by Product Type....................
summary_table <- df %>%
  group_by(product_type) %>%
  summarise(transaction_count = n(),
            total_revenue = sum(Total_cost, na.rm = TRUE)) %>%
  arrange(desc(total_revenue)) %>%
  head(15)

kable(summary_table, format = "latex", booktabs = TRUE)

# ....................Top Products on Evening....................
evening_data <- df %>%
  select(product_detail, product_category, hour_of_day,store_location) %>%
  filter((hour_of_day) >= 17) %>%  # Filter transactions after 5 PM
  group_by(product_detail, product_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

ggplot(evening_data, aes(x = reorder(product_detail, count), y = count, fill = product_category)) +
  geom_segment(aes(xend = product_detail, yend = 0), color = "black") +
  geom_point(size = 4, shape = 21, color = "white") +
  coord_flip() +
  labs(title = "Top 10 Products in the Evening", x = "Product Detail", y = "Count") +
  bbc_style()

# ....................Top Products on Morning....................
morning_data <- df %>%
  select(product_detail, product_category, hour_of_day) %>%
  filter(as.numeric(hour_of_day) <= 10) %>%  # Filter transactions before 10 PM
  group_by(product_detail, product_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

ggplot(morning_data, aes(x = reorder(product_detail, count), y = count, fill = product_category)) +
  geom_segment(aes(xend = product_detail, yend = 0), color = "black") +
  geom_point(size = 4, shape = 21, color = "white") +
  coord_flip() +
  labs(title = "Top 10 Products in the Morning (Before 10 AM)", x = "Product Detail", y = "Count") +
  bbc_style() +
  theme(legend.position = "bottom")



# .................... Top total sales for each product detail....................
product_location_bottom <- df %>%
  group_by(store_location, product_detail,product_category) %>%
  summarise(total_qty = sum(transaction_qty))

bottom_products <- product_location_popularity %>%
  arrange(store_location, -total_qty) %>%
  group_by(store_location) %>%
  slice_min(n = 5, order_by = total_qty)

# Bar plot for bottom-most selling products
ggplot(bottom_products, aes(x = reorder(product_detail,-total_qty), y = total_qty, fill = product_category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = total_qty, y = total_qty/2),  
            position = position_dodge(width = 0.9), 
            size = 4,
            vjust = 0.5,  
            color = "white") +
  labs(title = "Bottom Most  Products in Each Location", 
       x = "Product Detail", 
       y = "Total Quantity Sold", 
       fill = "Product Category") +
  bbc_style()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  facet_wrap(~ store_location, scales = "free_x")+
  scale_fill_brewer(palette = "Set2") 





# ....................Box plot of Product category by Unit Price....................
ggplot(df, aes(x = product_category, y = unit_price, fill = product_category)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.shape = 1) +  
  labs(title = "Price Distributions Across Different Product Categories",
       x = "Product Category",
       y = "Unit Price") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10))
  ) +
  scale_fill_brewer(palette = "Set3")

# ....................Apriori Algorithm(Basket Or Association Analysis)....................

library(arulesViz)
library(arules)


# Identify duplicate timestamps
duplicates <- df %>%
  filter(duplicated(DateTime) | duplicated(DateTime, fromLast = TRUE))

# Note: every row has a unique transaction_id, so items sharing an exact
# timestamp are treated as one basket (a proxy for a multi-item order)
print(head(duplicates))


grouped_data <- duplicates %>%
  group_by(DateTime) %>%
  summarise_all(list(~ paste(unique(.), collapse = "/")))
# Formatting the data for changing to transaction object
grouped_data <- grouped_data %>%
  select(transaction_id, transaction_date, transaction_time, product_detail)
# SAving the Object to transaction.csv
write.csv(grouped_data, file = "transactions.csv", quote = FALSE, row.names = TRUE)

# changing to transaction object
data <- arules::read.transactions(file = "transactions.csv", rm.duplicates = TRUE, format = "basket", sep = "/", cols = 2)
# Checking the data 
summary(data)
# Using the Apriori Algorithm
rules <- arules::apriori(data, parameter = list(minlen = 2, sup = 0.001, conf = 0.01, target = "rules")) 


# Inspect the rules
inspect(rules)
# Dataype of rules 
mode(rules)
# Visualize the rules
plot(rules, method = "grouped", control = list(type = "items"))

# ******************************THANK YOU******************************

