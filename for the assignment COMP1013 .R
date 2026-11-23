
library(tidyverse)   # loads readr (read_csv), dplyr (data wrangling), ggplot2 (plots)

# Create a folder to save the figures so they can be pasted into the report
dir.create("figures", showWarnings = FALSE)


#Task 1 : Product Prices and Shipping Costs

# 1.1 Load the data

order_items <- read_csv("data/order_items.csv")
products    <- read_csv("data/products.csv")

# Quick look at the structure (helps you check the data loaded correctly)
glimpse(order_items)

#1.2 Histogram of item prices + mean and median

# Calculate the summary statistics
mean_price   <- mean(order_items$price)
median_price <- median(order_items$price)

cat("Mean price:  ", round(mean_price, 2), "\n")
cat("Median price:", round(median_price, 2), "\n")

# Histogram on the raw scale.
# geom_vline() draws vertical lines marking the mean and the median.
p1_hist <- ggplot(order_items, aes(x = price)) +
  geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
  geom_vline(aes(xintercept = mean_price,   colour = "Mean"),   linewidth = 1) +
  geom_vline(aes(xintercept = median_price, colour = "Median"), linewidth = 1) +
  scale_colour_manual(name = "Statistic",
                      values = c("Mean" = "red", "Median" = "darkgreen")) +
  labs(title = "Distribution of item prices",
       x = "Price (BRL)", y = "Number of items")

print(p1_hist)
ggsave("figures/t1_price_hist.png", p1_hist, width = 7, height = 4.5)

# Prices are heavily right-skewed (a few very expensive items stretch the axis),
# so a second histogram on a log10 scale shows the shape of the bulk of the data.
p1_hist_log <- ggplot(order_items, aes(x = price)) +
  geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
  scale_x_log10() +
  labs(title = "Distribution of item prices (log10 scale)",
       x = "Price (BRL, log scale)", y = "Number of items")

print(p1_hist_log)
ggsave("figures/t1_price_hist_log.png", p1_hist_log, width = 7, height = 4.5)

# 1.3 Join order_items and products

# left_join keeps every order item and attaches the product details that
# share the same product_id. (Each product_id appears once in products.)
items_products <- order_items %>%
  left_join(products, by = "product_id")

# Some products have no recorded weight (NA). We can't really plot those, so remove them.
# We also report how many were dropped, for transparency.
cat("Rows before removing missing weights:", nrow(items_products), "\n")
items_clean <- items_products %>%
  filter(!is.na(product_weight_g))
cat("Rows after removing missing weights: ", nrow(items_clean), "\n")

#1.4 Scatter plot: freight value vs product weight

# alpha = 0.2 makes points semi-transparent so overlapping points are visible
# (there are over 100,000 points, so they overlap heavily).
# geom_smooth(method = "lm") adds a straight trend line.
p1_scatter <- ggplot(items_clean, aes(x = product_weight_g, y = freight_value)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "red", se = FALSE) +
  labs(title = "Freight value vs product weight",
       x = "Product weight (g)", y = "Freight value (BRL)")

print(p1_scatter)
ggsave("figures/t1_freight_vs_weight.png", p1_scatter, width = 7, height = 4.5)

# Quantify the relationship with correlation coefficients.
# Pearson measures straight-line association; Spearman measures whether the
# two variables rise together (more robust to outliers and skew).
cor_pearson  <- cor(items_clean$product_weight_g, items_clean$freight_value,
                    method = "pearson")
cor_spearman <- cor(items_clean$product_weight_g, items_clean$freight_value,
                    method = "spearman")
cat("Pearson correlation: ", round(cor_pearson, 3), "\n")
cat("Spearman correlation:", round(cor_spearman, 3), "\n")


# Task 2: Orders that were not delivered

#2.1 Load the data

orders    <- read_csv("data/orders.csv")
customers <- read_csv("data/customers.csv")

#2.2 Orders by status

# count() gives the number of orders per status;
# mutate() adds a column with each status's share of all orders.
status_counts <- orders %>%
  count(order_status, name = "n_orders") %>%
  mutate(percentage = n_orders / sum(n_orders) * 100) %>%
  arrange(desc(n_orders))

print(status_counts)

# Bar chart. reorder() sorts the bars from largest to smallest.
# The label above each bar shows the percentage.
p2_status <- ggplot(status_counts,
                    aes(x = reorder(order_status, -n_orders), y = n_orders)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5, size = 3) +
  labs(title = "Number of orders by status",
       x = "Order status", y = "Number of orders")

print(p2_status)
ggsave("figures/t2_status_bar.png", p2_status, width = 7, height = 4.5)

# Percentage NOT delivered = any status other than "delivered".
# (order_status != "delivered") is TRUE/FALSE for each order;
# the mean of TRUE/FALSE values is the proportion of TRUE.
pct_not_delivered <- mean(orders$order_status != "delivered") * 100
cat("Percentage of orders not delivered:", round(pct_not_delivered, 2), "%\n")

#2.3 Not-delivered percentage by customer state

# Join orders with customers using customer_id, so each order gets its state.
orders_customers <- orders %>%
  left_join(customers, by = "customer_id")

# For each state: total orders, and % of those that were not delivered.
state_summary <- orders_customers %>%
  group_by(customer_state) %>%
  summarise(total_orders       = n(),
            not_delivered      = sum(order_status != "delivered"),
            pct_not_delivered  = not_delivered / total_orders * 100) %>%
  arrange(desc(pct_not_delivered))

print(state_summary, n = Inf)   # n = Inf prints all rows

# Horizontal bar chart (coord_flip) so the state labels are easy to read.
p2_state <- ggplot(state_summary,
                   aes(x = reorder(customer_state, pct_not_delivered),
                       y = pct_not_delivered)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Percentage of orders not delivered, by customer state",
       x = "Customer state", y = "Orders not delivered (%)")

print(p2_state)
ggsave("figures/t2_state_bar.png", p2_state, width = 7, height = 6)


# Task 3 : Payment Methods

#3.1 Load the data

payments <- read_csv("data/payments.csv")

#3.2 Usage frequency of each payment type

type_counts <- payments %>%
  count(payment_type, name = "n_payments") %>%
  mutate(percentage = n_payments / sum(n_payments) * 100) %>%
  arrange(desc(n_payments))

print(type_counts)

p3_type <- ggplot(type_counts,
                  aes(x = reorder(payment_type, -n_payments), y = n_payments)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5, size = 3) +
  labs(title = "Number of payments by payment type",
       x = "Payment type", y = "Number of payments")

print(p3_type)
ggsave("figures/t3_type_bar.png", p3_type, width = 7, height = 4.5)

#3.3 Payment values by type (boxplot)

# Numerical summary per type, to back up what the boxplot shows.
value_summary <- payments %>%
  group_by(payment_type) %>%
  summarise(n      = n(),
            mean   = mean(payment_value),
            median = median(payment_value),
            q1     = quantile(payment_value, 0.25),
            q3     = quantile(payment_value, 0.75),
            max    = max(payment_value))

print(value_summary)

# Payment values are very right-skewed, so a log10 axis makes the boxes readable.
# log10(0) is undefined, so we drop zero-value payments for the plot only
# (these are the 3 "not_defined" payments, which all have value 0).
p3_box <- payments %>%
  filter(payment_value > 0) %>%
  ggplot(aes(x = payment_type, y = payment_value, fill = payment_type)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  labs(title = "Payment value by payment type (log10 scale)",
       x = "Payment type", y = "Payment value (BRL, log scale)")

print(p3_box)
ggsave("figures/t3_value_box.png", p3_box, width = 7, height = 4.5)

# End of Script
