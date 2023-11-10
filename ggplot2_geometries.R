# ggplot2 Geometries
library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

order_value_tbl <- bike_orderlines_tbl %>%
    
    select(order_id, order_line, total_price, quantity) %>%
    
    group_by(order_id) %>%
    summarize(
        total_quantity = sum(quantity),
        total_price    = sum(total_price)
    ) %>%
    ungroup()

order_value_tbl %>%
    
    ggplot(aes(x = total_quantity, y = total_price)) +
    
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", se = FALSE)

revenue_by_month_tbl <- bike_orderlines_tbl %>%
    
    select(order_date, total_price) %>%
    
    mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
    
    group_by(year_month) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup()

revenue_by_month_tbl %>%
    
    ggplot(aes(year_month, revenue)) +
    
    geom_line(size = 0.5, linetype = 1) +
    geom_smooth(method = "loess", span = 0.2)

revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
    
    select(category_2, total_price) %>%
    
    group_by(category_2) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup()

revenue_by_category_2_tbl %>%
    mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
    ggplot(aes(category_2, revenue)) +
    geom_col(fill = "#2c3e50") + 
    coord_flip()

bike_orderlines_tbl %>%
    
    distinct(model, price) %>%
    ggplot(aes(price)) +
    geom_histogram(bins = 25, fill = "blue", color = "white")

bike_orderlines_tbl %>%
    
    distinct(price, model, frame_material) %>%
    
    ggplot(aes(price, fill = frame_material)) +
    
    geom_histogram() +
    
    facet_wrap(~ frame_material, ncol = 1) +
    
    scale_fill_tq() +
    theme_tq()

bike_orderlines_tbl %>%
    distinct(price, model, frame_material) %>%
    ggplot(aes(price, fill = frame_material)) +
    geom_density(alpha = 0.5) +
    scale_fill_tq() +
    theme_tq() +
    theme(legend.position = "bottom")

unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
    select(category_2, model, price) %>%
    distinct() %>%
    mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

unit_price_by_cat_2_tbl %>%
    ggplot(aes(category_2, price)) +
    geom_boxplot() +
    coord_flip() +
    theme_tq()

unit_price_by_cat_2_tbl %>%
    ggplot(aes(category_2, price)) +
    geom_jitter(width = 0.15, color = "#2c3e50") +
    geom_violin(alpha = 0.5) +
    coord_flip() +
    theme_tq()

revenue_by_year_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    group_by(year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup()

revenue_by_year_tbl %>%
    ggplot(aes(year, revenue)) +
    geom_col(fill = "#2c3e50") +
    geom_smooth(method = "lm", se = FALSE) +
    geom_text(aes(label =  scales::dollar(revenue, scale = 1e-6, suffix = "M")), 
              vjust = 1.5, color = "white") +
    geom_label(label =  "Major Demand This Year",
               vjust = -0.5, 
               size  = 5,
               fill  = "#1f78b4",
               color = "white",
               fontface = "italic",
               data = revenue_by_year_tbl %>%
                   filter(year %in% c(2013))) + 
    expand_limits(y = 2e7) +
    theme_tq()
