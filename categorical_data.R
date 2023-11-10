#CATEGORICAL DATA MANIPULATION ----

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl
sales_by_cat_2_tbl <- bike_orderlines_tbl %>% 
    select(category_2, total_price) %>%
    group_by(category_2) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(sales)) %>%
    mutate(category_2 = category_2 %>% as_factor() %>% fct_rev())

sales_by_cat_2_tbl %>%
    ggplot(aes(x= sales, y = category_2)) +
    geom_point(size = 5) + 
    labs(title = "Sales By Category 2") +
    scales_x_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    expand_limits(x = 0)

plot_sales <- function(data) {
    ggplot(aes(x= sales, y = category_2)) +
        geom_point(size = 5, color = "#2c3e50") + 
        labs(title = "Sales By Category 2") +
        scales_x_continuous(labels = scales::dollar_format()) +
        theme_tq() +
        expand_limits(x = 0)

}
sales_by_cat_2_tbl %>% 
    plot_sales()

sales_by_cat_2_tbl %>% pull(category_2) %>% levels()
ales_by_cat_2_tbl %>% pull(category_2) %>% as.numeric()

sales_by_cat_2_tbl %>% 
    mutate(category_2 = category_2 %>% fct_rev()) %>%
    mutate(
        label = category_2 %>% as.character(),
        value = category_2 %>% as.numeric()
    )

sales_by_cat_2_tbl %>%
    mutate(
        category_2           = as.character(category_2),
        category_2_as_factor = as_factor(category_2) %>% as.numeric(),
        category_2_as.factor = as.factor(category_2) %>% as.numeric()
    )

sales_by_cat_2_tbl %>%
    arrange(desc(sales)) %>%
    mutate(sales_negative = -sales) %>%
    mutate(
        category_2 = category_2 %>% fct_reorder(sales_negative),
        values     = category_2 %>% as.numeric()) %>%
    
    plot_sales()

sales_by_cat_2_q_tbl <- bike_orderlines_tbl %>%
    
    mutate(order_date = order_date %>% floor_date("quarter") %>% ymd()) %>%
    group_by(category_2, order_date) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup()

sales_by_cat_2_q_tbl


sales_by_cat_2_q_tbl %>%
    
    mutate(category_2 = category_2 %>% fct_reorder2(order_date, sales)) %>%
    ggplot(aes(x = order_date, y = sales, color = category_2)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ category_2) +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))

sales_by_cat_2_tbl %>%
    mutate(category_2 = category_2 %>% fct_lump(n = 6, w = sales, other_level = "All Other Bike Categories")) %>%
    group_by(category_2) %>%
    summarize(sales = sum(sales)) %>%
    mutate(category_2 = category_2 %>% fct_relevel("All Other Bike Categories", after = 0)) %>%
    plot_sales()





