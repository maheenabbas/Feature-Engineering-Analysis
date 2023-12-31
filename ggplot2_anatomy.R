#ANATOMY OF A GGPLOT2 OBJECT
library(tidyverse)
library(lubridate)
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")
glimpse(bike_orderlines_tbl)

revenue_by_year_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    group_by(year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup()

revenue_by_year_tbl

g <- revenue_by_year_tbl %>%

    ggplot(aes(x = year, y = revenue, color = revenue)) +
    geom_line(size = 1) +
    geom_point(size = 5) +
    geom_smooth(method = "lm", se = FALSE) +
    expand_limits(y = 0) +
    scale_color_continuous(low = "red", high = "black",
                           labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
    labs(
        title = "Revenue",
        subtitle = "Sales are trending up and to the right!",
        x = "",
        y = "Sales (Millions)",
        color = "Rev ($M)",
        caption = "What's happening?\nSales numbers showing year-over-year growth."
    ) +
    theme_bw() +
    theme(legend.position = "right", legend.direction = "vertical")
g # whats g?

View(g)
