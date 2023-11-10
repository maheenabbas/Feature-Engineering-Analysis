# Text Manipulation with R

library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl

bikes_tbl <- readxl::read_excel("00_data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl

c("Supersix Evo Black Inc.", "Supersix Evo Hi-Mod Team") %>%
    str_detect(pattern = "Supersix")

bikes_tbl %>%
    select(model) %>%
    mutate(supersix = model %>% str_detect("Supersix")%>% as.numeric())  %>%
    mutate(black = model%>% str_detect("Black")%>% as.numeric())

bikeshop_name <- "Ithaca Mountain Climbers"
str_to_upper(bikeshop_name)
str_to_lower(bikeshop_name)
str_to_title(bikeshop_name)

order_id <-  1
order_line <-  1
str_c("Order Line: ", order_id, ".", order_line, 
      " sent to Customer: ", bikeshop_name)

str_glue("Order Line: {order_id}.{order_line} sent to Customer:{str_to_upper(bikeshop_name)}")

bike_orderlines_tbl %>% 
   select(bikeshop_name, order_id, order_line) %>%
    mutate(purchase_statement = str_glue(
        "Order Line: {order_id}.{order_line} sent to Customer:{str_to_upper(bikeshop_name)}"
    ) %>% as.character())

c("Road - Elite Road - Carbon", "Road - Elite Road" ) %>% str_split(pattern = " - ", simplify = TRUE)

bikes_tbl %>%
    select(description) %>%
    separate(col = description, into = c("category_1", "category_2", "frame_material"),
             sep = " - ",
             remove = FALSE)
"text with space  " %>% str_trim(side = "both")

c("CAAD12", "CAAD", "CAAD8") %>% str_replace(pattern = "[0-9]", replacement = "")
c("CAAD12", "CAAD", "CAAD8") %>% str_replace_all(pattern = "[0-9]", replacement = "")

bikes_tbl %>%
    select(model) %>%
    mutate(model_num_removed = model %>% str_replace_all("[0-9]", ""))

value <- 1e6
(1e6 / 6) %>% scales::number(prefix = "S", suffix = "M")
value %>% scales :: number(prefix = "$", big.mark = ",")

value %>% scales :: dollar(scale = 1/1e6, suffix = "M")

pct %>% scales::number(scale = 100, suffix = "%")
pct %>% scales::percent()

bike_orderlines_tbl %>%
    set_names(names(.) %>% str_replace("_", ".") %>% str_to_upper())

bike_orderlines_tbl %>%
    set_names(str_glue("{names(.)}_bike"))

bike_orderlines_colnames_tbl <- bike_orderlines_tbl %>%
    rename_at(.vars = vars(model:frame_material), 
              .funs = ~ str_c("prod_", .)) %>%
    rename_at(vars(bikeshop_name:state),
              ~ str_c("cust_", .)) 

bike_orderlines_colnames_tbl %>%
    select(contains("cust_"), total_price)

bikes_tbl %>%
    select(model) %>%
    mutate(model = case_when(
        model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
        model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
       model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
        TRUE ~ model
    )) %>%
    separate(col     = model, 
          into    = str_c("model_", 1:7), 
          sep     = " ", 
          remove  = FALSE, 
          fill    = "right") %>%
    
    mutate(model_base = case_when(
        str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
        str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
        str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
        str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
        str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
        TRUE ~ model_1)
    ) %>%
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
    select(-matches("[0-9]")) %>%
    mutate(
        black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
        hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
        team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
        red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
        ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
        dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
        disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    ) %>%
     View() 
