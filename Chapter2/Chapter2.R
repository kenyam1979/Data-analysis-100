library(tidyverse)
library(readxl)

## 011: Data loading
uriage <- read_csv("sample_code/2章/uriage.csv", 
                   col_types = cols(purchase_date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
kokyaku_daicho <- read_excel("sample_code/2章/kokyaku_daicho.xlsx")


## 012: Data quick check
view(uriage)
view(kokyaku_daicho)


## 013: Summary data in monthly by product
# Qty
uriage %>% mutate(purchase_month = format(purchase_date, "%Y%m")) %>% 
  group_by(purchase_month, item_name) %>%
  summarize(qty=sum(n())) %>%
  pivot_wider(id_cols=purchase_month, names_from=item_name, values_from=qty, values_fill=0)

# Sales
uriage %>% mutate(purchase_month = format(purchase_date, "%Y%m")) %>% 
  group_by(purchase_month, item_name) %>%
  summarize(sales=sum(item_price)) %>%
  pivot_wider(id_cols=purchase_month, names_from=item_name, values_from=sales, values_fill=0)


## 014: Data cleansing (uriage$item_name)
length(unique(uriage$item_name))

uriage$item_name <- uriage$item_name %>% 
  str_remove_all(" ") %>%
  str_to_upper()
  

## 015: Data cleansing (uriage$item_price)
sum(is.na(uriage$item_price))

price_table <- uriage %>% filter(!is.na(item_price)) %>% group_by(item_name) %>% summarize(item_price=max(item_price))

for (item in price_table$item_name) {
  price <- price_table[price_table$item_name==item,]$item_price
  print(price)
  uriage[is.na(uriage$item_price)&(uriage$item_name==item),]$item_price <- price
}

uriage %>% group_by(item_name) %>% summarize(min(item_price), max(item_price))

## 016: Data cleansing (kokyaku_daicho$customer_name)

colnames(kokyaku_daicho) <- c("customer_name", "customer_name_kana", "region", "email", "reg_date")
kokyaku_daicho$customer_name <- kokyaku_daicho$customer_name %>%
  str_remove_all(" ") %>%
  str_remove_all("　")

## 017: Data cleansing (kokyaku_daicho$reg_date)

kokyaku_daicho <- kokyaku_daicho %>%
  mutate(reg_date=if_else(!str_detect(reg_date, pattern="/"), as.Date("1900-01-01")+as.numeric(reg_date)-2, as.Date(reg_date)))


## 018: Join data
join_data <- left_join(uriage, kokyaku_daicho, by="customer_name")


## 019: Data save
write_csv(join_data, "dump_data.csv")


## 020: Summary in monthly by items
join_data %>% 
  mutate(purchase_month = format(purchase_date, "%Y%m")) %>% 
  group_by(purchase_month, item_name) %>%
  summarize(qty=sum(n())) %>%
  pivot_wider(id_cols=purchase_month, names_from=item_name, values_from=qty, values_fill=0)

join_data %>% 
  mutate(purchase_month = format(purchase_date, "%Y%m")) %>% 
  group_by(purchase_month, item_name) %>%
  summarize(sales=sum(item_price)) %>%
  pivot_wider(id_cols=purchase_month, names_from=item_name, values_from=sales, values_fill=0)

join_data %>% 
  mutate(purchase_month = format(purchase_date, "%Y%m")) %>% 
  group_by(purchase_month, customer_name) %>%
  summarize(qty=sum(n())) %>%
  pivot_wider(id_cols=purchase_month, names_from=customer_name, values_from=qty, values_fill=0)

join_data %>%
  mutate(purchase_month = format(purchase_date, "%Y%m")) %>% 
  group_by(purchase_month, region) %>%
  summarize(qty=sum(n())) %>%
  pivot_wider(id_cols=purchase_month, names_from=region, values_from=qty, values_fill=0)

kokyaku_daicho[!kokyaku_daicho$customer_name %in% join_data$customer_name,]



rm(list=ls(all.names = TRUE))
