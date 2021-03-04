library(tidyverse)


## 001: Data loading
customer_master <- read_csv("サンプルコード_20201021/1章/customer_master.csv", 
                            col_types = cols(registration_date = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                             birth = col_date(format = "%Y/%m/%d")))
item_master <- read_csv("サンプルコード_20201021/1章/item_master.csv")
transaction_1 <- read_csv("サンプルコード_20201021/1章/transaction_1.csv", 
                          col_types = cols(payment_date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
transaction_2 <- read_csv("サンプルコード_20201021/1章/transaction_2.csv", 
                          col_types = cols(payment_date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
transaction_detail_1 <- read_csv("サンプルコード_20201021/1章/transaction_detail_1.csv", 
                                 col_types = cols(detail_id = col_double()))
transaction_detail_2 <- read_csv("サンプルコード_20201021/1章/transaction_detail_2.csv")


## 002: Union for multiple data sources
transaction <- union_all(transaction_1, transaction_2)
transaction_detail <- union_all(transaction_detail_1, transaction_detail_2)


## 003: Join for multiple tables 1
join_data <- left_join(transaction_detail, transaction[,c(1,3,4)], by="transaction_id")


## 004: Join 2
join_data <- left_join(join_data, customer_master, by ="customer_id")
join_data <- left_join(join_data, item_master, by = "item_id")


## 005: Mutate a new column
join_data <- join_data %>% mutate(price = item_price * quantity)


## 006: Data validation
sum(join_data$price) == sum(transaction$price)

## 007: Basic statistics
summary(!is.na(join_data))
summary(join_data)


## 008: Summary in monthly
join_data <- join_data %>% mutate(payment_month = format(payment_date, "%Y%m"))
join_data %>% group_by(payment_month) %>% summarize(sum(price))


## 009: Summary in monthly and by item
join_data %>% group_by(payment_month, item_name) %>% summarize(sales=sum(price), amt=sum(quantity))
join_data %>% group_by(payment_month, item_name) %>% summarize(sales=sum(price), amt=sum(quantity)) %>% pivot_wider(id_cols=item_name, names_from=payment_month, values_from=c(sales, amt))


## 010: Visualization
join_data %>% group_by(payment_month, item_name) %>% summarize(sales=sum(price)) %>% 
  ggplot(aes(x=payment_month, y=sales, color=item_name, group=item_name)) + geom_line()



rm(list=ls(all.names = TRUE))