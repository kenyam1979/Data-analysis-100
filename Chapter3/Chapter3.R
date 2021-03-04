library(tidyverse)

## 021: Data loading
use_log <- read_csv("サンプルコード_20201021/3章/use_log.csv", 
                    col_types = cols(usedate = col_date(format = "%Y-%m-%d")))
customer_master <- read_csv("サンプルコード_20201021/3章/customer_master.csv", 
                            col_types = cols(start_date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

class_master <- read_csv("サンプルコード_20201021/3章/class_master.csv")
campaign_master <- read_csv("サンプルコード_20201021/3章/campaign_master.csv")


## 022: Enrich customer data
customer_join <- customer_master %>% left_join(y=class_master, by="class") %>% left_join(y=campaign_master, by="campaign_id")
summary(customer_join)
summary(is.na(customer_join))

## 023: Basic summary
customer_join %>% 
  group_by(class_name) %>%
  summarize(n=sum(n()))

customer_join %>% 
  group_by(campaign_name) %>%
  summarize(n=sum(n()))

customer_join %>% 
  group_by(gender) %>%
  summarize(n=sum(n()))

customer_join %>% 
  group_by(is_deleted) %>%
  summarize(n=sum(n()))

customer_join %>% filter(start_date >= as.Date("2018-04-01") & start_date < as.Date("2019-04-01")) %>% summarize(n())

## 024: 意味不明

## 025: Summarize use log 
use_log <- use_log %>% mutate(usemonth=format(usedate, "%Y%m"))
use_log_customer <- use_log %>%
  group_by(customer_id, usemonth) %>%
  summarize(cnt=n()) %>%
  group_by(customer_id) %>% 
  summarize(mean=mean(cnt), median=median(cnt), min=min(cnt), max=max(cnt))

## 026: Repeat flag
use_log <- use_log %>% mutate(useweekday=weekdays(usedate, abbreviate=TRUE))
use_log_weekday <- use_log %>%
  group_by(customer_id, usemonth, useweekday) %>%
  summarize(cnt=n()) %>%
  group_by(customer_id) %>%
  summarize(maxweek=max(cnt)) %>%
  mutate(routine_flg=if_else(maxweek>3, 1, 0))


## 027: Join above
customer_join <- customer_join %>%
  left_join(y=use_log_customer, by="customer_id") %>%
  left_join(y=use_log_weekday, by="customer_id")

summary(is.na(customer_join))

## 028: Calculate membership period
customer_join <- customer_join %>%
  mutate(membership_period=if_else(is.na(end_date), 
                                   as.numeric(difftime(as.Date("2019-04-30"), start_date))/30,
                                   as.numeric(difftime(end_date, start_date))/30)) 

## 029: Analyze customer behavior
summary(customer_join[, c("mean", "median", "min", "max")])

customer_join %>% group_by(routine_flg) %>% summarize(n())

customer_join$is_deleted <- as.factor(customer_join$is_deleted)
customer_join %>% ggplot(aes(x=membership_period, fill=is_deleted)) + geom_histogram(position="identity", alpha=0.6)

## 030: Analyze churn customers
summary(customer_join[customer_join$is_deleted==1, c("price", "mean", "median", "min", "max", "routine_flg", "membership_period")])
summary(customer_join[customer_join$is_deleted==0, c("price", "mean", "median", "min", "max", "routine_flg", "membership_period")])

write_csv(customer_join, "customer_join.csv")

rm(list=ls(all.names = TRUE))

