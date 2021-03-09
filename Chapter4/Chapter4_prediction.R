library(tidyverse)

## 031: Data loading
use_log <- read_csv("sample_code/3章/use_log.csv", 
                    col_types = cols(usedate = col_date(format = "%Y-%m-%d")))

customer_join <- read_csv("Chapter3/customer_join.csv", 
                          col_types = cols(start_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
                                           end_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")))
summary(customer_join)


## 036: Pre-processing for prediction
tmp <- use_log %>% 
  mutate(usemonth=format(usedate, '%Y%m')) %>% 
  group_by(customer_id, usemonth) %>% 
  summarize(n=n())

###基準月データ
data <- tmp %>% filter(usemonth>="201810") %>% mutate(cutoff=usemonth, count='count_pred')
months <- sort(unique(tmp$usemonth), decreasing=TRUE)
###各月データ
for (i in 1:6) {
  cutoff=(months[i])
  for (j in 1:6) {
    mon = months[i+j]
    count_name = sprintf('count_%d', j-1)
    tmp2 <- tmp %>% filter(usemonth==mon) %>% mutate(cutoff=cutoff, count=count_name)
    data <- union(data, tmp2)
  }
}
data <- data %>% pivot_wider(id_cols=c(cutoff, customer_id), names_from=count, values_from=n) %>% arrange(cutoff)
predict_data <- na.omit(data)
rm(list=c('data', 'tmp', 'tmp2', 'count_name', 'cutoff', 'i', 'j', 'mon', 'months'))

## 037: Additional features
predict_data <- left_join(predict_data, customer_join[, c('customer_id', 'start_date')], by='customer_id') %>% 
  mutate(now_date=as.Date('2018-10-01'), period=round(as.numeric(difftime(now_date, start_date))/30))


## 038: Liner model
predict_data2 <-predict_data %>% filter(start_date>=as.Date('2018/04/01'))
model <- lm(count_pred~count_0+count_1+count_2+count_3+count_4+count_5+period, predict_data2)


# 039: Model analysis
summary(model)

### *There will be some differences from the book due to randomized sampling of data set.

# 049: Prediction
new_data <- tibble(count_0=c(3,2),
       count_1=c(4,2),
       count_2=c(4,3),
       count_3=c(6,3),
       count_4=c(8,4),
       count_5=c(7,6),
       period=c(8,8))

predict(model, new_data)




rm(list=ls(all.names=TRUE))
