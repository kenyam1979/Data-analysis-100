library(tidyverse)
library(tidymodels)

## 041: Data preparation
customer_join <- read_csv("Chapter3/customer_join.csv", 
                          col_types = cols(start_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
                                           end_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")))
use_log <- read_csv("sample_code/3章/use_log.csv", 
                    col_types = cols(usedate = col_date(format = "%Y-%m-%d")))
### Cutoff month
tmp <- use_log %>% 
  mutate(usemonth=format(usedate, '%Y%m')) %>% 
  group_by(customer_id, usemonth) %>% 
  summarize(n=n())
rm(use_log)
tmp_data <- tmp %>% filter(usemonth>='201805') %>% mutate(cutoff=usemonth, count='count_0')
months <- sort(unique(tmp$usemonth), decreasing=TRUE)
### Month of cutoff - 1
for (i in 1:11) {
  cutoff = months[i]
  mon = months[i+1]
  tmp_filtered <- tmp %>% filter(usemonth==mon) %>% mutate(cutoff=cutoff, count='count_1')
  tmp_data <- union(tmp_data, tmp_filtered)
}
uselog <- tmp_data %>% pivot_wider(id_cols=c(cutoff, customer_id), names_from=count, values_from=n) %>% arrange(cutoff)
rm(list=c('tmp', 'tmp_filtered', 'tmp_data', 'cutoff', 'i', 'mon', 'months'))


## 042: Prepare data for exit customers
exit_uselog <- left_join(uselog, customer_join, by='customer_id') %>%
  filter(is_deleted==1) %>% 
  filter(cutoff==format(as.Date(end_date) - 31, '%Y%m'))


## 043: Prepare data for continuing customers
conti_uselog <- left_join(uselog, customer_join, by='customer_id') %>% 
  filter(is_deleted==0)
### Under sampling
conti_uselog <- conti_uselog[sample(nrow(conti_uselog), nrow(conti_uselog)),] %>% distinct(customer_id, .keep_all=TRUE)
### Data union
predict_data <- union(exit_uselog, conti_uselog)


## 044: Enrich data
predict_data <- predict_data %>% mutate(period=round(as.numeric(difftime(end_date-months(1), start_date))/30)) 


## 045: Omit NA
summary(predict_data)
predict_data <- predict_data %>% filter(!is.na(count_1))


## 046: Dummy variables
## *This process is not necessary for rpart
preprocess <- predict_data %>% 
  recipe(is_deleted~campaign_name+class_name+gender+count_1+routine_flg+period) %>%
  step_dummy(campaign_name, class_name, gender, one_hot=TRUE) %>%
  prep()
predict_data_encoded <- bake(preprocess, new_data=predict_data)


## 047: Modeling
library(rpart)
library(rpart.plot)
### Decision tree
model <- rpart(is_deleted~campaign_name+class_name+gender+count_1+routine_flg+period, predict_data, method='class')
summary(model)
### Draw a tree
rpart.plot(model)


## 048: Model evaluation
### Adjusting complexity parameter(CP)
plotcp(model)
model2 <- prune(model, cp=0.14)
rpart.plot(model2)

### Simple accuracy check
y <- predict_data$is_deleted
y_pred <- if_else(predict(model)[,2]>0.5, 1,0) 
t <- table(y_pred, y)
sum(diag(t))/sum(t)

y_pred2 <- if_else(predict(model2)[,2]>0.5, 1,0) 
t <- table(y_pred2, y)


## 049: Importance
model$variable.importance


## 050: Prediction
new_data <- tibble(campaign_name='入会費無料', class_name='オールタイム', gender='M', count_1=3, routine_flg=1, period=10)
#new_data <- bake(preprocess, new_data)
predict(model, new_data)
predict(model2, new_data)

rm(list=ls(all.names=TRUE))
