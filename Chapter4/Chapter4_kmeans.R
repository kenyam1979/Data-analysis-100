library(tidyverse)
library(tidymodels)

## 031: Data loading
customer_join <- read_csv("Chapter3/customer_join.csv", 
                          col_types = cols(start_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"), 
                                           end_date = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ")))

summary(customer_join)

## 032: K-means
customer_clustering <- customer_join[,c('mean', 'median', 'max', 'min', 'membership_period')]
customer_clustering <- customer_clustering %>% rename(monthly_visits_mean=mean, monthly_visits_median=median, monthly_visits_max=max, monthly_visits_min=min)
summary(customer_clustering)

### Scale variables
customer_clustering_s <- customer_clustering %>% 
  recipe(~monthly_visits_mean + monthly_visits_median + monthly_visits_max + monthly_visits_min + membership_period) %>%
  step_normalize(monthly_visits_mean, monthly_visits_median, monthly_visits_max, monthly_visits_min, membership_period) %>%
  prep() %>%
  bake(new_data=customer_clustering)
summary(customer_clustering_s)

### Perform k-mean
kmeans <- kmeans(customer_clustering_s, centers=4, nstart=10)


## 033: K-means result analysis
### Sample distribution at each cluster
kmeans$size

## Centers for each cluster
customer_clustering$cluster <- kmeans$cluster
customer_clustering %>% group_by(cluster) %>% summarize_all(mean)


## 034: Visualization with PCA
pc <- princomp(~monthly_visits_mean + monthly_visits_median + monthly_visits_max + monthly_visits_min + membership_period, customer_clustering_s)
summary(pc)
pc$loadings
tibble(as_tibble(pc$scores), cluster=factor(kmeans$cluster)) %>%
  ggplot() + geom_point(aes(x=Comp.1, y=Comp.2, color=cluster))


## 035: K-means result profiling 
customer_clustering <- tibble(customer_join, cluster=factor(kmeans$cluster))

customer_clustering %>% group_by(cluster, is_deleted) %>% summarize(n())
customer_clustering %>% group_by(cluster, routine_flg) %>% summarize(n())


rm(list=ls(all.names=TRUE))
