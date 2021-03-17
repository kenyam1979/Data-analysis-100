library(tidyverse)

## 051: Data load
tbl_factory <- read_csv("sample_code/6章/tbl_factory.csv")
tbl_warehouse <- read_csv("sample_code/6章/tbl_warehouse.csv")
rel_cost <- read_csv("sample_code/6章/rel_cost.csv")
tbl_transaction <- read_csv("sample_code/6章/tbl_transaction.csv", 
                            col_types = cols(TransactionDate = col_datetime(format = "%Y-%m-%d %H:%M:%S")))


join_data <- left_join(tbl_transaction, rel_cost, by=c('ToFC'='FCID', 'FromWH'='WHID')) %>%
  left_join(tbl_factory, by=c('ToFC'='FCID')) %>%
  left_join(tbl_warehouse, by=c('FromWH'='WHID'))


## 052: Data summary
join_data %>% group_by(WHRegion) %>% summarize(cost=sum(Cost))

join_data %>% group_by(WHRegion) %>% summarize(quantity=sum(Quantity))

join_data %>% group_by(WHRegion) %>% summarize(perunit_cost=sum(Cost)/sum(Quantity)*10000)

join_data %>% group_by(WHRegion) %>% summarize(ave_cost=mean(Cost))


## 053: Network visualization basic
library(igraph)
library(ggraph)
library(tidygraph)

g <- data_frame(
  from=c('A', 'A', 'B'),
  to=c('B', 'C', 'C'))

gt <- as_tbl_graph(g, directed=FALSE) 


gt <- gt%>% 
  activate(nodes) %>% 
  mutate(name=c('A','B','C'))

gt %>%
  ggraph(layout = 'kk') +
  geom_edge_link(alpha=0.8, color="lightgray") + 
  scale_edge_width(range=c(0.1,1)) +
  geom_node_point(aes(size=4)) +
  geom_node_label(aes(label=name),repel=TRUE)


## 055: Network visualization basic - Edge weight
gt <- gt %>% activate(edges) %>% mutate(weight=c(3,2,1))

gt %>%
  ggraph(layout = 'kk') +
  geom_edge_link(aes(edge_width=weight), alpha=0.8, color="lightgray") + 
  geom_node_point(aes(size=4)) +
  geom_node_label(aes(label=name),repel=TRUE)



## 056: Data load for route analysis
trans_route <- read_csv("sample_code/6章/trans_route.csv") %>% rename(WH=工場)
trans_route_pos <- read_csv("sample_code/6章/trans_route_pos.csv")
trans_cost <- read_csv("sample_code/6章/trans_cost.csv")
demand <- read_csv("sample_code/6章/demand.csv")
supply <- read_csv("sample_code/6章/supply.csv")
trans_route_new <- read_csv("sample_code/6章/trans_route_new.csv") %>% rename(WH=工場)



## 057: Transportation route visualization
tr <- trans_route　%>% 
  pivot_longer(cols=-WH, names_to='FC', values_to='freq') %>%
  filter(freq>0) %>%
  as_tbl_graph(directed=FALSE)

tr %>%
  ggraph() +
  geom_edge_link(aes(edge_width=freq), alpha=0.8, color="lightgray") + 
  geom_node_point(aes(size=4)) +
  geom_node_label(aes(label=name),repel=TRUE)


## 058: Cost evaluation function
ttl_cost <- function(tr, tc) {
  sum(tr[,2:5] * tc[,2:5])
}
ttl_cost(trans_route, trans_cost)


## 059: Constraint check
demand <- data_frame(FC=colnames(demand), demand=as.numeric(demand))

trans_route　%>% 
  pivot_longer(cols=-WH, names_to='FC', values_to='freq') %>%
  group_by(FC) %>%
  summarize(supply=sum(freq)) %>%
  left_join(demand, by='FC') %>%
  mutate(check=if_else(supply>=demand, T, F))

supply <- data_frame(WH=colnames(supply), supply=as.numeric(supply))
trans_route　%>% 
  pivot_longer(cols=-WH, names_to='FC', values_to='freq') %>%
  group_by(WH) %>%
  summarize(demand=sum(freq)) %>%
  left_join(supply, by='WH') %>%
  mutate(check=if_else(supply>=demand, T, F))


## 060: Cost evaluation function(new data)
ttl_cost(trans_route_new, trans_cost)




rm(list=ls(all.names=TRUE))

## APPENDIX: igraph
m <- matrix(c(
  0,1,1,
  0,0,1,
  0,0,0
), ncol=3, nrow=3, byrow=TRUE)
g <- graph_from_adjacency_matrix(m)
plot(g)


## Reference
## https://www.slideshare.net/kashitan/tidygraphggraph-ver-152368322