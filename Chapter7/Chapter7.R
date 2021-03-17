library(tidyverse)
library(linprog)
library(igraph)
library(ggraph)
library(tidygraph)

## 061: Optimization - Transportation network
trans_route <- read_csv("sample_code/6章/trans_route.csv") %>% rename(WH=工場)
trans_cost <- read_csv("sample_code/6章/trans_cost.csv") %>% rename(WH=工場)
demand <- read_csv("sample_code/6章/demand.csv")
supply <- read_csv("sample_code/6章/supply.csv")

cvec <- trans_cost %>%
  pivot_longer(cols=-WH, names_to='FC', values_to='cost') %>%
  pull(cost)

Amat <- matrix(c(
  1,1,1,1,0,0,0,0,0,0,0,0,  # Supply upper limit by WH1
  0,0,0,0,1,1,1,1,0,0,0,0,  # Supply upper limit by WH2
  0,0,0,0,0,0,0,0,1,1,1,1,  # Supply upper limit by WH3
  1,0,0,0,1,0,0,0,1,0,0,0,  # Demand lower limit by FC1
  0,1,0,0,0,1,0,0,0,1,0,0,  # Demand lower limit by FC2
  0,0,1,0,0,0,1,0,0,0,1,0,  # Demand lower limit by FC3
  0,0,0,1,0,0,0,1,0,0,0,1   # Demand lower limit by FC4
), ncol=12, byrow=TRUE)

bvec <- c(as.numeric(supply), as.numeric(demand))

const.dir <- c('<=',
               '<=',
               '<=',
               '>=',
               '>=',
               '>=',
               '>=')

s <- solveLP(cvec=cvec, bvec=bvec, Amat=Amat, const.dir=const.dir)
s

## 062: Optimized network
tr <- trans_route　%>% 
  pivot_longer(cols=-WH, names_to='FC', values_to='freq') %>% 
  mutate(new_freq=s$solution) %>%
  filter(new_freq>0) %>%
  as_tbl_graph(directed=FALSE) %>% 
  ggraph(layout = 'kk') +
  geom_edge_link(aes(edge_width=new_freq), alpha=0.8, color="lightgray") + 
  geom_node_point(aes(size=4)) +
  geom_node_label(aes(label=name),repel=TRUE)


rm(list=ls(all.names=TRUE))

## 064: Optimization - Product mix
product_plan_material <- read_csv("sample_code/7章/product_plan_material.csv")
product_plan_profit <- read_csv("sample_code/7章/product_plan_profit.csv") 
product_plan_stock <- read_csv("sample_code/7章/product_plan_stock.csv")

cvec <- pull(product_plan_profit[,2])
cvec <- -cvec

Amat <- t(product_plan_material[,2:4])

bvec <- as.numeric(product_plan_stock[,2:4])

const.dir <- c('<=',
               '<=',
               '<=')

s <- solveLP(cvec=cvec, bvec=bvec, Amat=Amat, const.dir=const.dir)
s


rm(list=ls(all.names=TRUE))


## Reference
## https://tjo.hatenablog.com/entry/2014/12/19/190525