## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(sgraph)
library(igraph)

data(lesMis)
class(lesMis)
names(vertex_attr(lesMis))
names(edge_attr(lesMis))

## -----------------------------------------------------------------------------
sig <- sigma_from_igraph(lesMis)
sig

## -----------------------------------------------------------------------------
sig %>% add_node_size(one_size = 7)

## -----------------------------------------------------------------------------
df_nodes = cbind.data.frame(name = vertex_attr(lesMis, 'id'),
  degree = degree(lesMis))

# seems sigma.js is not scaling automatically with min_size and max_size
# do it manually for now
df_nodes$degree %<>% scale(center = FALSE) %>% `*`(3) %>% `+`(3)

igraph = add_igraph_info(lesMis, df_nodes)

sig <- sigma_from_igraph(lesMis) %>%
 add_node_size(size_vector = vertex_attr(igraph, 'degree'), min_size = 3, max_size = 8)
sig

## -----------------------------------------------------------------------------
sig %>%
  add_node_labels(label_attr = 'label')

## -----------------------------------------------------------------------------
sig %>%
  add_edge_size(one_size = 5)

## -----------------------------------------------------------------------------
sig %>%
  add_edge_color(one_color = "#ccc")

