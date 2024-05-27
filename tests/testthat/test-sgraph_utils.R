
test_add_igraph_info = function() {
  df_nodes = cbind.data.frame(name = igraph::vertex_attr(lesMis, 'label'),
    log10_degree = degree(lesMis))
 
  igraph = add_igraph_info(lesMis, df_nodes)
 
  sig <- sigma_from_igraph(lesMis) %>%
    add_node_size(size_vector = 'log10_degree')
  expect_true(TRUE)
}
test_that('add_igraph_info', test_add_igraph_info())

