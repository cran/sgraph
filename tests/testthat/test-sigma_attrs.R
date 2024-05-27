
test_add_node_size = function() {
   layout <- layout_nicely(lesMis)
  
   # one size for all nodes
   sig <- sigma_from_igraph(igraph = lesMis, layout = layout) %>%
     add_node_size(one_size = 3)
   sig
  
   # using a vector
   custom_size <- log10(degree(lesMis))
   sig <- sigma_from_igraph(igraph = lesMis, layout = layout) %>%
    add_node_size(size_vector = custom_size)
  expect_true(TRUE)
}
test_that('add_node_size', test_add_node_size())


test_add_node_labels = function() {
   sig <- sigma_from_igraph(igraph = lesMis) %>%
     add_node_labels(label_attr = 'label')
  expect_true(TRUE)
}
test_that('add_node_labels', test_add_node_labels())



test_add_edge_size = function() {
   sig <- sigma_from_igraph(igraph = lesMis) %>%
     add_edge_size(one_size = 5)
  expect_true(TRUE)
}
test_that('add_edge_size', test_add_edge_size())


test_add_edge_zindex = function() {
  sig <- sigma_from_igraph(igraph = lesMis) %>%
    add_edge_zindex(zindex = 2)
  expect_true(TRUE)
}
test_that('add_edge_zindex', test_add_edge_zindex())



test_add_edge_color = function() {
   sig <- sigma_from_igraph(igraph = lesMis) %>%
     add_edge_color(one_color = "#ccc")
  expect_true(TRUE)
}
test_that('add_edge_color', test_add_edge_color())

