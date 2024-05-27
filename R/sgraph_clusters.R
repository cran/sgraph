                                                                               
#' Build a sgraph object colored by clusters
#'
#' Wrapper function to build a sigma.js visualization of an igraph object and
#' color it using a 'clusters' attribute. It calls the sigma_from_igraph
#' function, and manages labels, node sizes, color mapping, layouts, and
#' optionally arrows. The clusters attribute can be also be disabled to just
#' use the wrapper to manage the other attributes.
#'
#' @param igraph      Igraph object
#' @param color_map   Use a color mapping to select colors (enables to link the
#'                    graph with other plots) as returned by the get_color_map
#'                    function. Leave NULL for automatic colors (default).
#' @param label       Name of the igraph attribute to use as labels
#' @param clusters    Whether or not to use a column named clusters to color
#'                    the nodes.
#' @param arrows      Whether or not to display arrows on directed edges.
#' @param node_size   Passed to add_node_size function, either a numeric or an
#'                    attribute name. Default is NULL.
#' @param layout      Output of an igraph layout (default: layout_with_fr)
#' @param ...         Passed to sigma_from_igraph
#'
#' @return Htmlwidget object
#'
#' @export
sgraph_clusters = function(igraph, color_map = NULL,                              
                           label = 'name', clusters = TRUE, arrows = FALSE,        
                           node_size = NULL,                                       
                           layout = igraph::layout_with_fr(igraph),                
                           ...) {                                                  
                                                                                
  sigma_graph = sigma_from_igraph(igraph, layout = layout, ...)                 
                                                                                
  if (clusters == TRUE) {                                                       
      sigma_graph %<>% sigma_nodes_colors('clusters', color_map)                
  }                                                                             
                                                                                
  if (length(label) > 1 || label != 'name') {                                   
      sigma_graph %<>% add_node_labels(label)                                   
  }                                                                             
                                                                                
  if (!is.null(node_size)) {                                                    
      if (is.numeric(node_size)) {                                              
        sigma_graph %<>% add_node_size(one_size = node_size)                    
      } else {                                                                  
        sigma_graph %<>%                                                        
            add_node_size(size_vector = igraph::vertex_attr(igraph, node_size)) 
      }                                                                         
  }                                                                             
                                                                                
  sigma_graph                                                                   
}                                                                               



igraph_clusters = function(l_graph, largest_connected = TRUE, min_degree = 0,
  max_nodes = 1500) {

  igraph = igraph::graph_from_data_frame(l_graph$df_links)

  # remove isolated nodes
  if (min_degree > 0) {
    igraph %<>% igraph::delete_vertices(
      which(igraph::centralization.degree(.)$res < min_degree))
  }

  # remove using modularity
  if (length(igraph::V(igraph)) > max_nodes) {
    metric = igraph::cluster_walktrap(igraph)$modularity
    threshold = sort(metric, decreasing = TRUE)[max_nodes]
    igraph %<>% igraph::delete_vertices(which(metric < threshold))
  }

  # keep only largest connected graph
  if (largest_connected) {
    igraph %<>% igraph::components() %$%
      which(membership != which.max(csize)) %>% names %>%
      igraph::delete_vertices(igraph, .)
  }

  igraph %<>% igraph::simplify()

  if ('df_nodes' %in% names(l_graph)) {
      igraph %<>% add_igraph_info(l_graph$df_nodes)
  }

  igraph
}

# can probably remove and use add_node_color instead
sigma_nodes_colors = function(sigmaObj, attr_name = NULL, color_map = NULL,
  palette = RColorBrewer::brewer.pal(8, 'Dark2')) {

  json_data = jsonlite::fromJSON(sigmaObj$x$data)

  colors = sigmaObj$x$graph$vertices[, attr_name]
  if (is.null(color_map)) color_map = get_color_map(colors, palette)

  json_data$nodes$attributes$color = color_map %$% color[match(colors, group)]

  sigmaObj$x$data <- jsonlite::toJSON(json_data, pretty = TRUE,
                                      auto_unbox = TRUE)
  sigmaObj
}


