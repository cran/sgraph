#' Modify the node size of a sgraph object.
#'
#' Modify the node size of an existing sgraph object by providing either:
#' (1) A single size to use for all nodes; (2) a vector of node sizes; or (3) a
#' metric to use to scale the nodes.
#'
#' @param sigma_obj   sgraph object, returned by sigma_from_igraph function
#' @param min_size    Minimum node size on the graph (for scaling)
#' @param max_size    Maximum node size on the graph (for scaling)
#' @param one_size    A single size to use for all nodes
#' @param size_vector An optional vector with the sizes for each node
#'
#' @return A sgraph object with modified node sizes
#'
#' @examples
#' library(igraph)
#' library(sgraph)
#'
#' data(lesMis)
#'
#' layout <- layout_nicely(lesMis)
#'
#' # one size for all nodes
#' sig <- sigma_from_igraph(igraph = lesMis, layout = layout) %>%
#'   add_node_size(one_size = 3)
#' sig
#'
#' # using a vector
#' custom_size <- log10(degree(lesMis))
#' sig <- sigma_from_igraph(igraph = lesMis, layout = layout) %>%
#'  add_node_size(size_vector = custom_size)
#' sig
#'
#' @export
add_node_size <- function(sigma_obj, min_size = 1, max_size = 3,
  one_size = NULL, size_vector = NULL) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  nodes <- json_obj$nodes
  
  if (!is.null(one_size)) {

    nodes$attributes$size = one_size
    sigma_obj$x$options$minNodeSize <- one_size
    sigma_obj$x$options$maxNodeSize <- one_size

  } else if (!is.null(size_vector)) {

    nodes$attributes$size <- size_vector
    sigma_obj$x$options$minNodeSize <- min_size
    sigma_obj$x$options$maxNodeSize <- max_size

  }

  update_sigma_json(sigma_obj, nodes, json_obj$edges, json_obj$directed)
}

#' Modify the node labels of a sgraph object.
#'
#' Modify the node labels of an existing sgraph object by providing an
#' attribute from the initial igraph to use as the labels.
#'
#' @param sigma_obj   sgraph object, returned by sigma_from_igraph function
#' @param label_attr  Attribute to use to replace node labels
#'
#' @return sgraph object with modified node labels
#'
#' @examples
#' library(igraph)
#' library(sgraph)
#'
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(igraph = lesMis) %>%
#'   add_node_labels(label_attr = 'label')
#' sig
#'
#' @export
add_node_labels <- function(sigma_obj, label_attr = NULL) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  nodes <- json_obj$nodes

  nodes$attributes$label <- if (length(label_attr) > 1) {
    concat_labels(sigma_obj$x$graph$vertices, label_attr)
  } else {
    as.character(sigma_obj$x$graph$vertices[, label_attr])
  }

  update_sigma_json(sigma_obj, nodes, json_obj$edges, json_obj$directed)
}


concat_labels = function(df_nodes, label_attr) {
  apply(df_nodes, 1, function(row) {
      paste0(names(label_attr), row[label_attr], collapse = '\n')
    })
}

#' Modify the edge size of a sgraph object.
#'
#' Modify the edge size of a sgraph by providing a single size
#'
#' @param sigma_obj sgraph object
#' @param one_size  A single size to use for all edges
#'
#' @return sgraph with modified edge sizes
#'
#' @examples
#' library(igraph)
#' library(sgraph)
#'
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(igraph = lesMis) %>%
#'   add_edge_size(one_size = 5)
#' sig
#'
#' @export
add_edge_size <- function(sigma_obj, one_size = NULL) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  edges <- json_obj$edges

  if (one_size == 0) edges$attributes$hidden = TRUE
  edges$attributes$size <- one_size
  sigma_obj$x$options$minEdgeSize <- one_size
  sigma_obj$x$options$maxEdgeSize <- one_size

  update_sigma_json(sigma_obj, json_obj$nodes, edges, json_obj$directed)
}

#' Modify the edge zIndex of a sgraph object.
#'
#' Modify the edge zIndex
#'
#' @param sigma_obj sgraph object
#' @param zindex    Zindex value, larger is drawn above.
#'
#' @return sgraph
#'
#' @examples
#' library(igraph)
#' library(sgraph)
#'
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(igraph = lesMis) %>%
#'   add_edge_zindex(zindex = 2)
#' sig
#'
#' @export
add_edge_zindex <- function(sigma_obj, zindex) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  edges <- json_obj$edges

  edges$attributes$zIndex <- zindex

  update_sigma_json(sigma_obj, json_obj$nodes, edges, json_obj$directed)
}

#' Modify the edge colors of a sgraph object.
#'
#' Modify the edge colors of a sgraph object by providing a single color.
#' Also works with a vector of correct size.
#'
#' @param sigma_obj     sgraph object
#' @param one_color     A single color to color all of the nodes (hex format)
#' @param color_attr    The name of an edge attribute
#' @param color_palette Name of RColorBrewer palette to use
#'
#' @return sgraph with modified edge colors
#'
#' @examples
#' library(igraph)
#' library(sgraph)
#'
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(igraph = lesMis) %>%
#'   add_edge_color(one_color = "#ccc")
#' sig
#'
#' @export
add_edge_color <- function(sigma_obj, one_color = NULL, color_attr = NULL,
                           color_palette = 'Set2') {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  edges <- json_obj$edges

  if (is.null(one_color)) {
    temp_col <- edges[, color_attr]
    uniq_cols = unique(temp_col)
    n_uniq_cols = length(uniq_cols)

    # If there are more edge colors than colors in the chosen palette,
    # interpolate colors to expand the palette
    palette = RColorBrewer::brewer.pal(8, color_palette)
    pal <- tryCatch(RColorBrewer::brewer.pal(n_uniq_cols, color_palette),
      warning = function(w) grDevices::colorRampPalette(palette)(n_uniq_cols))

    df_palette <- data.frame(group = uniq_cols,
                             color = utils::head(pal, n_uniq_cols))
    group_idxs = match(temp_col, df_palette$group)
    edges$attributes$color <- df_palette$color[group_idxs]
  } else {
    edges$attributes$color <- one_color
  }

  update_sigma_json(sigma_obj, json_obj$nodes, edges, json_obj$directed)
}

update_sigma_json = function(sigma_obj, nodes, edges, directed) {

  graph <- list(nodes = nodes, edges = edges, directed = directed)
  sigma_obj$x$data <- jsonlite::toJSON(graph, auto_unbox = TRUE)

  sigma_obj
}

#' Format multiline labels
#'
#' The sgraph R package extends the sigma.js library to enable multiline labels.
#' The Javascript functions will start new lines on line breaks ('\\n') and
#' this function enables to easily format the details of nodes by showing the
#' value of the relationship (edge weights) it has with other nodes.
#' See the Shiny examples for use cases.
#'
#' @param df_nodes        Nodes data frame of a sgraph object
#' @param display_val_str String that will be prepended to each edge weight
#' @param replace_codes   Should the label of the node replace the id ?
#' @param label_str       String that will pe prepended to the node label
#' @param group_str       String that will pe prepended to the node group
#'
#' @return sgraph object with modified node hidden atribute 
#'
#' @export
multiline_labels = function(df_nodes, display_val_str = '\nP-value: ',
                            replace_codes = TRUE, label_str = 'Label: ',
                            group_str = 'Group: ') {

  if (!replace_codes) {
    df_nodes$label = df_nodes$desc %>%
      { ifelse(df_nodes$word != ., paste0(label_str, ., '\n'), '') }

    df_nodes$label %<>% paste0(group_str, df_nodes$clusters)

  } else {
    df_nodes$label = df_nodes$desc
  }

  val_labels = df_nodes$display_val %>%
      { ifelse(!is.na(.), paste0(display_val_str, df_nodes$display_val), '') }

  df_nodes$label %<>% paste0(val_labels)

  df_nodes
}

#' Modify the node visibility of a sgraph object.
#'
#' Modify the node hidden attribute of an existing sgraph object.
#' The sgraph R package extends the sigma.js library to enable hidden nodes
#' that will appear upon clicking on their parent group.
#'
#' @param sigma_obj   sgraph object, returned by sigma_from_igraph function
#' @param hidden_attr Attribute to use to set node hidden value
#'
#' @return sgraph object with modified node hidden atribute 
#'
#' @export
add_node_hidden = function(sigma_obj, hidden_attr) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  nodes <- json_obj$nodes
  nodes$attributes$hidden <- hidden_attr

  update_sigma_json(sigma_obj, nodes, json_obj$edges, json_obj$directed)
}

