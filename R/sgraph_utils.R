
if (getRversion() >= "2.15.1") {
  vars <- c('.', 'group', 'x', 'y', 'from', 'to', 'membership', 'csize',
            'color')
  utils::globalVariables(vars)
}

#' Add nodes information to the igraph object
#'
#' Modify the node attributes of an existing igraph object by providing a
#' dataframe
#'
#' @param igraph   Igraph object to modify
#' @param df_nodes Data frame to add to nodes
#' @param fields   Columns of df_nodes to add.
#'                 First must be the node identifier.
#'
#' @return A sgraph object with modified node labels
#'
#' @examples
#' library(igraph)
#' library(sgraph)
#' data(lesMis)
#'
#' df_nodes = cbind.data.frame(name = igraph::vertex_attr(lesMis, 'label'),
#'   log10_degree = degree(lesMis))
#'
#' igraph = add_igraph_info(lesMis, df_nodes)
#'
#' sig <- sigma_from_igraph(lesMis) %>%
#'   add_node_size(size_vector = 'log10_degree')
#'
#' @export
add_igraph_info = function(igraph, df_nodes, fields = names(df_nodes)) {

  df_igraph = data.frame(name = vertex_attr(igraph)[[1]])

  df_igraph %<>% merge(df_nodes[fields], by.x = 'name', by.y = fields[1],
      all.x = TRUE, all.y = FALSE, sort = FALSE)

  param_names = names(df_igraph)[-1]

  for (param in param_names) {
    igraph %<>% igraph::set_vertex_attr(param, value = df_igraph[[param]])
  }

  igraph
}


#' Highlight edges of multiple connected nodes
#'
#' Using a selection of nodes, highlight edges linking to nodes that are
#' connected to several nodes from the selection.
#' Differentiate multiple connected and fully connected (all selected nodes).
#' Use a maximum number of connected nodes to use lighter colored edges
#' (default 20).
#'
#' @param df_links       Links data frame of a sgraph object
#' @param selected_nodes Nodes identifiers to be used for the selection.
#' @param n_max          Maximum number of connected nodes, to use either
#'                       lighter or darker color sets for edges (default 20).
#' @param light_cols     Three hex values for colors to use with n_max.
#' @param dark_cols      Three hex values for colors to use with n_max.
#'
#' @return Links data frame of a sgraph object
#' @export
highlight_multiple_connected = function(df_links, selected_nodes, n_max = 20,
  dark_cols = c('#ddd', '#444', '#444'),
  light_cols = c('#efefef', '#ddd', '#bbb')) {

  df_links_pheno = subset(df_links, from %in% selected_nodes)

  if (is.null(names(selected_nodes))) {

    full_nodes = df_links_pheno %$%
      { names(which(table(to) == length(selected_nodes))) }
 
    mult_nodes = df_links_pheno %$%
      { names(which(table(to) > 1)) }

  } else {

    df_links_pheno$group = df_links_pheno %$%
        stringi::stri_replace_all_fixed(from, selected_nodes,
                                        names(selected_nodes),
                                        vectorize_all = FALSE)

    df_group_uniq = unique(df_links_pheno[c('group', 'to')])
    full_nodes = df_group_uniq %$%
      { names(which(table(to) == length(unique(group)))) }
 
    mult_nodes = df_group_uniq %$%
      { names(which(table(to) > 1)) }
  }

  df_links$color = if (length(mult_nodes) > n_max) light_cols[1] else dark_cols[1]
  mult_highlight = if (length(mult_nodes) > n_max) light_cols[3] else dark_cols[3]
  full_highlight = if (length(full_nodes) > n_max) light_cols[2] else dark_cols[2]

  df_links$color[df_links$to %in% mult_nodes] = mult_highlight
  df_links$color[df_links$to %in% full_nodes] = full_highlight

  df_links$zindex = (df_links$to %in% full_nodes) +
      (df_links$to %in% mult_nodes)

  df_links
}

#' Convert weights for spring layout
#'
#' Apply spring weights (revert weights).
#' Can add weak links to selected nodes, but best without.
#'
#' @param df_links       Links data frame of a sgraph object
#' @param selected_nodes Nodes identifiers to be used for the selection.
#'
#' @return Links data frame of a sgraph object
#' @export
convert_to_spring_weights = function(df_links, selected_nodes = NULL) {

  df_links$weight %<>% { max(.) - . + 1}
  if (length(selected_nodes) <= 1) return(df_links)

  df_links_targets = if (length(selected_nodes) == 2) {
      as.list(selected_nodes)
  } else {
      utils::combn(selected_nodes, 2) |> t()
  }
  df_links_targets %<>% as.data.frame %>% stats::setNames(c('from', 'to'))

  df_links_targets$weight = max(df_links$weight)# +
 #   diff(range(df_links$weight))

  df_links %>% rbind(df_links_targets)
}

#' Scale weights
#'
#' Use non-centered scaling, multiplied by a constant, and set a lower bound
#' constant.
#'
#' @param weights           Either nodes or links weights vector
#' @param upper_bound_mult  Constant to multiply weights by after scaling.
#'                          Use to set an upper bound for weights.
#' @param lower_bound_const Constant to set a lower bound for weights.
#'                          All weights below will be set to lower bound.
#'
#' @return Weights vector
#' @export
scale_graph = function(weights,
  upper_bound_mult = if (length(weights) > 1e3) 2 else 4,
  lower_bound_const = if (length(weights) > 1e3) 2 else 3) {

  wts = scale(weights, center = FALSE)
  wts = wts * upper_bound_mult
  ifelse(wts < lower_bound_const, lower_bound_const, wts)
}
