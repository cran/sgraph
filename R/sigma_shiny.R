#' Create a UI element for a sgraph visualization in Shiny
#'
#' @param outputId ID of the UI element
#' @param width    Width of the UI element
#' @param height   Height of the UI element
#'
#' @return Htmlwidgets output object
#'
#' @export
sgraphOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, 'sgraph', width, height,
                    package = 'sgraph')
}

#' Render a sgraph visualization in Shiny
#'
#' @param expr   An expression that creates a sgraph visualization
#' @param env    Defaults to parent.frame() (cf. Shiny docs)
#' @param quoted Defaults to FALSE (cf. Shiny docs)
#'
#' @return Htmlwidgets render object
#'
#' @export
renderSgraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  shinyRenderWidget(expr, sgraphOutput, env, quoted = TRUE)
}

#' Build a sgraph object from an igraph object
#'
#' Basic sigma.js visualization of an igraph object, with pipeable syntax.
#'
#' @param igraph      Igraph object
#' @param layout      Output of an igraph layout (default: layout_nicely)
#' @param label_color Hex color for labels
#' @param width       Width of the output graph (default: fit container)
#' @param height      Height of the output graph (default: fit container)
#' @param elementId   Do not specify, used by the htmlwidgets package
#' @param label_grid_cell_size Sigma.js corresponding parameter. Roughly goes
#'                             from 1 to 5000, the smaller the more labels 
#'                             displayed.
#'
#' @return Htmlwidget object, meant to be called directly to render a default
#'   visualization, or passed to other functions to change attributes
#'   (colors, sizes, interactivity, etc.).
#'
#' @examples
#' library(sgraph)
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(igraph = lesMis)
#' sig
#' @export
sigma_from_igraph <- function(igraph, layout = NULL, label_color = '#fff',
                              width = NULL, height = NULL, elementId = NULL,
                              label_grid_cell_size = 200) {

  directed_flag <- igraph::is_directed(igraph)
  graph_parse <- igraph::as_data_frame(igraph, what = 'both')

  edges <- graph_parse$edges[, c('from', 'to')]
  edges[c('from', 'to')] %<>% lapply(as.character)
  colnames(edges) <- c('source', 'target')
  edges$key <- seq_len(nrow(edges))
  edges$size <- 1
  edges$color <- '#636363'

  nodes <- graph_parse$vertices
  nodes$label <- row.names(nodes)
  layout = if (is.null(layout)) igraph::layout_nicely(igraph) else layout
  nodes <- cbind(nodes[, 'label', drop = FALSE], layout)
  colnames(nodes) <- c('label', 'x', 'y')

  nodes$key <- seq_len(nrow(nodes))
  nodes$size <- 3
  nodes[c('x', 'y')] %<>% lapply(as.numeric)
  nodes$color <- '#3182bd'

  edges$source %<>% match(nodes$label) %>% `[`(nodes$key, .)
  edges$target %<>% match(nodes$label) %>% `[`(nodes$key, .)

  nodes$label %<>% as.character
  edges[c('source', 'target')] %<>% lapply(as.character)

  # adapt changes for v2.4.0
  nodes %<>% graph_to_json 
  edges %<>% graph_to_json('edges')

  graph_out <- list(nodes = nodes, edges = edges, directed = directed_flag)

  sigma_opts <- list(min_node_size = 1, max_node_size = 3, min_edge_size = 1,
                  max_edge_size = 3, neighborEvent = 'onClick',
                  neighborStart = 'clickNode', neighborEnd = 'clickStage',
                  doubleClickZoom = TRUE, mouseWheelZoom = TRUE,
                  edge_arrows = 'def', label_color = label_color,
                  label_grid_cell_size = label_grid_cell_size)

  graph_json <- jsonlite::toJSON(graph_out, auto_unbox = TRUE)
  sigma_obj <- list(data = graph_json, options = sigma_opts,
                    graph = graph_parse)

  createWidget(name = 'sgraph', sigma_obj, width = width, height = height,
               package = 'sgraph', elementId = elementId)
}

# separate df in keys fields and attributes
graph_to_json = function(df_input, type = c('nodes', 'edges')) {

  type = match.arg(type)
  fields = switch(type, nodes = 'key',
                  edges = c('key', 'source', 'target'))
  num_attrs = switch(type, nodes = c('x', 'y', 'size'), edges = 'size')
  
  names_attr = setdiff(names(df_input), fields)

  # t breaks if not characters
  df_input$key %<>% as.character
  df_json = as.data.frame(trimws(t(df_input))) %>%
      lapply(graph_row_to_json, names(df_input), fields, names_attr, num_attrs)

  stats::setNames(df_json, NULL)
}

graph_row_to_json = function(vec, names_df, fields, names_attr, num_attrs) {

  vec = stats::setNames(vec, names_df)
  vec = append(as.list(vec[fields]),
               list(attributes = as.list(vec[names_attr])))

  vec$attributes[num_attrs] = lapply(vec$attributes[num_attrs], as.numeric)

  vec 
}
