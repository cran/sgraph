#' @import htmlwidgets
#' @importFrom cowplot get_legend
#' @importFrom ggplot2 aes element_text ggplot geom_point scale_color_manual
#'                     theme theme_bw
#' @importFrom grDevices colorRampPalette
#' @importFrom igraph as_data_frame betweenness closeness degree
#'                    graph_from_data_frame is.directed
#'                    layout_nicely vertex_attr edge_attr
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom magrittr %>% %<>% %$%
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stats setNames lm
#' @importFrom utils combn head tail
NULL

#' Pipe
#'
#' Pipe an object forward into a function or call expression.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return Result of rhs applied to lhs, see details in magrittr package.
#' @export
NULL

#' Assignment pipe
#'
#' Pipe an object forward into a function or call expression and update the
#' `lhs` object with the resulting value.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %<>%
#' @name %<>%
#' @rdname compound
#' @param lhs An object which serves both as the initial value and as target.
#' @param rhs a function call using the magrittr semantics.
#' @return None, used to update the value of lhs.
#' @export
NULL

#' Exposition pipe
#'
#' Expose the names in `lhs` to the `rhs` expression.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %$%
#' @name %$%
#' @rdname exposition
#' @param lhs A list, environment, or a data.frame.
#' @param rhs An expression where the names in lhs is available.
#' @return Result of rhs applied to one or several names of lhs.
#' @export
NULL

