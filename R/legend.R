
#' Get the legend for a sgraph network
#'
#' @param colors_map Color mapping to use, typically built by `get_color_map`
#' @param clusters  Group names
#'
#' @return Ggplot object
#'
#' @export
get_legend = function(colors_map, clusters) {

  colors_map %<>% subset(group %in% clusters)
  colors_map %<>% cbind(data.frame(x = 1, y = 1))
  colors_map$group %<>% factor(unique(.))

  gglegend = ggplot2::ggplot(colors_map, ggplot2::aes(x, y, color = group)) +
      ggplot2::geom_point(size = 10) +
      ggplot2::scale_color_manual(name = NULL, values = colors_map$color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.text.position = 'top',
                     legend.title = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size = 15))

  cowplot::get_legend(gglegend)
}

interpolate_palette = function(n_unique, palette) {

  if (n_unique <= length(palette)) return(utils::tail(palette, n_unique))

  grDevices::colorRampPalette(palette)(n_unique)
}

#' Build a color map
#'
#' @param colors Groups that will be assigned to colors
#' @param palette Palette to use, typically a RColorBrewer palette
#'
#' @return Data frame mapping group names to colors
#'
#' @export
get_color_map = function(colors,
  palette = RColorBrewer::brewer.pal(8, 'Dark2')) {

  unique_colors = unique(colors)
  palette = interpolate_palette(length(unique_colors), palette)

  data.frame(group = unique_colors,
             color = palette[seq_along(unique_colors)],
             stringsAsFactors = FALSE)
}

