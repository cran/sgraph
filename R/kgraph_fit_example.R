                                                                            

                                                                               
#' Kgraph fit to graph list object
#'
#' Example function to build a graph list object (list of nodes and links data
#' frames) from a kgraph object (embeddings with cosine similarity cut-off
#' based on random null concept pairs and known related concept pairs)
#'
#' @param l_fit_embeds kgraph object: embeddings with cosine similarity cut-off
#'                     based on random null concept pairs and known related
#'                     concept pairs
#'
#' @return graph list object: list of nodes and links data frames
#'
#' @export
kgraph_to_lgraph = function(l_fit_embeds) {                                     
                                                                                
  df_projs = l_fit_embeds$df_projs                                              
                                                                                
  df_projs %<>% order_dataframe_sgraph(relevant_pattern = 'suicid')                    
                                                                                
  df_nodes = data.frame(name = unique(unlist(df_projs[1:2])))                   
  # color by CUIs                                                               
  df_nodes$clusters = grepl('[)]$', df_nodes$name)                              
                                                                                
  stats::setNames(df_projs, c(1:2, 'weight')) %>%                                      
      list(df_links = ., df_nodes = df_nodes)                                   
}                                                                               
                                                                                


order_dataframe_sgraph = function(df_x, cols = 1:2, relevant_pattern = NULL) {

  # put strings matching relevant patterns in first column
  if (!is.null(relevant_pattern)) {

    df_x %<>% subset(grepl(relevant_pattern, .[[cols[1]]]) |
                     grepl(relevant_pattern, .[[cols[2]]]))

    uniq_lvls = unique(unlist(df_x[cols]))
    relevant_lvls = grep(relevant_pattern, uniq_lvls)
    uniq_lvls = c(uniq_lvls[relevant_lvls], uniq_lvls[-relevant_lvls])

    df_x[cols] %<>% lapply(function(lvls) factor(lvls, uniq_lvls) %>%
                           as.numeric)
    df_x %<>% order_dataframe_sgraph
    df_x[cols] %<>% lapply(function(lvls) uniq_lvls[lvls])

  } else {
    df_x[cols] = apply(df_x[cols], 1, sort) %>%
      t %>% as.data.frame %>% setNames(names(df_x[cols]))
  }

  df_x
}

