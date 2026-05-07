# binned scatter plot
binned_scatter_graph <- function(data = rdd,
                                 cut = 0.05,
                                 out = outcome,
                                 zoom_upper = NULL,
                                 zoom_lower = NULL,
                                 y_axis = NULL, #y.axis.title,
                                 graph_caption = NULL #graph.cap
) {
  
  class_var = typeof(data %>% pull(!!out))
  
  data = data %>%
    mutate(moy1 = round(moy1, 2),
           moy_cut = as.numeric(as.character(cut(moy1, breaks = seq(10,20 + cut,cut), labels = seq(10,20 + cut,cut)[-length(seq(10,20 + cut,cut))], include.lowest = TRUE, right = FALSE)))) %>%
    group_by(moy_cut) %>%
    summarise(proba = if_else(class_var == "logical", mean(get(out) == TRUE, na.rm = T), mean(get(out), na.rm = T)),
              n_obs = n()) %>% 
    filter(!is.na(moy_cut))
  
  if(!is.null(zoom_upper) & !is.null(zoom_lower)) {
    data = data %>%
      filter(moy_cut >= zoom_lower & moy_cut <= zoom_upper)
  }
  
  min_x = max(10, min(data$moy_cut, na.rm = T))
  max_x = min(20, max(data$moy_cut, na.rm = T))
  
  graph = data %>%
    filter(moy_cut < max_x) %>% 
    ggplot() +
    geom_point(aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7) +
    geom_vline(xintercept = 16, col = "darkred") +
    scale_x_continuous(lim = c(min_x, max_x), breaks = seq(min_x, max_x, ifelse(min_x == 10, 2, 1)), expand = c(.01,.01)) +
    scale_size_continuous(labels = scales::comma) +
    labs(x = "Bac grade",
         y = NULL,
         size = "# of observations",
         alpha = NULL) +
    theme(legend.justification = c(0, 1),
          legend.position = c(0.01, 0.99))
  
  if (class_var != "logical") {
    graph = graph +
      scale_y_continuous(labels = scales::comma, expand = c(0,0))
  } else {
    graph = graph +
      scale_y_continuous(labels = scales::percent_format(1), expand = c(0,0))
  }
  
  if (!is.null(y_axis)) {
    graph = graph +
      labs(y = y_axis)
  }
  
  if (!is.null(graph_caption)) {
    graph = graph +
      labs(caption = graph_caption)
  }
  
  return(graph)
}