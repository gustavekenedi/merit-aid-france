# observables binned scatter plot of outcome with baseline regression line ----
rd_graph_observables <- function(data,
                                 cut,
                                 var,
                                 threshold,
                                 outcome,
                                 y = Y,
                                 x = X,
                                 sub = donut,
                                 cutoff = cutoff_16,
                                 donut_graph = TRUE,
                                 xmin = 14,
                                 xmax = 18,
                                 donut_min = 15.7,
                                 donut_max = 16.05,
                                 donut_lab = F,
                                 reg_file = NULL) {
  
  if (is.null(reg_file)) {
    reg <- readRDS(here::here("out", "regressions", paste0(outcome, "_mse.rds")))
  } else {
    reg <- readRDS(here::here("out", "regressions", paste0(outcome, reg_file)))
  }
  
  if (donut_graph == T) {
    rdplot_outcome <- rdplot(y, x, c = cutoff, p = 1, kernel = "triangular", binselect = "es", nbins = 40, h = reg$bws[1,1], x.lim = c(14,18), subset = sub)
  } else {
    rdplot_outcome <- rdplot(y, x, c = cutoff, p = 1, kernel = "triangular", binselect = "es", nbins = 40, h = reg$bws[1,1], x.lim = c(14,18))
  }
  
  pred <- data.frame(x_hat = rdplot_outcome[["vars_poly"]][["rdplot_x"]],
                     y_hat = rdplot_outcome[["vars_poly"]][["rdplot_y"]])
  
  cut = cut
  class_var = typeof(data[[outcome]])
  
  if (donut_graph == T) {
    data_donut = data %>%
      mutate(moy_cut = as.numeric(as.character(cut(round(moy1,2), breaks = seq(10,20,cut), labels = seq(10,20,cut)[-length(seq(10,20,cut))], include.lowest = TRUE, right = FALSE))))%>%
      group_by(moy_cut) %>%
      summarise(proba = if_else(class_var == "logical", mean(get(var) == TRUE), mean(get(var), na.rm = T)),
                n_obs = n()) %>%
      filter(moy_cut >= donut_min & moy_cut < donut_max)
    
    data_graph = data %>%
      mutate(moy_cut = as.numeric(as.character(cut(round(moy1,2), breaks = seq(10,20,cut), labels = seq(10,20,cut)[-length(seq(10,20,cut))], include.lowest = TRUE, right = FALSE))))%>%
      group_by(moy_cut) %>%
      summarise(proba = if_else(class_var == "logical", mean(get(var) == TRUE), mean(get(var), na.rm = T)),
                n_obs = n()) %>%
      filter(moy_cut > xmin & moy_cut < xmax) %>%
      filter(moy_cut < donut_min | moy_cut >= donut_max)
    
    graph = data_graph %>% 
      ggplot() +
      geom_point(aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7) +
      geom_point(data = data_donut, aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7, color = "grey", show.legend = FALSE) +
      geom_line(data = pred, aes(x = x_hat, y = y_hat), color = "black") +
      geom_vline(xintercept = threshold, col = "darkred")
    # geom_vline(xintercept = c(donut_min, donut_max), col = "black", linetype = 2) +
    # scale_x_continuous(lim = c(14,18), breaks = c(seq(14,18,1),donut_min, donut_max), labels = c("14","15","16","17","18", donut_min, paste0("           ", donut_max)), expand = c(.01,.01))
  } else {
    graph = data %>%
      mutate(moy_cut = as.numeric(as.character(cut(moy1, breaks = seq(10,20,cut), labels = seq(10,20,cut)[-length(seq(10,20,cut))], include.lowest = TRUE, right = FALSE))))%>%
      group_by(moy_cut) %>%
      summarise(proba = mean(get(var) == TRUE),
                n_obs = n()) %>%
      filter(moy_cut > xmin & moy_cut < xmax) %>%
      ggplot() +
      geom_point(aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7) +
      geom_line(data = pred, aes(x = x_hat, y = y_hat), color = "black") +
      geom_vline(xintercept = threshold, col = "darkred")
    # scale_x_continuous(lim = c(14,18), breaks = seq(14,18,1), labels = c("14","15","16","17","18"), expand = c(.01,.01))
  }
  
  if (donut_graph == T & donut_lab == T){
    graph = graph +
      scale_x_continuous(lim = c(xmin, xmax),
                         breaks = c(xmin:xmax, donut_min, donut_max),
                         labels = c(as.character(xmin:xmax), donut_min, paste0("           ", donut_max)),
                         expand = c(.005,.005))
  } else {
    graph = graph +
      scale_x_continuous(lim = c(xmin, xmax),
                         breaks = xmin:xmax,
                         expand = c(.005,.005))
  }
  
  graph = graph +
    scale_y_continuous(labels = scales::percent_format(1), expand = c(0.001,0.001)) +
    scale_size_continuous(labels = scales::comma) +
    labs(x = "Bac grade",
         size = "# of observations",
         alpha = NULL) +
    theme(text = element_text(size = 20),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(face = "italic", size = 14),
          legend.text = element_text(size = 14),
          legend.justification = c(0, 0),
          legend.position = c(0.01, 0.01),
          legend.background = element_rect(fill = alpha("white", .75)),
          plot.caption = element_text(size = 10, face = "italic"))
  
  return(list(graph, data_graph, data_donut, pred))
  
}
# ----