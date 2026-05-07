# binned scatter plot of outcome with baseline regression line ----
rd_graph <- function(data,
                     cut,
                     out,
                     cutoff = 16,
                     sub = donut,
                     donut_graph = TRUE,
                     xmin = 14,
                     xmax = 18,
                     donut_min = 15.7,
                     donut_max = 16.05,
                     donut_lab = F,
                     reg_file = NULL) {
  
  data <- data |> 
    filter(!is.na(get(out)))
  
  y = data[[out]]
  x = data$moy1
  sub = data$donut
  
  if (is.null(reg_file)) {
    reg <- readRDS(here::here("out", "regressions", paste0(out, "_mse.rds")))
  } else {
    reg <- readRDS(here::here("out", "regressions", paste0(out, reg_file)))
  }
  
  if (donut_graph == T) {
    rdplot_out <- rdplot(y, x, c = cutoff, p = 1, kernel = "triangular", binselect = "es", nbins = 40, h = reg[[2]]$bws[1,1], x.lim = c(xmin, xmax), subset = sub)
  } else {
    rdplot_out <- rdplot(y, x, c = cutoff, p = 1, kernel = "triangular", binselect = "es", nbins = 40, h = reg[[1]]$bws[1,1], x.lim = c(xmin, xmax))
  }
  
  pred <- data.frame(x_hat = rdplot_out[["vars_poly"]][["rdplot_x"]],
                     y_hat = rdplot_out[["vars_poly"]][["rdplot_y"]])
  
  cut = cut
  class_var = typeof(data[[out]])
  
  if (donut_graph == T) {
    data_donut = data %>%
      mutate(moy_cut = as.numeric(as.character(cut(moy1, breaks = seq(10,20,cut), labels = seq(10,20,cut)[-length(seq(10,20,cut))], include.lowest = TRUE, right = FALSE))))%>%
      group_by(moy_cut) %>%
      summarise(proba = if_else(class_var == "logical", mean(get(out) == TRUE), mean(get(out), na.rm = T)),
                n_obs = n()) %>%
      filter(moy_cut >= donut_min & moy_cut < donut_max)
    
    data_graph = data %>%
      mutate(moy_cut = as.numeric(as.character(cut(moy1, breaks = seq(10,20,cut), labels = seq(10,20,cut)[-length(seq(10,20,cut))], include.lowest = TRUE, right = FALSE))))%>%
      group_by(moy_cut) %>%
      summarise(proba = if_else(class_var == "logical", mean(get(out) == TRUE), mean(get(out), na.rm = T)),
                n_obs = n()) %>%
      filter(moy_cut > xmin & moy_cut < xmax) %>%
      filter(moy_cut < donut_min | moy_cut >= donut_max)
    
    graph = data_graph %>%
      ggplot() +
      geom_point(aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7, color = "#de6757") +
      geom_point(data = data_donut, aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7, color = "grey", show.legend = FALSE) +
      geom_line(data = pred, aes(x = x_hat, y = y_hat), color = "black") +
      geom_vline(xintercept = cutoff, col = "darkred")
  } else {
    graph = data %>%
      mutate(moy_cut = as.numeric(as.character(cut(moy1, breaks = seq(10,20,cut), labels = seq(10,20,cut)[-length(seq(10,20,cut))], include.lowest = TRUE, right = FALSE))))%>%
      group_by(moy_cut) %>%
      summarise(proba = mean(get(var) == TRUE),
                n_obs = n()) %>%
      filter(moy_cut > xmin & moy_cut < xmax) %>%
      ggplot() +
      geom_point(aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7, color = "#de6757") +
      geom_line(data = pred, aes(x = x_hat, y = y_hat), color = "black") +
      geom_vline(xintercept = cutoff, col = "darkred") +
      scale_x_continuous(lim = c(xmin, xmax),
                         breaks = xmin:xmax,
                         labels = as.character(xmin:xmax),
                         expand = c(.005,.005))
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
    scale_size_continuous(labels = scales::comma, breaks = c(2500, 5000, 7500)) +
    labs(x = "Bac grade",
         size = "# of observations",
         alpha = NULL) +
    theme(legend.justification = c(0, 0),
          legend.position = c(0.01, 0.01))
  
  return(list(graph, data_graph, data_donut, pred))
}
# ----

