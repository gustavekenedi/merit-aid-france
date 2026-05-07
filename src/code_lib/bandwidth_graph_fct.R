bandwidth_graph_fct <- function(out,
                                band = 1,
                                coefs = fread(here("out", "figures", "raw_data", paste0("bandwidth_", out, "_", ifelse(is.null(band), "mse", band), "_bandwidth_nocovs.csv"))),
                                mse_t = F,
                                mse_t_y = 0.075,
                                legend = T) {
  reg_mse = readRDS(here("out", "regressions", paste0(out, "_bandwidth_mse.rds")))
  mse_band = data.frame(h_bandwidth = c(reg_mse[[2]]$bws[1,1],
                                        reg_mse[[5]]$bws[1,1]),
                        poly = factor(1:2, labels = c("Poly. order: 1", "Poly. order: 2")))
  graph <- graph_bandwidth(data = coefs,
                           mse_band_data = mse_band,
                           mse_line = T,
                           mse_text = mse_t,
                           y_text = mse_t_y,
                           poly_legend = legend) +
    labs(y = NULL)
  graph
  
  ggsave(here("out","figures", paste0("bandwidth_", out, "_", ifelse(is.null(band), "mse", band), "_bandwidth_nocovs.pdf")),
         height = fig_height/2, width = fig_width/2)
  
  return(graph)
}