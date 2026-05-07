# bandwidth robustness graph ----
graph_bandwidth <- function(data = coef_ci,
                            mse_band_data = mse_band,
                            band_min = 0.5,
                            band_max = 2,
                            mse_line = T,
                            mse_text = F,
                            y_text = 0.075,
                            poly_legend = T) {
  
  if(max(data$poly) == 2) {
    data <- data %>% 
      mutate(poly = factor(poly, labels = c("Poly. order: 1", "Poly. order: 2")))
  }
  
  graph <- data %>%
    filter(h_bandwidth >= .5) %>%
    mutate(sig = (robust_p_value <= 0.05)) %>%
    ggplot(aes(x = h_bandwidth, y = coef)) +
    geom_point(aes(color = sig), size = 3) +
    geom_line(aes(y = robust_ci_lower), linetype = 2, color = "grey") +
    geom_line(aes(y = robust_ci_upper), linetype = 2, color = "grey") +
    geom_hline(yintercept = 0, col = "black") +
    scale_x_continuous(lim = c(0.5, band_max), breaks = seq(0, band_max,.5)) +
    # scale_y_continuous(breaks = seq(-1,1,.05)) +
    # coord_cartesian(ylim = c(-.1,.1)) +
    scale_color_manual(limits = c(FALSE, TRUE), values = c("grey","black")) +
    labs(x = "Bandwidth",
         y = "Effect of eligibility to aide au mérite",
         col = "Significant at 5%") +
    guides(color = guide_legend(nrow = 2)) +
    facet_wrap(~ poly) +
    theme(legend.position = c(1,1),
          legend.justification = c(1,1),
          plot.margin = unit(c(6, 15, 6, 6), "pt"),
          panel.spacing = unit(3, "lines"))
  
  if (poly_legend == F) {
    graph <- graph +
      theme(legend.position = "none")
  }
  
  if (mse_line == T) {
    graph = graph +
      geom_vline(data = mse_band_data, aes(xintercept = h_bandwidth), linetype = 2, col = "#de6757")
  }
  
  if (mse_text == T) {
    graph = graph +
      geom_text(data = data.frame(poly = "Poly. order: 1"),
                aes(y = y_text, x = mse_band_data[1,1] + .1, label = "MSE-optimal \nbandwidth"),
                col = "#de6757", hjust = 0, size = 4.5)
  }
  
  return(graph)
}
# ----