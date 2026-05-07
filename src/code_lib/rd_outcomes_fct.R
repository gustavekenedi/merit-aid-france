rd_outcomes_fct <- function() {
  
  rds_file <- here(glue("out/figures/raw_data/graph_{outcome}_bandwidth_1_donut_nocovs.rds"))
  
  data_graph <- read_rds(rds_file)[[2]]
  data_donut <- read_rds(rds_file)[[3]]
  pred <- read_rds(rds_file)[[4]]
  
  cut = 0.05
  threshold = 16
  
  data_graph %>% 
    ggplot() +
    geom_point(aes(x = moy_cut + cut/2, y = proba, size = n_obs), alpha = 0.7) +
    geom_point(data = data_donut,
               aes(x = moy_cut + cut/2, y = proba, size = n_obs),
               alpha = 0.7, color = "grey", show.legend = FALSE) +
    geom_line(data = pred,
              aes(x = x_hat, y = y_hat),
              color = "black") +
    geom_vline(xintercept = threshold, col = "darkred") +
    scale_y_continuous(labels = scales::percent_format(1), expand = c(0.001,0.001)) +
    scale_size_continuous(breaks = c(2500, 5000, 7500), labels = scales::comma) +
    labs(x = "Bac grade",
         size = "# of observations",
         alpha = NULL) +
    theme(text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(face = "italic", size = 12),
          legend.text = element_text(size = 12),
          legend.justification = c(0, 0),
          legend.position = c(0.01, 0.01),
          legend.background = element_rect(fill = alpha("white", .75)))
}
