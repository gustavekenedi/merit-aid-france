bandwidth_fct <- function(out, band = 1) {
  Y = rdd[[out]]
  coef_ci <- coef_ci_bandwidth(out,
                               Y,
                               X,
                               bandwidth = band,
                               poly_max = 2,
                               controls_spec = F)
  
  fwrite(coef_ci, here("out", "figures", "raw_data", paste0("bandwidth_", out, "_", ifelse(is.null(band), "mse", band), "_bandwidth_nocovs.csv")))
  
  return(coef_ci)
}