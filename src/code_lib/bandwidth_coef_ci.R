# bandwidth robustness ----
coef_ci_bandwidth <- function(outcome,
                              y = Y, x = X,
                              ctrls = covs,
                              cutoff = cutoff_16,
                              bandwidth = NULL,
                              sub = donut,
                              poly_max,
                              band_min = 0.5,
                              band_max = 2,
                              donut_spec = TRUE,
                              controls_spec = TRUE) {
  
  if(is.null(bandwidth)) {
    baseline_reg <- readRDS(here::here("out","regressions", paste0(outcome, "_bandwidth_mse.rds")))
  } else {
    baseline_reg <- readRDS(here::here("out","regressions", paste0(outcome, "_bandwidth_1.rds")))
  }
  
  h_band <- rep(seq(band_min, band_max, .1), poly_max)
  if (is.null(bandwidth)) {
    b_band <- rep(baseline_reg[[3]]$bws[2,1], length(h_band))
  } else {
    b_band = h_band
  }
  poly <- rep(1:poly_max, each = length(h_band)/poly_max)
  
  grappe <- makeCluster(n_cores)
  registerDoParallel(grappe)
  
  system.time({
    if (donut_spec == TRUE) {
      if (controls_spec == TRUE) {
        coef_ci <- foreach(i = 1:length(h_band), .packages=c("rdrobust", "tidyverse", "here"), .combine = rbind) %dopar% {
          reg = rdrobust(y, x, c = cutoff, all = T, h = h_band[i], b = b_band[i], p = poly[i], covs = ctrls, subset = sub)
          source(here("src", "code_lib", "out_coef_ci.R"))
          data = out_coef_ci(list(reg), donut_vec = T, controls_vec = T)}
      } else {
        coef_ci <- foreach(i = 1:length(h_band), .packages=c("rdrobust", "tidyverse", "here"), .combine = rbind) %dopar% {
          reg = rdrobust(y, x, c = cutoff, all = T, h = h_band[i], b = b_band[i], p = poly[i], subset = sub)
          source(here("src", "code_lib", "out_coef_ci.R"))
          data = out_coef_ci(list(reg), donut_vec = T, controls_vec = F)}
      }
    } else if (controls_spec == TRUE) {
      coef_ci <- foreach(i = 1:length(h_band), .packages=c("rdrobust", "tidyverse", "here"), .combine = rbind) %dopar% {
        reg = rdrobust(y, x, c = cutoff, all = T, h = h_band[i], b = b_band[i], p = poly[i], covs = ctrls)
        source(here("src", "code_lib", "out_coef_ci.R"))
        data = out_coef_ci(list(reg), donut_vec = F, controls_vec = T)}
    } else {
      coef_ci <- foreach(i = 1:length(h_band), .packages=c("rdrobust", "tidyverse", "here"), .combine = rbind) %dopar% {
        reg = rdrobust(y, x, c = cutoff, all = T, h = h_band[i], b = b_band[i], p = poly[i])
        source(here("src", "code_lib", "out_coef_ci.R"))
        data = out_coef_ci(list(reg), donut_vec = F, controls_vec = F)}}})
  stopCluster(grappe)
  
  return(coef_ci)
}
# ----