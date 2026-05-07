# extract useful statistics from regression list ----
out_coef_ci <- function(reg_list,
                        donut_vec = c(F, T, T, F, T, T),
                        controls_vec = c(F, F, T, F, F, T)) {
  
  results <- reg_list
  results_clean <- data.frame()
  
  ncol = length(results)
  
  for (i in 1:ncol) {
    print(i)
    results_clean[i,1] <- results[[i]]$coef[[1]] # coefficient
    results_clean[i,2] <- results[[i]]$coef[[2]] # coefficient with bias
    results_clean[i,3] <- results[[i]]$se[[1]] # se
    results_clean[i,4] <- results[[i]]$se[[3]] # robust se
    results_clean[i,5] <- results[[i]]$pv[[1]] # p-value
    results_clean[i,6] <- results[[i]]$pv[[3]] # robust p-value
    results_clean[i,7] <- results[[i]]$N_h[1] # N (control)
    results_clean[i,8] <- results[[i]]$N_h[2] # N (treated)
    results_clean[i,9] <- results[[i]]$p # polynomial of running variable
    results_clean[i,10] <- results[[i]]$bws[1,1] # h bandwidth
    results_clean[i,11] <- results[[i]]$bws[2,1] # b bandwidth
    results_clean[i,12] <- results[[i]]$c # cutoff
  }
  
  colnames(results_clean) <- c("coef", "coef_bias", "se", "robust_se", "p_value", "robust_p_value", "n_cont", "n_treat", "poly", "h_bandwidth", "b_bandwidth", "threshold")
  
  results_clean <- results_clean %>%
    mutate(donut = donut_vec,
           controls = controls_vec,
           sig = ifelse(p_value <= 0.01, "***",
                        ifelse(p_value > 0.01 & p_value <= 0.05, "**",
                               ifelse(p_value > 0.05 & p_value <= 0.1, "*", ""))),
           robust_sig = ifelse(robust_p_value <= 0.01, "***",
                               ifelse(robust_p_value > 0.01 & robust_p_value <= 0.05, "**",
                                      ifelse(robust_p_value > 0.05 & robust_p_value <= 0.1, "*", ""))),
           ci_lower = coef - 1.96*se,
           ci_upper = coef + 1.96*se,
           robust_ci_lower = coef_bias - 1.96*robust_se,
           robust_ci_upper = coef_bias + 1.96*robust_se)
  
  return(results_clean)
}
# ----