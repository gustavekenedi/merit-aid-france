# extract useful statistics from regression list - simple ----
out_coef_ci_simple <- function(reg_list) {
  
  results <- reg_list
  results_clean <- data.frame()
  
  ncol = length(results)
  
  for (i in 1:ncol) {
    print(i)
    
    results_clean[i,1] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$coef[[1]]) # coefficient
    results_clean[i,2] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$coef[[2]]) # coefficient with bias
    results_clean[i,3] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$se[[1]]) # se
    results_clean[i,4] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$se[[3]]) # robust se
    results_clean[i,5] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$pv[[1]]) # p-value
    results_clean[i,6] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$pv[[3]]) # robust p-value
    results_clean[i,7] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$N_h[1]) # N (control)
    results_clean[i,8] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$N_h[2]) # N (treated)
    results_clean[i,9] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$p) # polynomial of running variable
    results_clean[i,10] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$bws[1,1]) # h bandwidth
    results_clean[i,11] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$bws[2,1]) # b bandwidth
    results_clean[i,12] <- ifelse(sum(is.na(results[[i]])) == 1, NA, results[[i]]$c) # cutoff
  }
  
  colnames(results_clean) <- c("coef", "coef_bias", "se", "robust_se", "p_value", "robust_p_value", "n_cont", "n_treat", "poly", "h_bandwidth", "b_bandwidth", "threshold")
  
  results_clean <- results_clean %>%
    mutate(sig = ifelse(p_value <= 0.01, "***",
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