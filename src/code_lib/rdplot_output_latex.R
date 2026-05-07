out_tab_latex <- function(..., dep_var_name, donut, controls, mean_baseline, round = 3, file){
  
 results <- list(...)
 ncol <- length(results)
 
 results_clean <- data.frame(x = c(dep_var_name,"","\\textit{p}-value","\\# obs. (control)","\\# obs. (treated)","Poly. order", "Bandwidth","Donut","Controls","Mean (left)"), y = "")

 for (i in 1:ncol) {
   results_clean[1,i+1] <- round(results[[i]]$coef[[3]], round) # coefficient
   results_clean[2,i+1] <- paste0("(", round(results[[i]]$se[[3]], round), ")") # se
   results_clean[3,i+1] <- round(results[[i]]$pv[[3]], round) # p-value
   results_clean[4,i+1] <- results[[i]]$N_h[1] # N (control)
   results_clean[5,i+1] <- results[[i]]$N_h[2] # N (treated)
   results_clean[6,i+1] <- results[[i]]$p # polynomial of running variable
   results_clean[7,i+1] <- paste0("[",results[[i]]$call$c - as.numeric(as.character(results[[i]]$call$h)[2]), ", ", results[[i]]$call$c + as.numeric(as.character(results[[i]]$call$h)[2]), "]") # bandwidth
   results_clean[8,i+1] <- donut[i] # donut checkmark
   results_clean[9, i+1] <- controls[i] # controls checkmark
   results_clean[10, i+1] <- mean_baseline[i] # baseline mean
 }
 
 for (i in 1:ncol) {
   results_clean[1,i+1] <- paste0(results_clean[[1,i+1]], ifelse(results_clean[3,i+1] <= 0.01, "$^{***}$",
                                                                 ifelse(results_clean[3,i+1] > 0.01 & results_clean[3,i+1] <= 0.05, "$^{**}$",
                                                                 ifelse(results_clean[3,i+1] > 0.05 & results_clean[3,i+1] <= 0.1, "$^{*}$",
                                                                 ""))))
 }
 
 return(results_clean)
 
}
