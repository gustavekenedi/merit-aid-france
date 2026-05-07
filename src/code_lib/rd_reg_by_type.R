# estimate rd by initial enrollment type
rd_reg_by_type <- function(dt,
                           hei_type,
                           out.var = outcome) {
  
  rdd_reg <- dt[!is.na(get(out.var)) & ins_princip_lab %in% hei_type]
  Y = rdd_reg[[out.var]]
  X = rdd_reg$moy1
  donut = rdd_reg$donut == TRUE
  
  rd_graph <- rdplot(y = Y, x = X, c = 16, nbins = c(40,40), kernel = "triangular", p = 1, h = 1,
                     x.lim = c(14,18), subset = donut)
  rd_graph
  
  reg_mse <- rdrobust(y = Y, x = X, c = 16, p = 1, subset = donut, all = T)
  summary(reg_mse)
  
  reg_band1 <- rdrobust(y = Y, x = X, c = 16, p = 1, h = 1, subset = donut, all = T)
  summary(reg_band1)
  
  return(list(rd_graph, reg_mse, reg_band1))
}