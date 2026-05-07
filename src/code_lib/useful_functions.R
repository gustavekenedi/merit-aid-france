# Count to percentages
count_to_pct <- function(data, ..., col = n) {
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr) * 100) %>%
    ungroup()
}

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
