# export table to latex ----
export_table <- function(input,
                         coef_tab = FALSE,
                         poly_header = TRUE,
                         caption,
                         label,
                         footnote_text = "\\\\footnotesize Conventional standard errors in parenthesis. Statistical significance is computed based on the robust p-value and ***, **, and * indicate significance at 1, 5, and 10\\\\%, respectively.",
                         file_name) {
  
  n_columns = ncol(input) - 1
  
  coef_table <- kbl(input, "latex",
                    booktabs = T,
                    caption = caption,
                    label = label,
                    linesep = "",
                    col.names = c("", paste0("(", 1:n_columns,")")),
                    escape = FALSE,
                    align = c("l",rep("c", n_columns))) %>%
    footnote(general_title = "\\\\footnotesize Notes:", general = footnote_text, threeparttable = TRUE, footnote_as_chunk = T, escape = F) %>%
    row_spec(9, hline_after = T) %>% 
    kable_styling(position = "center", latex_options = c("HOLD_position","scale_down"))
  
  if (input[2,1] == "") {
    coef_table <- coef_table %>%
      row_spec(2, hline_after = T) 
  } else {
    coef_table <- coef_table %>%
      row_spec(1, hline_after = T) %>%
      row_spec(3, hline_after = T)
  }
  
  if (poly_header == T) {
    coef_table <- coef_table %>%
      add_header_above(c("","First order" = 3, "Second order" = 3), italic = T)
  }
  
  coef_table %>% save_kable(file = here::here("out","tables",file_name))
}
# ----
