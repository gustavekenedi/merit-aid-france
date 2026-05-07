placebos_table <- function(coefs, file_name) {
  
  n_columns <- ncol(coefs) - 1
  
  kbl(coefs, "latex",
      booktabs = T,
      caption = "Placebos",
      label = "placebos_band1",
      linesep = "",
      col.names = c("", paste0("(", 1:n_columns,")")),
      escape = F,
      align = c("l",rep("c", n_columns))) %>%
    add_header_above(c("", "\\\\thead{Enrollment in\\\\\\\\HS Grad. Year}",
                       "\\\\thead{Enrollment in 2nd\\\\\\\\Year in HS Grad.\\\\\\\\Year + 1}",
                       "\\\\thead{Enrollment in 3rd\\\\\\\\Year in HS Grad.\\\\\\\\Year + 2}",
                       "\\\\thead{Number of\\\\\\\\Years in HE}",
                       "\\\\thead{Highest Level of\\\\\\\\Study Attained}",
                       "\\\\thead{Obtaining\\\\\\\\a Degree}"),
                     line = F, escape = F) %>% 
    pack_rows("Panel A. Grade 15", 1, 10, bold = F, italic = T) %>%
    pack_rows("Panel B. Grade 14", 11, 20, bold = F, italic = T) %>%
    pack_rows("Panel C. Grade 16 for non-eligibles to need-based grants", 21, 30, bold = F, italic = T) %>%
    kable_styling(position = "center", latex_options = c("scale_down")) %>% 
    footnote(general_title = "Notes:",
             general =  "Robust 95\\\\% confidence intervals in brackets. Statistical significance is computed based on the robust p-value and ***, **, and * indicate significance at 1, 5, and 10\\\\%, respectively.",
             threeparttable = TRUE, escape = F) %>%
    save_kable(file = here("out","tables", paste0(file_name, ".tex")))
}
