library(tidyverse)
library(readxl)

sectdis <- read_excel(here("original_data/other/sectdis_lab.xlsx"))
discipli <- read_excel(here("original_data/other/discipli_lab.xlsx"))

sectdis_clean <- sectdis |>
  left_join(discipli)

fwrite(sectdis_clean, here("modified_data/other/sectdis_lab_clean.csv"))
