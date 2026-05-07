# load packages
# librarian::shelf(here,
#                  tidyverse, data.table,
#                  ggthemr, ggalluvial, patchwork, scico,
#                  janitor,
#                  fastDummies,
#                  broom, kableExtra,
#                  doParallel, furrr,
#                  rdrobust, rddensity,
#                  )
packages <- c("here",
              "tidyverse", "data.table",
              "ggthemr", "ggalluvial", "patchwork", "scico",
              "janitor", "glue",
              "fastDummies",
              "broom", "kableExtra",
              "doParallel", "furrr", "tidylog")

# install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load required packages
invisible(lapply(packages, library, character.only = TRUE))

# load useful functions
source(here("src","code_lib","useful_functions.R"))
# code_lib_files <- paste0(here("src/code_lib", list.files(here("src/code_lib"), pattern = ".R")))
# lapply(code_lib_files, source)

# figures parameters
fig_height = 6
fig_width = (16/9)*fig_height

ggthemr("pale")
theme_update(text = element_text(size = 12),
             legend.title = element_text(face = "italic", size = 14),
             legend.text = element_text(size = 14),
             legend.background = element_rect(fill = alpha("white", .75)),
             plot.caption = element_text(size = 10, face = "italic", color = "#7d7d7d"))

# no scientific notation
options(scipen=999)

# number of cores for parallelisation
n_cores = 10

# data folder
data_path <- "../data/"
data_modified_path <- here("out", "data/")