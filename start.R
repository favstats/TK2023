
pacman::p_load(knitr, tidyverse, knitr, openxlsx, sf)

# rstudioapi::jobRunScript("retrieve_targeting_data.R")
# rstudioapi::jobRunScript("fbadlibrary.R")

# Sys.sleep(60*7)
all_dat <- readRDS("data/all_dat.rds")

write_lines(nrow(all_dat), file = "n_advertisers.txt")


dir("_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(quarto::quarto_render)

knitr::knit("Readme.Rmd")
