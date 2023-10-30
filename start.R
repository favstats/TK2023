
pacman::p_load(knitr, tidyverse, rmarkdown, openxlsx, sf)

# rstudioapi::jobRunScript("retrieve_targeting_data.R")
# rstudioapi::jobRunScript("fbadlibrary.R")

# Sys.sleep(60*7)

dir("_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(quarto::quarto_render)

rmarkdown::render("Readme.Rmd", output_file = "Readme.md")


# election_dat30 <- readRDS("data/election_dat30.rds") %>%
#   # left_join(all_dat) %>% 
#   rename(internal_id = page_id)  %>% 
#   filter(is.na(no_data)) %>% 
#   filter(party != "And")
# 
# election_dat7 <- readRDS("data/election_dat30.rds") %>%
#   # left_join(all_dat) %>% 
#   rename(internal_id = page_id)  %>% 
#   filter(is.na(no_data)) %>% 
#   filter(party != "And")
# 
#  # read_csv("data/lastdays.csv") %>% .[1,] %>% as.character() %>% rev %>% rep(2)
#  
#  # ggl_sel_sp <- readRDS("data/ggl_sel_sp.rds")
#  # ggl_sel_sp7 <- readRDS("data/ggl_sel_sp7.rds")
#  
# 
# election_dat30 %>%
#   slice(1) %>% 
#   select(tstamp, ds) %>% 
#   mutate(platform = "Meta", tf = "30 days") %>% 
#   bind_rows(
#     election_dat7 %>%
#       slice(1) %>% 
#       select(tstamp, ds) %>% 
#       mutate(platform = "Meta", tf = "7 days")
#   ) 
