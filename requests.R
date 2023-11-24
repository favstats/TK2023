
library(tidyverse)
library(openxlsx)
library(sf)
library(janitor)

#### replace this data with the most recent one, just change the date below this __vvv___
election_dat30 <- read_rds("https://github.com/groene/TK2023/raw/main/historic/2023-11-20/30.rds") %>% 
  filter(is.na(no_data))



color_dat <- tibble(
  colors = c("#00b13d", "#80c31c", "#0a2cca", "#008067", "#bf0000", "#ff0000", "#6f2421", "#02a6e9", "#92107d", "#04d3d4", "#242b57", "#66cdaa", "#242b57", "#006b28", "#012758", "#ea5b0b", "#582c83", "#698c0c", "#fdfd00", "#8da6d6", "#dc1f26"),
  party = c("D66", "GroenLinks", "VVD", "CDA", "SP", "PvdA", "FvD", "ChristenUnie", "50PLUS", "Alliantie", "BVNL", "DENK", "Ja21", "PvdD", "PVV", "SGP", "Volt Nederland", "BBB", "BIJ1", "NSC", "GroenLinks-PvdA"))

scale_fill_parties <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}
scale_color_parties <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}


#### detailed list #######

# 1) GL/PvdA, 2) D66, 3) BBB, 4) CDA, 5) VVD, 6) DENK 

detailed_list <- election_dat30 %>%
  filter(party %in% c("GroenLinks-PvdA", "D66", "BBB", "CDA", "VVD", "DENK")) %>% 
  filter(!is_exclusion) %>% 
  mutate(spend = total_spend_pct*total_spend_formatted) %>% 
  filter(!is.na(detailed_type)) %>% 
  group_by(party, detailed_type, value) %>% 
  summarize(spend_on_targeting = sum(spend), 
            overall_spend = sum(total_spend_formatted),
            n_pages = n(),
            num_ads = sum(num_ads)) %>% 
  ungroup() %>% 
  mutate(perc_spend = spend_on_targeting/overall_spend)


write_csv(detailed_list, "detailed_list.csv")


#### MAPPING ############


cbs<- openxlsx::read.xlsx("https://github.com/groene/TK2023/raw/main/data/cbs_pc4_2020_v2.xlsx")


all_zip <- election_dat30 %>% 
  filter(type == "location") %>% 
  filter(location_type == "zips") %>% 
  mutate(total_spend_formatted = total_spend_pct*total_spend_formatted) %>% 
  filter(!is_exclusion) %>% 
  group_by(page_id, total_spend_pct, party) %>%
  mutate(n_together = n()) %>%
  mutate(total_spend_formatted_old = total_spend_formatted) %>% 
  mutate(total_spend_formatted = total_spend_formatted/n_together) %>% 
  select(party, page_id, value, n_together, total_spend_formatted, total_spend_formatted_old) %>% 
  ungroup() %>% 
  arrange(value) %>%
  group_by(value, party) %>% 
  summarize(total_spend = sum(total_spend_formatted),
            total_spend_old = sum(total_spend_formatted_old)) %>% 
  ungroup() %>% 
  dplyr::mutate(pc4 = str_remove_all(value, ", Netherlands") %>% as.numeric) %>% 
  left_join(cbs %>% 
              janitor::clean_names())

if(!file.exists("res.geojson")){
  ## this file is too big for dropbox
  download.file("https://www.dropbox.com/scl/fi/f2zin6b3k95jxkprah3j0/georef-netherlands-postcode-pc4.geojson?rlkey=m3cbtoqbvg89wfkyb80cmu3co&dl=1", "res.geojson")
  
  geo <- sf::st_read("res.geojson", quiet = T)
}

pparty <- all_zip %>%
  mutate(pc4_code = str_remove_all(value, ", Netherlands")) %>%
  select(pc4, pc4_code, total_spend_old, party)  %>% 
  right_join(cbs %>%
               janitor::clean_names()) %>% 
  ## TODO: here I remove some parties before I count number of parties targeting a location
  ## feel free to add or remove parties as you see fit
  filter(!(party %in% c("GroenLinks-PvdA", "Ja21"))) %>%
  count(pc4_code)


geo %>% 
  left_join(pparty) %>% 
  ggplot()  +
  theme_void() +
  geom_sf(aes(fill = n),
          colour = NA,
          size = 0.00000001) +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(title = "Number of Parties Targeting Postcode", barwidth = 10, barheight = 0.5)) +
  scale_fill_viridis_c()
