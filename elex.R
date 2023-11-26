

tk2021 <- read_csv2("data/TK20210317.csv") %>% 
  janitor::clean_names() %>% 
  mutate(pvv = pvv_partij_voor_de_vrijheid/opkomst)

# tk2021$
tk2023 <- read_csv("data/data-2SSi6.csv") %>% 
  janitor::clean_names()



tk2023 %>% 
  mutate(regio_code = str_remove_all(lau_id, "M")) %>% 
  select(regio_code, pvv2023=pvv) %>% 
  left_join(tk2021 %>% select(regio_code, pvv2021 =pvv )) %>% 
  ggplot(aes(pvv2021, pvv2023)) +
  geom_point() +
  geom_smooth()
  # geom_abline(intercept = 0, slope = 1) +
  # ylim(0,0.6) +
  # xlim(0, 0.6)


tk2023 %>% 
  mutate(regio_code = str_remove_all(lau_id, "M")) %>% 
  select(regio_code, pvv2023=pvv) %>% 
  left_join(tk2021 %>% select(regio_code, pvv2021 =pvv )) %>% 
  mutate(diff = pvv2023-pvv2021) %>% 
  # mutate(regio_code = fct_r(regio_code, diff)) %>% 
  ggplot(aes(regio_code, diff)) +
  geom_col() 
  # coord_flip()
  # geom_histogram()



# openxlsx::read.xlsx("data/ignore/GWB2022_PC6_huisnr.xlsx")

library(openxlsx)
read_all_sheets = function(xlsxFile, ...) {
  sheet_names = openxlsx::getSheetNames(xlsxFile)
  sheet_list = as.list(rep(NA, length(sheet_names)))
  names(sheet_list) = sheet_names
  for (sn in sheet_names) {
    sheet_list[[sn]] = openxlsx::read.xlsx(xlsxFile, sheet=sn, ...)
  }
  return(sheet_list)
}

gwb <- read_all_sheets("data/ignore/GWB2022_PC6_huisnr.xlsx")



saveRDS(gwb %>% bind_rows(), "data/ignore/GWB2022_PC6_huisnr.rds")

gwb <- readRDS("data/ignore/GWB2022_PC6_huisnr.rds")

gwb %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(pc4 = str_sub(pc6, 1,4)) %>% 
  count(pc4, gem_code, sort = T) %>% 
  count(pc4, sort = T) %>% 
  filter(n == 2) %>% pull(pc4) %>% dput()
  select(pc4, pc6)
  
  doubles<-c("1261", "1724", "2114", "4062", "4197", "4715", "5091", "5383", 
    "5504", "6367", "6574", "6924", "6961", "7011", "7351", "7693", 
    "7694", "7933", "7963", "7964", "8096", "9207", "9417", "9423")
    
  
tobeadded <- gwb %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(pc4 = str_sub(pc6, 1,4)) %>% 
    count(pc4, gem_code, sort = T) %>% 
    filter(pc4 %in% doubles) %>% 
    group_by(pc4) %>% 
    # filter(pc4 == "4062")
    arrange(desc(n)) %>% 
    slice(1) %>% 
    ungroup() 
  
  
dict_pc4 <-  gwb %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(pc4 = str_sub(pc6, 1,4)) %>% 
    count(pc4, gem_code, sort = T) %>% 
    filter(!(pc4 %in% doubles)) %>% 
    bind_rows(tobeadded)  %>% 
  mutate(regio_code = str_remove_all(gem_code, "M"))


elex30 <- readRDS("data/election_dat30.rds")



all_zip %>% 
  group_by(pc4, party) %>%
  summarize(total_spend = sum(total_spend)) %>% 
  arrange(desc(total_spend)) %>% 
  ggplot(aes(total_spend)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~party)


all_zip <- election_dat30 %>% 
  filter(type == "location") %>% 
  filter(location_type == "zips") %>% 
  mutate(total_spend_formatted = total_spend_pct*total_spend_formatted) %>% 
  filter(!is_exclusion) %>% 
  group_by(page_id, total_spend_pct, num_ads, party) %>%
  mutate(n_together = n()) %>%
  # select(page_id, value, num_ads, total_spend_formatted, n_together) 
  mutate(total_spend_formatted_old = total_spend_formatted) %>% 
  mutate(total_spend_formatted = total_spend_formatted/n_together) %>% 
  select(party, page_id, value, n_together, total_spend_formatted, total_spend_formatted_old) %>% 
  ungroup() %>% 
  arrange(value) %>%
  group_by(value, party) %>% 
  summarize(total_spend = sum(total_spend_formatted),
            total_spend_old = sum(total_spend_formatted_old)) %>% 
  ungroup() %>% 
  # filter(party == "VVD")  %>% 
  dplyr::mutate(pc4 = str_remove_all(value, ", Netherlands") %>% as.numeric) %>% 
  right_join(cbs %>% 
              janitor::clean_names())

parties_with_zips <- all_zip %>%
  count(party) %>% distinct(party)


all_zip %>% count(party)

unique_parties <- parties_with_zips
unique_pc4 <- cbs %>% janitor::clean_names() %>% select(pc4)
all_combinations <- full_join(unique_parties, unique_pc4, by = character())
merged_data <- left_join(all_combinations, all_zip, by = c("party", "pc4")) %>% 
  mutate(total_spend = ifelse(is.na(total_spend), 0, total_spend),
         total_spend_old = ifelse(is.na(total_spend_old), 0, total_spend_old)) %>% 
  drop_na(party)

spend_per_party <- merged_data %>% 
  mutate(pc4 = as.factor(pc4)) %>% 
  group_by(pc4, party, .drop = F) %>%
  summarize(total_spend = sum(total_spend_old)) %>% 
  ungroup() %>% 
  # filter(party != "GroenLinks-PvdA") %>% 
  mutate(pc4 = as.character(pc4)) 
  # left_join(cbs %>% 
  #             janitor::clean_names() %>% 
  #             select(pc4))
# spend_per_party %>% distinct(pc4)
# tk2021 %>% 
spend_per_party %>% count(party)
  
tk2021 <- read_csv2("data/TK20210317.csv") %>% 
  janitor::clean_names() %>% 
  mutate(pvv = pvv_partij_voor_de_vrijheid/opkomst)

spend_per_party %>% 
  left_join(dict_pc4) %>% 
  left_join(tk2021) %>% 
  mutate(turnout = opkomst/kiesgerechtigden*100) %>% 
  filter(!(party  %in% c("BBB", "Ja21", "Libertaire Partij", "D66"))) %>% 
  ggplot(aes(total_spend, turnout)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  facet_wrap(~party) +
  ggpubr::stat_cor()



spend_per_party %>% 
  left_join(dict_pc4) %>% 
  left_join(tk2021) %>% 
  mutate(turnout = opkomst/kiesgerechtigden*100) %>% 
  # filter(!(party  %in% c("BBB", "Ja21", "Libertaire Partij", "D66"))) %>% 
  mutate(targeted = ifelse(total_spend>0, "Yes", "No")) %>% 
  ggplot(aes(targeted, turnout)) +
  geom_boxplot() +
  # facet_wrap(~party) +
  ggpubr::stat_compare_means() +
  EnvStats::stat_mean_sd_text()



spend_per_party  %>% 
  # filter(party == "D66") %>% View()
  right_join(dict_pc4)%>% 
  group_by(regio_code, party) %>% 
  summarize(total_spend=mean(total_spend, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(tk2023 %>% 
              mutate(regio_code = str_remove_all(lau_id, "M")) %>% 
              select(regio_code, pvv2023=pvv) %>% 
              left_join(tk2021 %>% select(regio_code, pvv2021 =pvv )) ) %>% 
  mutate(diff = pvv2023-pvv2021) %>% 
  drop_na(party) %>% 
  ggplot(aes(total_spend,diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Increase Vote Share PVV") +
  facet_wrap(~party, scales = "free")




spend_per_party  %>% 
  filter(party == "GroenLinks-PvdA") %>% 
  
  # filter(party == "D66") %>% View()
  right_join(dict_pc4)%>% 
  group_by(regio_code, party) %>% 
  summarize(total_spend=mean(total_spend, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(tk2023 %>% 
              mutate(regio_code = str_remove_all(lau_id, "M")) %>% 
              select(regio_code, gl_pvd_a2023=gl_pvd_a, pop_2021, pop_dens_2) %>% 
              left_join(tk2021 %>% 
                          mutate(gl_pvd_a2021 = (partij_van_de_arbeid_p_v_d_a + groenlinks)/opkomst) %>% select(regio_code, gl_pvd_a2021)) ) %>% 
  mutate(diff = gl_pvd_a2023-gl_pvd_a2021) %>%
  mutate(total_spend = ifelse(is.nan(total_spend), 0, total_spend)) %>% 
  drop_na(party) %>% 
  # mutate(total_spend = total_spend/pop_2021) %>% 
  ggplot(aes(pop_dens_2,diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor() +
  # scale_x_log10() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Increase Vote Share GL-PvdA (2021-2023)", x = "Population Density") +
  
  # labs(y = "Increase Vote Share GL-PvdA (2021-2023)", x = "Loggeed Per Capita Spend on Postcode by GL-PvdA") +
  theme_minimal()
  # facet_wrap(~party, scales = "free")



# when you look at it by party spending, the negative correlation seems to be somewhat driven by D66 though

