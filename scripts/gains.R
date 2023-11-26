


library(tidyverse)


readRDS("historic/2023-11-09/7.rds") %>% 
  distinct(page_id, .keep_all = T) %>% 
  filter(is.na(no_data)) %>% 
  group_by(party) %>% 
  summarise(total_spend_formatted = sum(total_spend_formatted)) %>% 
  bind_rows(readRDS("historic/2023-11-02/7.rds") %>% 
              distinct(page_id, .keep_all = T) %>% 
              filter(is.na(no_data)) %>% 
              group_by(party) %>% 
              summarise(total_spend_formatted = sum(total_spend_formatted))) %>% 
  bind_rows(readRDS("historic/2023-10-26/7.rds") %>% 
              distinct(page_id, .keep_all = T) %>% 
              filter(is.na(no_data)) %>% 
              group_by(party) %>% 
              summarise(total_spend_formatted = sum(total_spend_formatted))) %>% 
  bind_rows(readRDS("historic/2023-10-19/7.rds") %>% 
              distinct(page_id, .keep_all = T) %>% 
              filter(is.na(no_data)) %>% 
              group_by(party) %>% 
              summarise(total_spend_formatted = sum(total_spend_formatted)))  %>% 
  group_by(party) %>% 
  summarise(total_spend_formatted = sum(total_spend_formatted)) %>% 
  arrange(desc(total_spend_formatted))
# 
# readRDS("historic/2023-11-02/7.rds") %>% count(ds)
# 
# 
# readRDS("historic/2023-10-26/7.rds") %>% count(ds)
# 
# 
# readRDS("historic/2023-10-19/7.rds") %>% count(ds)



hc_data_dafb %>% 
  filter(between(date_produced, as.Date("2023-10-18"), as.Date("2023-11-08"))) %>% 
  group_by(party) %>% 
  summarize(spend = sum(`Daily Spend`)) %>% 
  arrange(desc(spend)) %>% 
  bind_rows(
    hc_data_daggl %>%
      mutate(`Daily Spend` = spend) %>%
      mutate(Date = date_produced)  %>% 
      filter(between(date_produced, as.Date("2023-10-18"), as.Date("2023-11-08"))) %>% 
      group_by(party) %>% 
      summarize(spend = sum(`Daily Spend`)) %>% 
      arrange(desc(spend))  
  ) %>% 
  group_by(party) %>% 
  summarize(spend = sum(spend)) %>% 
  ungroup()%>% 
  arrange(desc(spend)) 




# tibble::tribble(
#              ~party, ~seats,
#              "PVV",  37,
#               "VVD",  24,
#   "GroenLinks-PvdA",   25,
#               "D66",   9,
#              "Volt Nederland",   2,
#              "Ja21",   1,
#               "CDA",   5,
#    "ChristenUnie",    3,
#               "SGP",    3,
#               "BBB",    7,
#   "FvD",    3,
#   "50PLUS",    1,
#   "BVNL",    0,
#   "DENK",    3,
#   "FvD",    3,
#   "BIJ1",    0,
#   "Libertaire Partij", 1,  
#   "PvdD", 1  ,  
#   "SP", 5  ,  
#   "NSC", 20  
#   ) %>% 
tibble::tribble(
  ~party, ~Seats.in.2023, ~seats,
  "PVV",            36L,               19L,
  "GroenLinks-PvdA",            25L,                8L,
  "VVD",            24L,              -10L,
  "NSC",            20L,               20L,
  "D66",            10L,              -14L,
  "SP",             5L,               -4L,
  "CDA",             5L,              -10L,
  "BBB",             5L,                4L,
  "FvD",             4L,               -4L,
  "PvdD",             4L,               -2L,
  "DENK",             3L,                0L,
  "SGP",             3L,                0L,
  "ChristenUnie",             3L,               -2L,
  "Volt Nederland",             2L,               -1L,
  "Ja21",             1L,               -2L,
  "50PLUS",             0L,               -1L,
  "BIJ1",             0L,               -1L
) %>% 
  left_join(
    readRDS("historic/2023-11-20/30.rds") %>% 
      distinct(page_id, .keep_all = T) %>% 
      filter(is.na(no_data)) %>% 
      group_by(party) %>% 
      summarise(spend = sum(total_spend_formatted)) %>% 
      ungroup() %>% 
      bind_rows(
        hc_data_daggl %>%
          mutate(`Daily Spend` = spend) %>%
          mutate(Date = date_produced)  %>% 
          filter(between(date_produced, as.Date("2023-10-21"), as.Date("2023-11-19"))) %>% 
          group_by(party) %>% 
          summarize(spend = sum(`Daily Spend`)) %>% 
          arrange(desc(spend)) 
      ) %>% 
      mutate(party = str_trim(party)) %>% 
      group_by(party) %>% 
      summarize(spend = sum(spend)))  %>% 
  arrange(party) %>% 
  distinct(party, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(spend = ifelse(party == "NSC", 0, spend)) %>% 
  # filter(party != "GroenLinks-PvdA") %>% 
  ggplot(aes(spend, seats)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = party)) +
  theme_minimal() +
  labs(y = "Seats Gains and Losses", x = "Spent on Meta + Google Ads") +
  scale_x_continuous(labels = scales::comma_format()) +
  ggpubr::stat_cor(label.x.npc = "center") +
  geom_smooth(method = "lm")


Party,Spending
VVD,1302768
GroenLinks-PvdA,507790
D66,469738
Volt,256086
JA21,249931
CDA,169090
ChristianUnion,72292
SGP,66194
BBB,18131



