# nllll <- readRDS("data/NL (1).rds")
# nllll2 <- readRDS("data/NL.rds")

# setwd("C:/Users/fabio/Dropbox/postdoc/bs/TK2023")
elex2021reps <- readRDS("data/elex2021reps.rds")

dat2021 <- elex2021reps  %>%
  janitor::clean_names() %>% 
  mutate(page_id = as.character(page_id)) %>% 
  left_join(election_dat30 %>%
              distinct(page_id, party) %>% 
              select(page_id, party)) %>% 
  drop_na(party) %>% 
  mutate(amount_spent_eur = readr::parse_number(amount_spent_eur)) %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) %>% 
  # pull(days_until) %>% sort(decreasing = T)
  filter(days_until <= 112) %>% 
  group_by(date) %>% 
  summarize(amount_spent_eur = sum(amount_spent_eur)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(`Daily Spend` = cumsum(amount_spent_eur))    %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) 
  

more_data <- readr::read_rds("lifelong/NL.rds")  %>%
  mutate(date_produced = lubridate::ymd(date)) %>%
  drop_na(date_produced) %>% 
  janitor::clean_names()%>% #rename(advertiser_id = page_id) %>%
  mutate(spend = readr::parse_number(as.character(amount_spent_eur))) %>%
  # mutate(spend = ifelse(spend == 100, 50, spend)) %>%
  # distinct(page_id, .keep_all = T) %>%
  filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T)) %>%
  mutate(page_id = as.character(page_id)) %>% 
  # filter(cntry == "NL") %>% 
  filter(date_produced >= lubridate::as_date("2023-08-01"))


# me_advertisers <- read_csv("../data/wtm-advertisers-gr-2023-05-20T08_49_00.571Z.csv")
hc_data_cum_raw <-  more_data %>%
  # mutate(advertiser_id = as.character(advertiser_id)) %>%
  left_join(election_dat30 %>%
              distinct(page_id, party) %>% 
              select(page_id, party)) %>% 
  drop_na(party) %>%
  group_by(date_produced) %>%
  summarize(spend  = sum(spend)) %>%
  # ungroup() %>%
  # spread(key = party, value = spend, fill = 0) %>%
  # arrange(date_produced) %>%
  # mutate(across(starts_with("50PLUS"):(last_col()), ~cumsum(.), .names = "cumulative_{.col}")) %>%
  # select(date_produced, starts_with("cumulative")) %>%
  # rename_with(~str_remove(., "cumulative_"), starts_with("cumulative")) %>%
  # pivot_longer(-date_produced, names_to = "party", values_to = "spend")  %>%
  # group_by(party) %>%
  mutate(total_spend = max(spend))# %>%
  # ungroup()  %>%
  # left_join(color_dat) %>%
  # mutate(party = as.factor(party)) %>%
  # mutate(party = fct_reorder(party, total_spend))

# saveRDS(hc_data, "../data/hc_data.rds")
# 
# color_order <- hc_data_cum_raw %>%
#   distinct(party, .keep_all = T) %>%
#   arrange(party) %>%
#   pull(colors)
# 
# # more_data %>% 


hc_data_cumfb <- hc_data_cum_raw %>%
  mutate(Date = date_produced) %>%
  # group_by(party) %>%  # Assuming you have a 'party' column
  arrange(Date) %>% 
  mutate(`Daily Spend` = spend - first(spend)) #%>%
  # ungroup() 


the_dat <- hc_data_cumfb %>% mutate(election = "2023") %>%  
  mutate(days_until = as.numeric(lubridate::ymd("2023-11-22")-Date)) %>%
  bind_rows(dat2021 %>% mutate(election = "2021")) 

current_one <- the_dat %>% 
  filter(election == "2023") %>% 
  filter(days_until==min(days_until)) %>% pull(days_until)

the_dat %>% 
  filter(days_until ==current_one) %>% 
  select(election, `Daily Spend`) %>% 
  pivot_wider(names_from = election, values_from = `Daily Spend`) %>% 
  janitor::clean_names() %>% 
  mutate(diff = x2021-x2023)

the_dat %>% 
  ggplot(aes(days_until, `Daily Spend`, color = election)) +
  geom_line(size = 2.4) +
  scale_x_reverse() +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format()) +
  labs(y = "Cumulative Spent/n", x = "Days Until Election Day")  +
  scale_color_grey() +
  theme(legend.position = "top")
