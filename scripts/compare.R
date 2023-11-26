# nllll <- readRDS("data/NL (1).rds")
# nllll2 <- readRDS("data/NL.rds")

# setwd("C:/Users/fabio/Dropbox/postdoc/bs/TK2023")
elex2021reps <- readRDS("data/elex2021reps.rds")
add_them <- readRDS("data/add_them.rds")

# source("party_utils.R")

dat2021 <- elex2021reps  %>%
  janitor::clean_names() %>% 
  mutate(page_id = as.character(page_id)) %>% 
  left_join(readRDS("data/all_dat.rds") %>%
              distinct(page_id, party)) %>%
  drop_na(party) %>% 
  mutate(amount_spent_eur = readr::parse_number(amount_spent_eur)) %>% 
  mutate(amount_spent_eur = ifelse(amount_spent_eur==100, 50, amount_spent_eur)) %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) %>% 
  # pull(days_until) %>% sort(decreasing = T)
  filter(days_until <= 112) %>% 
  filter(days_until >= 0) %>% 
  group_by(date) %>% 
  summarize(amount_spent_eur = sum(amount_spent_eur)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(`Daily Spend` = cumsum(amount_spent_eur))    %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) 
  
dat2021_hi <- elex2021reps  %>%
  janitor::clean_names() %>% 
  mutate(page_id = as.character(page_id)) %>% 
  left_join(readRDS("data/all_dat.rds") %>%
              distinct(page_id, party)) %>%
  drop_na(party) %>% 
  mutate(amount_spent_eur = readr::parse_number(amount_spent_eur)) %>% 
  # mutate(amount_spent_eur = ifelse(amount_spent_eur==100, 50, amount_spent_eur)) %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) %>% 
  # pull(days_until) %>% sort(decreasing = T)
  filter(days_until <= 112) %>% 
  filter(days_until >= 0) %>% 
  group_by(date) %>% 
  summarize(amount_spent_eur = sum(amount_spent_eur)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(`Daily Spend` = cumsum(amount_spent_eur))    %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) 


dat2021_lo <- elex2021reps  %>%
  janitor::clean_names() %>% 
  mutate(page_id = as.character(page_id)) %>% 
  left_join(readRDS("data/all_dat.rds") %>%
              distinct(page_id, party)) %>%
  drop_na(party) %>% 
  mutate(amount_spent_eur = readr::parse_number(amount_spent_eur)) %>% 
  mutate(amount_spent_eur = ifelse(amount_spent_eur==100, 1, amount_spent_eur)) %>%
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) %>% 
  # pull(days_until) %>% sort(decreasing = T)
  filter(days_until <= 112) %>% 
  filter(days_until >= 0) %>% 
  group_by(date) %>% 
  summarize(amount_spent_eur = sum(amount_spent_eur)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(`Daily Spend` = cumsum(amount_spent_eur))    %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) 

calc2023 <- function(est = 50) {
  
  
  
  more_data <- #the_daaaat %>%
    readr::read_rds("lifelong/NL.rds")  %>%
    mutate(date_produced = lubridate::ymd(date)) %>%
    drop_na(date_produced) %>%
    janitor::clean_names()%>% #rename(advertiser_id = page_id) %>%
    mutate(spend = readr::parse_number(as.character(amount_spent_eur))) %>%
    mutate(spend = ifelse(amount_spent_eur==100, est, amount_spent_eur)) %>% 
    # mutate(spend = ifelse(spend == 100, 50, spend)) %>%
    # distinct(page_id, .keep_all = T) %>%
    filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie", negate = T)) %>%
    mutate(page_id = as.character(page_id)) %>%
    # filter(cntry == "NL") %>%
    filter(date_produced >= lubridate::as_date("2023-08-01"))
  
  
  # me_advertisers <- read_csv("../data/wtm-advertisers-gr-2023-05-20T08_49_00.571Z.csv")
  hc_data_cum_raw <-  more_data %>%
    # mutate(advertiser_id = as.character(advertiser_id)) %>%
    left_join(readRDS("data/all_dat.rds") %>%
                distinct(page_id, party)) %>% 
    drop_na(party) %>%
    group_by(date_produced) %>%
    summarize(spend  = sum(spend)) %>%
    ungroup() %>%
    # spread(key = party, value = spend, fill = 0) %>%
    # arrange(date_produced) %>%
    # mutate(across(starts_with("50PLUS"):(last_col()), ~cumsum(.), .names = "cumulative_{.col}")) %>%
    # select(date_produced, starts_with("cumulative")) %>%
    # rename_with(~str_remove(., "cumulative_"), starts_with("cumulative")) %>%
    # pivot_longer(-date_produced, names_to = "party", values_to = "spend")  %>%
    ##### THIS NEEDS TO CHANGE FOR OTHER COUNTRIES
    bind_rows(add_them %>%
                group_by(date_produced) %>%
                summarize(spend  = sum(spend)) %>%
                ungroup() ) #%>%
  ##### THIS NEEDS TO CHANGE FOR OTHER COUNTRIES
  # group_by(party) %>%
  # mutate(total_spend = max(spend)) %>%
  # ungroup()  %>%
  # left_join(color_dat) %>%
  # mutate(party = as.factor(party)) %>%
  # mutate(party = fct_reorder(party, total_spend))
  
  
  hc_data_cumfb <- hc_data_cum_raw %>%
    mutate(Date = date_produced) %>%
    # group_by(party) %>%  # Assuming you have a 'party' column
    arrange(Date) %>%
    mutate(`Daily Spend` = spend - first(spend)) %>%
    ungroup() %>%  
    mutate(days_until = as.numeric(lubridate::ymd("2023-11-22")-Date)) 
  
  return(hc_data_cumfb)
}


hc_data_cumfb <- calc2023(50)
hc_data_cumfb_lo <- calc2023(1)
hc_data_cumfb_hi <- calc2023(100)

boundaries <- dat2021_hi %>% mutate(election = "2021") %>% select(election, days_until, hi_spend = `Daily Spend`) %>% 
  left_join(dat2021_lo %>% mutate(election = "2021") %>% select(election, days_until, lo_spend = `Daily Spend`)) %>% 
  bind_rows(  hc_data_cumfb_hi %>% mutate(election = "2023") %>% select(election, days_until, hi_spend = `Daily Spend`) %>% 
                left_join(hc_data_cumfb_lo %>% mutate(election = "2023") %>% select(election, days_until, lo_spend = `Daily Spend`))
  )



the_dat <- hc_data_cumfb %>% mutate(election = "2023") %>%
  bind_rows(dat2021 %>% mutate(election = "2021")) %>% 
  left_join(boundaries)

current_one <- the_dat %>% 
  filter(election == "2023") %>% 
  filter(days_until==min(days_until)) %>% pull(days_until) %>% .[1] %>% magrittr::add(1)

the_dat %>% 
  filter(days_until == current_one) %>% 
  select(election, `Daily Spend`) %>% 
  pivot_wider(names_from = election, values_from = `Daily Spend`) %>% 
  janitor::clean_names() %>% 
  mutate(diff = x2021-x2023)


comparison <- the_dat %>% select(Date, cumulative_spend = `Daily Spend`, days_until, election) %>%
  filter(days_until <= 112) 

write_csv(comparison, "data/comparison.csv")

the_dat %>% 
  ggplot(aes(days_until, `Daily Spend`, color = election)) +
  geom_ribbon(aes(ymin = lo_spend, ymax= hi_spend), fill = NA, linetype = "dashed") +
  geom_line(size = 2.4) +
  scale_x_reverse() +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format()) +
  labs(y = "Cumulative Spent on Meta Platforms\n", x = "Days Until Election Day")  +
  ggthemes::scale_color_colorblind("Dutch Election") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 15)) +
  labs(caption = "Source: Meta Ad Library Report. Note: Data for 2021 comes with boundaries because daily data below 100 Euro needs to be estimated.", title = "Dutch parties spend considerably less in 2023 election compared to 2021")

ggsave("img/spending.png", width = 10, height = 6, dpi = 900, bg = "white")

# ----------------------------------------




# nllll <- readRDS("data/NL (1).rds")
# nllll2 <- readRDS("data/NL.rds")

# setwd("C:/Users/fabio/Dropbox/postdoc/bs/TK2023")
elex2021reps <- readRDS("data/elex2021reps.rds")
add_them <- readRDS("data/add_them.rds")

# source("party_utils.R")


more_data <- #the_daaaat %>%
  readr::read_rds("lifelong/NL.rds")  %>%
  mutate(date_produced = lubridate::ymd(date)) %>%
  drop_na(date_produced) %>%
  janitor::clean_names()%>% #rename(advertiser_id = page_id) %>%
  mutate(spend = readr::parse_number(as.character(amount_spent_eur))) %>%
  mutate(spend = ifelse(amount_spent_eur==100, 50, amount_spent_eur)) %>% 
  # mutate(spend = ifelse(spend == 100, 50, spend)) %>%
  # distinct(page_id, .keep_all = T) %>%
  filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie", negate = T)) %>%
  mutate(page_id = as.character(page_id)) %>%
  # filter(cntry == "NL") %>%
  filter(date_produced >= lubridate::as_date("2023-08-01"))


# me_advertisers <- read_csv("../data/wtm-advertisers-gr-2023-05-20T08_49_00.571Z.csv")
hc_data_cum_raw <-  more_data %>%
  # mutate(advertiser_id = as.character(advertiser_id)) %>%
  left_join(readRDS("data/all_dat.rds") %>%
              distinct(page_id, party)) %>% 
  drop_na(party) %>%
  group_by(date_produced, party) %>%
  summarize(spend  = sum(spend)) %>%
  ungroup() %>%
  # spread(key = party, value = spend, fill = 0) %>%
  # arrange(date_produced) %>%
  # mutate(across(starts_with("50PLUS"):(last_col()), ~cumsum(.), .names = "cumulative_{.col}")) %>%
  # select(date_produced, starts_with("cumulative")) %>%
  # rename_with(~str_remove(., "cumulative_"), starts_with("cumulative")) %>%
  # pivot_longer(-date_produced, names_to = "party", values_to = "spend")  %>%
  ##### THIS NEEDS TO CHANGE FOR OTHER COUNTRIES
  bind_rows(add_them) #%>%
##### THIS NEEDS TO CHANGE FOR OTHER COUNTRIES
# group_by(party) %>%
# mutate(total_spend = max(spend)) %>%
# ungroup()  %>%
# left_join(color_dat) %>%
# mutate(party = as.factor(party)) %>%
# mutate(party = fct_reorder(party, total_spend))


hc_data_cumfb <- hc_data_cum_raw %>%
  mutate(Date = date_produced) %>%
  group_by(party) %>%  # Assuming you have a 'party' column
  arrange(Date) %>%
  mutate(`Daily Spend` = spend - first(spend)) %>%
  ungroup()  %>%  
  mutate(days_until = as.numeric(lubridate::ymd("2023-11-22")-Date)) %>% 
  filter(days_until == min(days_until))



dat2021 <- elex2021reps  %>%
  janitor::clean_names() %>% 
  mutate(page_id = as.character(page_id)) %>% 
  left_join(readRDS("data/all_dat.rds") %>%
              distinct(page_id, party)) %>% 
  drop_na(party) %>% 
  mutate(amount_spent_eur = readr::parse_number(amount_spent_eur)) %>% 
  mutate(amount_spent_eur = ifelse(amount_spent_eur==100, 50, amount_spent_eur)) %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) %>% 
  # pull(days_until) %>% sort(decreasing = T)
  filter(days_until <= 112) %>% 
  filter(days_until >= 0) %>% 
  group_by(date, party) %>% 
  summarize(amount_spent_eur = sum(amount_spent_eur)) %>% 
  ungroup() %>% 
  group_by(party) %>% 
  arrange(date) %>% 
  mutate(`Daily Spend` = cumsum(amount_spent_eur))    %>% 
  ungroup() %>% 
  mutate(Date = lubridate::ymd(date))  %>% 
  mutate(days_until = as.numeric(lubridate::ymd("2021-03-15")-Date)) %>% 
  filter(days_until == hc_data_cumfb$days_until[1])


the_dat <- hc_data_cumfb %>% mutate(election = "2023") %>%
  bind_rows(dat2021 %>% mutate(election = "2021")) 

# current_one <- the_dat %>% 
  # filter(election == "2023") %>% 
  # filter(days_until==min(days_until)) %>% pull(days_until) %>% .[1] %>% magrittr::add(1)

# the_dat %>% 
#   filter(days_until == current_one) %>% 
#   select(election, `Daily Spend`) %>% 
#   pivot_wider(names_from = election, values_from = `Daily Spend`) %>% 
#   janitor::clean_names() %>% 
#   mutate(diff = x2021-x2023)


# comparison <- the_dat %>% select(Date, cumulative_spend = `Daily Spend`, days_until, election) %>%
#   filter(days_until <= 112) 
# 
# write_csv(comparison, "data/comparison.csv")

the_dat %>% 
  filter(!(party %in% c("GO", "Alliantie", "Politiek Op Maat", "Nieuwe Democratie", "Piratenpartij"))) %>% 
  mutate(party = fct_reorder(party, `Daily Spend`)) %>% 
  ggplot(aes(party, `Daily Spend`, color = election, fill = election, group = party)) +
  # geom_segment(aes(xend = )) +
  geom_line(size = 2.4, color = "grey") +
  # scale_x_reverse() +
  geom_point() +
  # geom_col(position = position_dodge()) +
  theme_minimal() +
  scale_y_continuous(labels = scales::number_format()) +
  labs(y = "Meta Spending From 112 Days to Election Day", x = "")  +
  # scale_color_grey() +
  theme(legend.position = "top") +
  coord_flip() +
  ggthemes::scale_color_fivethirtyeight()


the_dat %>%
  filter(election == "2023")  %>% select(days_until, party, spend2023 = `Daily Spend`) %>% 
  left_join(the_dat %>%
              filter(election == "2021") %>% select(days_until, party, spend2021 = `Daily Spend`)) %>% 
  select(-days_until) %>% 
  mutate(spend2021 = ifelse(is.na(spend2021), 0, spend2021)) %>% 
  mutate(diff = spend2023-spend2021) %>% 
  arrange(desc(diff)) %>% View()

