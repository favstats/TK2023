library(playwrightr)
# library(tidyverse)

source("utils.R")

options(python_init = TRUE)


# install.packages("pacman")


##TODO: change this if you want to defunct it
defunct <- T

pacman::p_load(
  vroom,
  reticulate,
  fs,
  janitor,
  progress,
  tidyverse,
  countrycode,
  lubridate,
  glue,
  rvest,
  cli,
  digest
)


# conda_install(packages = "fcntl", pip = T)
if (Sys.info()[["sysname"]] == "Windows") {
  # xxxx <- F
  pw_init(use_xvfb = F)
  
  ggl_spend <- readRDS("data/ggl_spend.rds")
  
  
} else{
  # xxxx <- T
  
  
  conda_install(packages = "xvfbwrapper", pip = T)
  
  print("installed xvfbwrapper")
  conda_install(packages = "playwright", pip = T)
  
  print("installed playwright")
  
  pw_init(use_xvfb = T)
  
  print("pw initted")
  # Launch the browser
  system("playwright install")
  
  
  dir.create("data/ggl", recursive = T)
  
  
  download.file(url = "https://storage.googleapis.com/political-csv/google-political-ads-transparency-bundle.zip",
                destfile = "data/ggl/ggl.zip",
                method = "curl")
  
  unzip("data/ggl/ggl.zip", exdir = "data/ggl")
  
  # google_political_ads_advertiser_stats %>%
  #   filter(str_detect(Regions, "NL")) %>% View()
  
  # wdman::gecko()
  
  Sys.sleep(10)
  
  gglstats <-
    vroom::vroom("data/ggl/google-political-ads-creative-stats.csv")
  
  ggl_spend <- gglstats   %>%
    mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
    filter(Date_Range_Start >= as.Date("2023-08-01")) %>%
    filter(str_detect(Regions, "NL")) %>%
    distinct(Advertiser_ID, Advertiser_Name, .keep_all = T) %>%
    mutate(
      party1 = case_when(
        str_detect(
          Advertiser_Name,
          "VVD|Volkspartij voor Vrijheid en Democratie|Stichting Liberaal Dordrecht"
        ) ~ "VVD",
        str_detect(Advertiser_Name, "\\bCDA\\b|Christen Democratisch") ~ "CDA",
        str_detect(
          Advertiser_Name,
          "PvdA|Jonge Socialisten|Partij van de Arbeid"
        ) ~ "GroenLinks-PvdA",
        str_detect(Advertiser_Name, "\\bD66\\b|Jonge Democraten|Democraten 66") ~ "D66",
        str_detect(Advertiser_Name, "GroenLinks|\\bGL\\b") ~ "GroenLinks-PvdA",
        str_detect(Advertiser_Name, "ChristenUnie|\\bCU\\b") ~ "ChristenUnie",
        str_detect(Advertiser_Name, "\\bSP\\b|Socialistische Partij") ~ "SP",
        str_detect(Advertiser_Name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(Advertiser_Name, "50.lus|50PLUS|VLG") ~ "50PLUS",
        str_detect(
          Advertiser_Name,
          "\\bSGP\\b|Staatkundig Gereformeerde Partij"
        ) ~ "SGP",
        str_detect(Advertiser_Name, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(Advertiser_Name, "PVV|Partij .oor de Vrijheid") ~ "PVV",
        str_detect(Advertiser_Name, "DENK") ~ "DENK",
        str_detect(Advertiser_Name, "Volt Nederland") ~ "Volt Nederland",
        str_detect(Advertiser_Name, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(
          Advertiser_Name,
          "BVNL|Belang Van Nederland|Engel Huibert van Dalen"
        ) ~ "BVNL",
        str_detect(Advertiser_Name, "Ja21|JA21|Conservatieve Liberalen") ~ "JA21",
        # str_detect(Advertiser_Name, "Alliantie") ~ "Alliantie",
        str_detect(Advertiser_Name, "BBB|Marc-Michel Strijker") ~ "BBB",
        T ~ NA_character_
      )
    ) %>%
    filter(!(
      str_detect(
        Advertiser_Name,
        "Gleichheitspartei|Nieuw-Vlaamse|SP Digital LLC|MURRAY|REVOLT|Angelenos Against Higher Property Taxes|ITALIA|Volt Deutschland"
      )
    )) %>%
    drop_na(party1) %>%
    bind_rows(readRDS("data/ggl_spend.rds")) %>%
    mutate(party1 = ifelse(
      party1 %in% c("GroenLinks", "PvdA"),
      "GroenLinks-PvdA",
      party1
    )) %>%
    distinct(Advertiser_ID, .keep_all = T)
  
  saveRDS(ggl_spend, "data/ggl_spend.rds")
  
  
}





# ggl_spend <- readRDS("data/ggl_spend.rds")

# readr::read_rds("https://github.com/favstats/TK2023/raw/main/data/ggl_spend.rds")


print("Launch the browser")

browser_df <- browser_launch(
  headless = F,
  browser = "firefox",
  user_agent = NULL,
  user_data_dir = "out"
)


print("headlesss")
# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


print("sooo")
pw_restart <- function() {
  reticulate::py_run_string("p.stop()")
  pw_init(use_xvfb = T)
  reticulate::py_run_string("p = sync_playwright().start()")
}


print("sooo22")




# retrieve_spend_daily <- function(id, the_date, cntry = "NL") {
#   # id <- "AR18091944865565769729"
#   url <-
#     glue::glue(
#       "https://adstransparency.google.com/advertiser/{id}?political&region={cntry}&start-date={the_date}&end-date={the_date}&topic=political&hl=en"
#     )
#   
#   page_df %>%
#     goto(url)
#   
#   Sys.sleep(3)
#   
#   
#   
#   root3 <- "/html/body/div[3]"
#   root5 <- "/html/body/div[5]"
#   ending <-
#     "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"
#   
#   page_df <- page_df %>% get_by_label("Insights")
#   
#   page_df %>% press("Enter") %>% click()
#   
#   root <- root3
#   
#   Sys.sleep(3)
#   
#   pp <- page_df %>% playwrightr::get_content()
#   
#   ending_eur <-
#     "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
#   ending_ads <-
#     "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"
#   
#   print("retrieve numbers")
#   # try({
#   eur_amount <- pp %>%
#     html_elements(xpath = paste0(root, ending_eur)) %>%
#     html_text()
#   
#   if (length(eur_amount) == 0) {
#     message("empty")
#     root <- root5
#     
#     eur_amount <- pp %>%
#       html_elements(xpath = paste0(root, ending_eur)) %>%
#       html_text()
#     
#     
#   }
#   # })
#   num_ads <- pp %>%
#     html_elements(xpath = paste0(root, ending_ads)) %>%
#     html_text()
#   
#   # })
#   
#   fin <-
#     tibble(advertiser_id = id,
#            eur_amount,
#            num_ads,
#            date = the_date)
#   
#   print(fin)
#   
#   saveRDS(fin %>% bind_rows(readRDS("data/ggl_daily_spending.rds")) %>% distinct(), file = "data/ggl_daily_spending.rds")
#   
#   return(fin)
#   
# }
# 
# # debugonce(retrieve_spend_daily)
# retrieve_spend_daily <-
#   possibly(retrieve_spend_daily, otherwise = NULL, quiet = F)
# 
# # daily_spending <- readRDS("data/daily_spending.rds")
# # Apr 17, 2023 - May 16, 2023
# # 13 February 2023
# timelines <-
#   seq.Date(as.Date("2023-10-01"),
#            lubridate::today() - lubridate::days(1),
#            by = "day")
# 
# #
# daily_spending_old <- readRDS("data/ggl_daily_spending.rds") # %>% 
#   # filter(!(date  %in% lubridate::ymd(c("2023-11-16",  "2023-11-17")))) 
#   # filter(!(date  %in% lubridate::ymd(c("2023-11-15", "2023-11-16",  "2023-11-17",  "2023-11-18")))) #%>%
#   # filter(!(date  %in% lubridate::ymd(c("2023-11-18"))))
# 
# 
# daily_spending <-
#   expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
#   set_names(c("advertiser_id", "timelines")) %>%
#   anti_join(daily_spending_old %>% select(advertiser_id, timelines = date)) %>%
#   # slice(1) %>%
#   split(1:nrow(.)) %>%
#   map_dfr_progress( ~ {
#     retrieve_spend_daily(.x$advertiser_id, .x$timelines)
#   })
# 
# # missings <- expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
# #   set_names(c("advertiser_id", "timelines")) %>%
# #   anti_join(daily_spending %>% rename(timelines = date))  %>%
# #   split(1:nrow(.)) %>%
# #   map_dfr_progress(~{retrieve_spend_daily(.x$advertiser_id, .x$timelines)})
# #
# 
# saveRDS(daily_spending %>% bind_rows(daily_spending_old) %>% distinct(),
#         file = "data/ggl_daily_spending.rds")
# 

# dates <- read_csv("data/dates.csv")
latest_hist <- dir("historic") %>% sort %>% .[length(.)]

election_dat30 <- readRDS(paste0("historic/", latest_hist,  "/30.rds")) %>%
  filter(is.na(no_data))

fin <- (as.Date(election_dat30$ds[1])-lubridate::days(1))
begin7 <- fin-lubridate::days(6)
begin30 <- fin-lubridate::days(29)

dates <- tibble(fin,
       begin7,
       begin30)



retrieve_spend_custom <- function(id, from, to, cntry = "NL") {
  fin <- tibble()

  while (nrow(fin) == 0) {
    # id <- "AR14716708051084115969"
    # cntry<- "NL"
    # from <- dates$begin30
    # to <- dates$fin
    url <-
      glue::glue(
        "https://adstransparency.google.com/advertiser/{id}?political&region={cntry}&start-date={from}&end-date={to}&hl=en"
      )
    # remDr$navigate(url)


    page_df %>%
      goto(url)

    Sys.sleep(3)



    root3 <- "/html/body/div[3]"
    root5 <- "/html/body/div[5]"
    ending <-
      "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"

    page_df <- page_df %>% get_by_label("Insights")

    page_df %>% press("Enter") %>% click()

    root <- root3

    Sys.sleep(3)

    pp <- page_df %>% playwrightr::get_content()

    ending_eur <-
      "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
    ending_ads <-
      "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"

    print("retrieve numbers")
    # try({
    eur_amount <- pp %>%
      html_elements(xpath = paste0(root, ending_eur)) %>%
      html_text()

    if (length(eur_amount) == 0) {
      message("empty")
      root <- root5

      eur_amount <- pp %>%
        html_elements(xpath = paste0(root, ending_eur)) %>%
        html_text()


    }
    # })
    num_ads <- pp %>%
      html_elements(xpath = paste0(root, ending_ads)) %>%
      html_text()

    ending_type <-
      "/root/advertiser-page/insights-grid/div/div/ad-formats/widget/div[4]"


    type_spend <<- pp %>%
      html_elements(xpath = paste0(root, ending_type)) %>%
      html_children() %>%
      html_text() %>%
      tibble(raww = .) %>%
      mutate(type = str_to_lower(str_extract(raww, "Video|Text|Image"))) %>%
      mutate(raww = str_remove_all(raww, "Video|Text|Image") %>% str_remove_all("%|\\(.*\\)") %>% as.numeric) %>%
      pivot_wider(names_from = type, values_from = raww)


    fin <- tibble(advertiser_id = id, eur_amount, num_ads, from, to)

    if (nrow(type_spend) != 0) {
      fin <- fin %>%
        bind_cols(type_spend)
    }



    print(fin)

  }





  return(fin %>% mutate_all(as.character))

}

retrieve_spend_custom <-
  possibly(retrieve_spend_custom,
           otherwise = NULL,
           quiet = F)

ggl_sel_sp_old <- readRDS("data/ggl_sel_sp.rds")
ggl_sel_sp7_old <- readRDS("data/ggl_sel_sp7.rds")

# if (T) {
# if(!all(dates$begin30 == ggl_sel_sp_old$from)){
  ggl_sel_sp <- unique(ggl_spend$Advertiser_ID) %>%
    # .[1:3] %>%
    map_dfr_progress( ~ {
      retrieve_spend_custom(.x, dates$begin30, dates$fin)
    })

  # retrieve_spend_custom(unique(ggl_spend$Advertiser_ID)[1], dates$begin30, dates$fin)

  misssss <-
    ggl_sel_sp$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)
  # filter(!(advertiser_id %in% unique(ggl_spend$Advertiser_ID)))

  fvd <- misssss %>%
    # .[22] %>%
    map_dfr_progress( ~ {
      retrieve_spend_custom(.x, dates$begin30, dates$fin)
    })



  ggl_sel_sp <- ggl_sel_sp %>%
    bind_rows(fvd) %>%
    distinct(advertiser_id, .keep_all = T)


  # saveRDS(ggl_sel_sp, file = "data/ggl_sel_sp.rds")
  saveRDS(ggl_sel_sp, file = paste0("historic/", latest_hist, "/ggl30.rds"))
  
# }

  # if (!all(dates$begin7 == ggl_sel_sp7_old$from)) {
  ggl_sel_sp7 <- unique(ggl_spend$Advertiser_ID) %>%
    # .[22] %>%
    map_dfr_progress( ~ {
      retrieve_spend_custom(.x, dates$begin7, dates$fin)
    })

  misssss7 <-
    ggl_sel_sp7$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)

  # misss <- retrieve_spend_custom("AR14725485108811268097", dates$begin7, dates$fin)

  misss <- misssss7 %>%
    # .[22] %>%
    map_dfr_progress( ~ {
      retrieve_spend_custom(.x, dates$begin7, dates$fin)
    })


  # saveRDS(ggl_sel_sp7 %>% bind_rows(misss) %>%
  #           distinct(advertiser_id, .keep_all = T),
  #         file = "data/ggl_sel_sp7.rds")
  
  
  saveRDS(ggl_sel_sp7 %>% bind_rows(misss) %>%
            distinct(advertiser_id, .keep_all = T),
          file = paste0("historic/", latest_hist, "/ggl7.rds"))
  
  # latest_hist


# }

unlink("data/ggl", recursive = T, force = T)
# file.remove("data/ggl/google-political-ads-creative-stats.csv")




if(!defunct){
  
  
  
  ggl_sel_sp <- readRDS("data/ggl_sel_sp.rds")
  ggl_sel_sp7 <- readRDS("data/ggl_sel_sp7.rds")
  
  try({
    tt_ads <- ggl_sel_sp %>%
      rename(Advertiser_ID = advertiser_id) %>%
      left_join(ggl_spend %>% distinct(Advertiser_ID, party1))  %>%
      # mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
      # filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
      group_by(party1) %>%
      summarize(total_num_ads = sum(parse_number(num_ads))) %>%
      # count(party1, name = "total_num_ads") %>%
      mutate(total_num_ads = scales::comma(total_num_ads)) %>%
      pivot_wider(names_from = party1, values_from = total_num_ads) %>%
      mutate(party_col = "Number of Ads")
    
    
    ttl_spn <- ggl_sel_sp %>%
      rename(Advertiser_ID = advertiser_id) %>%
      left_join(ggl_spend %>% distinct(Advertiser_ID, party1)) %>%
      mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\."))) %>%
      group_by(party1) %>%
      summarize(Spend_EUR = sum(Spend_EUR)) %>%
      arrange(desc(Spend_EUR)) %>%
      select(party = party1, spend = Spend_EUR) %>%
      mutate(spend = scales::comma(spend)) %>%
      mutate(spend = paste0("€", spend)) %>%
      drop_na() %>%
      pivot_wider(names_from = party, values_from = spend) %>%
      mutate(party_col = "Total Spend")
    
    
    
    tp_spnders <- ggl_sel_sp %>%
      rename(Advertiser_ID = advertiser_id) %>%
      left_join(
        ggl_spend %>% distinct(Advertiser_ID, party1, .keep_all = T) %>% select(Advertiser_ID, party1, Advertiser_Name)
      ) %>%
      mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\.")))   %>%
      group_by(Advertiser_Name, Advertiser_ID, party1) %>%
      summarize(Spend_EUR = sum(Spend_EUR)) %>%
      ungroup() %>%
      group_by(party1) %>%
      arrange(desc(Spend_EUR)) %>%
      slice(1:3) %>%
      mutate(Spend_EUR = scales::comma(Spend_EUR)) %>%
      mutate(n_words = str_count(Advertiser_Name, " ")) %>%
      # mutate(lab = paste0(word(str_remove(page_name, "-"), 1,ifelse(n_words>=2, 3, 2), sep=" "), "<br>(€", total_spend_formatted, ")")) %>%
      # mutate(lab = paste0(Advertiser_Name, " (€", Spend_EUR, ")")) %>%
      # mutate(
      #   lab = paste0(
      #     '<a href="https://adstransparency.google.com/advertiser/',
      #     Advertiser_ID,
      #     '?region=NL&topic=political" target="_blank" title="See ads for yourself" style="color: black; text-decoration: none">',
      #     Advertiser_Name,
      #     '</a> (€',
      #     # currency_symbol,
      #     Spend_EUR,
    #     ')'
    #   )
    # ) %>%
    mutate(
      lab =  glue::glue('[{Advertiser_Name}](https://adstransparency.google.com/advertiser/{Advertiser_ID}?region=NL&topic=political) (€{Spend_EUR})', .open = "{", .close = "}")
    ) %>%
      select(party1, lab) %>%
      drop_na() %>%
      summarize(lab = paste0("<br>", 1:n(), ". ", lab, collapse = "")) %>%
      mutate(lab = paste0(lab, "<br><br>")) %>%
      pivot_wider(names_from = party1, values_from = lab) %>%
      mutate(party_col = "Top Spenders")
    
    ggl_all <- tt_ads %>%
      bind_rows(tp_spnders) %>%
      bind_rows(ttl_spn) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Coalizione/Partito") %>%
      set_names(.[nrow(.), ] %>% as.character()) %>%
      slice(1:(n() - 1))
    
    
    saveRDS(ggl_all, file = "data/ggl_all.rds")
    
    
    
    
    
    
    ggl_sel_sp7 <- ggl_sel_sp7 %>%
      filter(num_ads != "0")
    
    tt_ads <- ggl_sel_sp7 %>%
      rename(Advertiser_ID = advertiser_id) %>%
      left_join(ggl_spend %>% distinct(Advertiser_ID, party1))  %>%
      # mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
      # filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
      group_by(party1) %>%
      summarize(total_num_ads = sum(parse_number(num_ads))) %>%
      # count(party1, name = "total_num_ads") %>%
      mutate(total_num_ads = scales::comma(total_num_ads)) %>%
      pivot_wider(names_from = party1, values_from = total_num_ads) %>%
      mutate(party_col = "Number of Ads")
    
    
    ttl_spn <- ggl_sel_sp7 %>%
      rename(Advertiser_ID = advertiser_id) %>%
      left_join(ggl_spend %>% distinct(Advertiser_ID, party1)) %>%
      mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\."))) %>%
      group_by(party1) %>%
      summarize(Spend_EUR = sum(Spend_EUR)) %>%
      arrange(desc(Spend_EUR)) %>%
      select(party = party1, spend = Spend_EUR) %>%
      mutate(spend = scales::comma(spend)) %>%
      mutate(spend = paste0("€", spend)) %>%
      drop_na() %>%
      pivot_wider(names_from = party, values_from = spend) %>%
      mutate(party_col = "Total Spend")
    
    
    
    tp_spnders <- ggl_sel_sp7 %>%
      rename(Advertiser_ID = advertiser_id) %>%
      left_join(
        ggl_spend %>% distinct(Advertiser_ID, party1, .keep_all = T) %>% select(Advertiser_ID, party1, Advertiser_Name)
      ) %>%
      mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\.")))   %>%
      group_by(Advertiser_Name, Advertiser_ID, party1) %>%
      summarize(Spend_EUR = sum(Spend_EUR)) %>%
      ungroup() %>%
      group_by(party1) %>%
      arrange(desc(Spend_EUR)) %>%
      slice(1:3) %>%
      mutate(Spend_EUR = scales::comma(Spend_EUR)) %>%
      mutate(n_words = str_count(Advertiser_Name, " ")) %>%
      # mutate(lab = paste0(word(str_remove(page_name, "-"), 1,ifelse(n_words>=2, 3, 2), sep=" "), "<br>(€", total_spend_formatted, ")")) %>%
      # mutate(lab = paste0(Advertiser_Name, " (€", Spend_EUR, ")")) %>%
      # mutate(
      #   lab = paste0(
      #     '<a href="https://adstransparency.google.com/advertiser/',
      #     Advertiser_ID,
      #     '?region=NL&topic=political" target="_blank" title="See ads for yourself" style="color: black; text-decoration: none">',
      #     Advertiser_Name,
      #     '</a> (€',
      #     # currency_symbol,
      #     Spend_EUR,
    #     ')'
    #   )
    # ) %>%
    mutate(
      lab =  glue::glue('[{Advertiser_Name}](https://adstransparency.google.com/advertiser/{Advertiser_ID}?region=NL&topic=political) (€{Spend_EUR})', .open = "{", .close = "}")
    ) %>%
      select(party1, lab) %>%
      drop_na() %>%
      summarize(lab = paste0("<br>", 1:n(), ". ", lab, collapse = "")) %>%
      mutate(lab = paste0(lab, "<br><br>")) %>%
      pivot_wider(names_from = party1, values_from = lab) %>%
      mutate(party_col = "Top Spenders")
    
    ggl_all7 <- tt_ads %>%
      bind_rows(tp_spnders) %>%
      bind_rows(ttl_spn) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Coalizione/Partito") %>%
      set_names(.[nrow(.), ] %>% as.character()) %>%
      slice(1:(n() - 1))
    
    
    saveRDS(ggl_all7, file = "data/ggl_all7.rds")
    
    
    
  })
  
  
  
}


unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

