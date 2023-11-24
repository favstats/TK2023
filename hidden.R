library(playwrightr)
# library(tidyverse)

source("utils.R")

options(python_init = TRUE)


# install.packages("pacman")



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
# if (Sys.info()[["sysname"]] == "Windows") {
  # xxxx <- F
  pw_init(use_xvfb = F)
  
  # ggl_spend <- readRDS("data/ggl_spend.rds")
  
  






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


get_content <- function(.x) {
  
  
  page_df %>% get_
  
  library(playwrightr)
  page_df <- page_df %>% get_by_selector("input")
  # set_input_files(page_df, "media/img_hash/0bb19614c4abcf9520712d9d3a5aaa36.jpg")
  
  files <- .x$fp
  library(reticulate)
  reticulate::py_run_string(glue("{page_df$elem_id}[{page_df$id-1}].set_input_files(['{files}'])"))
  
  page_df <- page_df %>% get_by_placeholder("Message ChatGPT…")
  
  print("stage 1")
  
  # page_df %>% click()
  
  yp<- paste0('You are an Expert at Assessing a Political Advertisement. Your task is to analyze a political advertisement for various elements. Keep your outputs short and to the point, preferring one word generic statements over long-winded explanations. Visual Content Analysis: Identify and list all key characters appearing in the advertisement. Specify details such as:
Number of people.
Notable features (e.g., attire, age, gender).
Expressions or actions (e.g., speaking, gesturing).
Name of the individuals displayed if the image clearly implies who is who, otherwise leave blank.


Analyze the setting/background:
Indoor or outdoor.
Any notable landmarks or symbols (e.g., flags, buildings).
Presence of text or slogans.
Graphic design or not.

Thematic Analysis:
Determine the central themes or messages.
Analyze the use of persuasive techniques (e.g., appeal to emotion, logical arguments).
Assess the portrayal of issues or opposing viewpoints.
Assess the overall goal of the ad, could be persuasion, mobilization, fundraising.

Keyword Analysis:
Extract key words or phrases.
Categorize keywords by relevance to political topics (e.g., economy, health, security).

Sentiment Analysis:
Evaluate the overall sentiment of the advertisement (positive, negative, neutral).
Assess how different elements contribute to the sentiment (visuals, audio, text).

Content Analysis:
OCR all text and also extract any claims, attacks, issues discussed or pledges made.

Output Format:
Present findings in a structured format, with clear sections for each analysis aspect. Only output a JSON file and no text explaining below or above. See template below:

{
  "advertisement_analysis": {
    "visual_content": {
      "key_characters": [
        {
          "character_id": 1,
          "features": {
            "attire": "business suit",
            "age": "mid-50s",
            "gender": "male",
	    "likely_name: "Wybren van Haga"
  },
  "actions": "speaking to a group"
  },
{
  "character_id": 2,
  "features": {
    "attire": "casual",
    "age": "early-30s",
    "gender": "female",
    "likely_name": "Sybille Hoofstraad"
  },
  "actions": "listening attentively"
}
],
"setting": {
  "location": "outdoor",
  "notable_elements": ["flag", "city skyline"],
  "presence_of_text": "Yes"
}
},
"thematic_analysis": {
  "overarching_issue": "economy",
  "central_themes": ["economic growth", "community development"],
  "persuasive_techniques": ["appeal to emotion", "anecdotal evidence"],
  "issue_portrayal": "optimistic view on economic policies"
  "ad_goal": "persuasion"
},
"keyword_analysis": {
  "keywords": ["economy", "growth", "community", "future"],
  "political_relevance": {
    "economy": "high",
    "health": "low",
    "security": "medium"
  }
},
"content": {
  "text": "full text of the entire ad",
  "claims": "claims made in ad...",
  "attacks": "attacks or criticisms in ad...",
  "issues_discussed": "Specific issues being discussed...",
  "pledges": "pledges made..."
},
"sentiment_analysis": {
  "overall_sentiment": "positive",
  "contributing_elements": {
    "visuals": "inspiring",
    "themes": "hopeful",
    "text": "encouraging"
  }
}
}
} 


THIS IS THE TEXT ABOVE THE IMAGE INCLUDE THAT INFORMATION IN YOUR ANALYSIS TOO FOR CONTEXT: ', .x$text)
  # debugonce(playwrightr::type)
  # page_df %>% playwrightr::type(yp)
  
  print(yp)
  
  print("stage 1.5")
  
  yp <- str_remove_all(yp, '\n') %>% str_remove_all("'")
  
  tryCatch({
    # Your code to run
    py_run_string(glue("{page_df$elem_id}[{page_df$id - 1}].type('{yp}')"))
  }, error = function(e) {
    # Code to run in case of an error
    the_error <<- e$message
  })
  
  Sys.sleep(30)
  # 
  # # Now error_result contains the error object if an error occurred
  # if (!is.null(the_error)) {
  #   cat("An error occurred:", the_error, "\n")
  #   
  #   
  #   yp<- paste0('THIS IS THE TEXT ABOVE THE IMAGE INCLUDE THAT INFORMATION IN YOUR ANALYSIS TOO FOR CONTEXT: ', .x$text)
  #   # Further processing with error_result
  #   py_run_string(glue("{page_df$elem_id}[{page_df$id - 1}].type('{str_remove_all(yp, '\n')}')"))
  #   
  #   the_error <<- NULL
  # }
  
  print("stage 1.75")
  
  # Sys.sleep()
  
  page_df <- page_df %>% get_by_placeholder("Message ChatGPT…")
  
  print("stage 2")
  
  
  page_df %>% playwrightr::press("Enter")
  
  print("stage 3")
  
  
  Sys.sleep(30)
  
  # page_df <- page_df %>% get_by_selector(".agent-turn")
  pp <- page_df %>% playwrightr::get_content()
  
  print("stage 4")
  
  pp %>% 
    html_elements('.agent-turn') %>% 
    html_element('code') %>% 
    html_text() %>% 
    str_trim() %>% str_squish() %>% .[length(.)] %>% jsonlite::fromJSON() %>% 
    jsonlite::write_json(paste0("content/",str_remove_all(.x$ad_id, "\\.png|\\.jpg|media/img_hash/"), ".json"))
  
  
}


page_df %>%
  goto("https://chat.openai.com/")

get_content <- purrr::possibly(get_content, quiet = F, otherwise = NULL)

already_there <- dir("content") %>% 
  str_remove_all("\\.json")

hash_df <- read_csv("media/hash_table.csv") %>% 
  mutate(ad_id_simple = str_remove_all(ad_id, "adid_|_[:digit:]|_[:digit:][:digit:]"))




fb_dat <- readRDS("data/fb_dat.rds")  %>% 
  mutate(text = paste0(ad_creative_body, ad_creative_link_caption, ad_creative_link_description, ad_creative_link_title))

dir("media/img_hash/", full.names = T) %>% 
  tibble(fp = .) %>% #View()
  sample_n(n()) %>% 
  mutate(merger = str_remove_all(fp, "\\.png|\\.jpg|media/img_hash/")) %>% #View()
  left_join(fb_dat %>% 
              rename(ad_id_simple = id) %>% 
              left_join(hash_df) %>% mutate(merger = hash)) %>% 
  distinct(merger, text, .keep_all = T) %>% 
  filter(str_detect(ad_id, paste0(already_there, collapse = "|"), negate = T)) %>% #View()
  split(1:nrow(.)) %>% 
  walk_progress(get_content)
