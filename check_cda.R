

east_groninge <- c(9663L, 9665L, 9631L, 9640L, 9641L, 9642L, 9644L, 9645L, 9646L, 9648L, 9540L, 9541L, 9545L, 9550L, 9551L, 9560L, 9561L, 9563L, 9566L, 9695L, 9696L, 9697L, 9698L, 9699L, 9670L, 9671L, 9672L, 9673L, 9674L, 9675L, 9677L, 9678L, 9679L, 9681L, 9682L, 9684L, 9685L, 9686L, 9687L, 9688L, 9691L, 9693L, 9942L, 9943L, 9944L, 9500L, 9501L, 9502L, 9503L, 9580L, 9581L, 9584L, 9585L, 9591L, 9661L)

east_groninge <- as.character(east_groninge)





all_zip <- election_dat30 %>%
  filter(type == "location") %>%
  filter(location_type == "zips") %>%
  mutate(total_spend_formatted = total_spend_pct*total_spend_formatted) %>%
  filter(!is_exclusion) %>%
  group_by(internal_id, total_spend_pct, party) %>%
  mutate(n_together = n()) %>%
  # select(page_id, value, num_ads, total_spend_formatted, n_together)
  mutate(total_spend_formatted_old = total_spend_formatted) %>%
  mutate(total_spend_formatted = total_spend_formatted/n_together) %>%
  select(party, internal_id, value, n_together, total_spend_formatted, total_spend_formatted_old) %>%
  ungroup() %>%
  arrange(value) %>%
  group_by(value, party) %>%
  summarize(total_spend = sum(total_spend_formatted),
            total_spend_old = sum(total_spend_formatted_old)) %>%
  ungroup() %>%
  # filter(party == "VVD")  %>%
  dplyr::mutate(pc4 = str_remove_all(value, ", Netherlands") %>% as.numeric) %>%
  left_join(cbs %>%
              janitor::clean_names())



all_zip %>% 
  filter(party == "CDA") %>% 
  filter(pc4 %in% east_groninge) %>% View()




# # Create the heading for the party
# cat("\n") 
# cat("##### ", partyz, "\n")

# Prepare the data for plotting
party_data <- geo %>%
  left_join(all_zip %>%
              filter(party == "CDA") %>%
              mutate(pc4_code = str_remove_all(value, ", Netherlands"))) %>% 
  filter(prov_name == "Groningen") %>% 
  mutate(east_gron = pc4 %in% east_groninge)

# Extract the color for the party
party_color <- color_dat %>%
  filter(party == "CDA") %>%
  pull(colors) %>%
  first()  # Make sure 'color' column has exactly one color per party

# Print the plot
# print(
  ggplot(party_data) +
    geom_sf(aes(fill = total_spend_old), colour = NA, size = 0.1) +
    scale_fill_gradient2(low = 'lightgrey', high = party_color, na.value = 'lightgrey') +
    theme_void() +
    theme(legend.position = 'bottom') +
    guides(fill = guide_colourbar(title = 'Spend (€)', barwidth = 10, barheight = 0.5)) +
    geom_sf_label(aes(label = pc4, alpha = east_gron))
# )

  
  
  
  
  
  
  length(east_groninge)
  
  
  
  
  election_dat30 %>%
    filter(type == "location") %>%
    filter(location_type == "zips") %>%
    mutate(total_spend_formatted = total_spend_pct*total_spend_formatted) %>%
    filter(is_exclusion) %>%
    group_by(internal_id, total_spend_pct, party) %>%
    mutate(n_together = n()) %>%
    # select(page_id, value, num_ads, total_spend_formatted, n_together)
    mutate(total_spend_formatted_old = total_spend_formatted) %>%
    mutate(total_spend_formatted = total_spend_formatted/n_together) %>%
    select(party, internal_id, value, n_together, total_spend_formatted, total_spend_formatted_old) %>%
    ungroup() %>%
    arrange(value)  %>%
    dplyr::mutate(pc4 = str_remove_all(value, ", Netherlands") %>% as.numeric) %>% 
    mutate(east_gron = pc4 %in% east_groninge) %>% View()
  
  
  
  election_dat30 %>%
    filter(type == "location") %>%
    filter(location_type == "zips") %>%
    mutate(total_spend_formatted = total_spend_pct*total_spend_formatted) %>% 
    filter(party == "CDA")  %>% 
    dplyr::mutate(pc4 = str_remove_all(value, ", Netherlands") %>% as.numeric) %>%
    filter(pc4 == "9644") %>% 
    mutate(east_gron = pc4 %in% east_groninge) %>% View()
  