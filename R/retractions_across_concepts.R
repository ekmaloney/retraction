concepts <- openalex_api("https://api.openalex.org/concepts?filter=level:0")

n <- get_number_of_pages("https://api.openalex.org/concepts?filter=level:0")

all_concepts <- get_all_data_for_query(base_page_path = "https://api.openalex.org/concepts?filter=level:0",
                                       number_of_pages = n) %>% 
  mutate(concept_id = str_remove_all(id, "https://openalex.org/"))


q <- openalex_api("https://api.openalex.org/works?filter=concepts.id:C121955636&group_by=is_retracted")



get_number_of_retractions_per_field <- function(concept_id){
  
  u <- paste0("https://api.openalex.org/works?filter=concepts.id:",
              concept_id, "&group_by=is_retracted")
  
  num <- openalex_api(u)$group_by
  
  return(num)
  
}


u <- paste0("https://api.openalex.org/works?filter=is_retracted:TRUE")


psych_retractions <- tibble(page_number = seq(from = 1, to = 100, by = 1),
                            u = paste0(u, "&page=", page_number))


get_paper_info <- function(url){
  openalex_api(url)$results
}

psych_retractions_sample <- psych_retractions %>% slice(1:3)

all_paper_retractions <- psych_retractions_sample$u %>% 
  map_df(get_paper_info)

get_paper_info(psych_retractions$u[4])

start_time <- Sys.time()
all_concepts$retraction_numbers <- all_concepts$concept_id %>% 
  map(get_number_of_retractions_per_field)
end_time <- Sys.time()

all_concepts_num_retraction <- all_concepts %>% 
  select(concept_id, display_name, retraction_numbers) %>% 
  unnest(retraction_numbers) %>% 
  mutate(count = as.integer(count)) %>% 
  mutate(retraction = if_else(key == 0, 
                              "not_retracted", 
                              "retracted")) %>% 
  select(-key) %>% 
  pivot_wider(names_from = retraction, values_from = count) %>% 
  mutate(percent_papers_retracted = retracted/(retracted+not_retracted))







