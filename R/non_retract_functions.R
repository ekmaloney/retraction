identify_no_og_date <- function(retracted_paper_info, 
                                         rw_db_info){
  
  rw_db_info <- rw_db_info %>% select(doi_for_requesting, original_paper_date) %>% 
                mutate(doi_for_requesting = paste0("https://doi.org/",
                                                   doi_for_requesting))
  
  need_hand_code <- left_join(retracted_paper_info, 
                        rw_db_info, by = "doi_for_requesting") %>% 
              select(id,title, host_venue_info, works_type, original_paper_date) %>% 
              filter(is.na(original_paper_date)) %>% 
              unnest(host_venue_info, names_sep = "_") %>% select(-host_venue_info_issn) %>% 
              distinct()
  
  return(need_hand_code)
  
  
}


a_d <- join_all_data_together(paper_info_df = paper_info,
                              no_date_info_df = code_final_date)

join_all_data_together <- function(paper_info_df, no_date_info_df){
  
  paper_info_df_selection <- paper_info_df %>% 
                             select(id, publication_date, host_venue_info) %>% 
                             unnest(host_venue_info, names_sep = "_") %>% 
                             select(-host_venue_info_issn) %>% 
                             filter(!is.na(publication_date) & !is.na(host_venue_info_id)) %>% 
                             distinct() %>% 
                             rowwise() %>% 
                             mutate(paper_id = id,
                                    new_date = lubridate::ymd(publication_date),
                                    publication_year = lubridate::floor_date(new_date, unit = "year"),
                                    new_pub_year = format(as.Date(publication_year, format="%Y/%m/%d"),"%Y"),
                                    only_id = str_remove(host_venue_info_id, "https://openalex.org/"),
                                    url = paste0("http://api.openalex.org/works?filter=host_venue.id:",
                                                      only_id, ",publication_date:", new_date),
                                    num_pages = ceiling(openalexR::openalex_api(url)$meta$count/25),
                                    ieee_name = NA_character_) %>% 
                            distinct() %>% 
                            select(paper_id, pub_date = new_date, url, ieee_name, num_pages)
  
  
  no_date_info_selection <- no_date_info_df %>% 
                            mutate(workid = str_remove_all(id, "https://openalex.org/"),
                                   api_call = paste0("https://api.openalex.org/",workid),
                                   api_call = trimws(api_call, which = "both"),
                                   only_id = str_remove(host_venue_info_id, "https://openalex.org/")) %>% 
                            filter(!is.na(paper_date_coded)) %>% 
                            mutate(date = if_else(paper_date_coded == "May-51", "05/01/1951",
                                                  paper_date_coded),
                                   ieee = if_else(str_detect(host_venue_info_publisher, "IEEE"), "yes",
                                                  "no")) %>% 
                            rowwise() %>% 
                            mutate(new_date = 
                                     case_when(ieee == "yes" ~ ymd(openalexR::openalex_api(api_call)[["publication_date"]]),
                                               TRUE ~ mdy(date)),
                                   url = case_when(ieee == "yes" ~
                                                     paste0("https://api.openalex.org/works?filter=host_venue.publisher:ieee,publication_date:",
                                                            new_date),
                                                   TRUE ~ paste0("http://api.openalex.org/works?filter=host_venue.id:",
                                                                 only_id, ",publication_date:", new_date)),
                                   num_pages = ceiling(openalexR::openalex_api(url)$meta$count/25),
                                   ieee_name = if_else(ieee == "yes", host_venue_info_name, NA_character_)) %>% 
                            select(paper_id = id,
                                   pub_date = new_date,
                                   url, ieee_name,
                                   num_pages)
  
  
  all_info_sample <- bind_rows(paper_info_df_selection, no_date_info_selection)
  
  sampled_papers <- tibble()
  set.seed(123)
  for(i in 490:nrow(all_info_sample)){
    
    
    possibleError <- tryCatch(
      get_paper_results(paper_id = all_info_sample$paper_id[i],
                        pub_date = all_info_sample$pub_date[i],
                        url = all_info_sample$url[i],
                        ieee_name = all_info_sample$ieee_name[i],
                        num_pages = all_info_sample$num_pages[i]),
      
      error = function(e) e
    )
    
    if(!inherits(possibleError, "error")){
          #REAL WORK
          s <- get_paper_results(paper_id = all_info_sample$paper_id[i],
                                 pub_date = all_info_sample$pub_date[i],
                                 url = all_info_sample$url[i],
                                 ieee_name = all_info_sample$ieee_name[i],
                                 num_pages = all_info_sample$num_pages[i])
          
          s <- s %>% mutate(original_paper_id = all_info_sample$paper_id[i])

          sampled_papers <- bind_rows(sampled_papers, s)
        }
  }
  
  sampled_papers <- sampled_papers %>% select(-abstract_inverted_index)
  return(sampled_papers)
  
}

get_paper_results <- function(paper_id, 
                              pub_date, 
                              url,
                              ieee_name,
                              num_pages){
  
  
  if(!is.na(ieee_name)){
    q <- openalexR::get_all_data_for_query(url, num_pages)
    #q <- q$results
    
    q_final <- q %>% 
              filter(host_venue$display_name == ieee_name) %>% 
              filter(id != paper_id) %>% filter(is_retracted == FALSE) %>% 
              filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract"))
  }else if(num_pages == 0){
    new_url <- str_replace_all(url, "publication_date", "from_publication_date")
    new_url <- paste0(new_url, "&sort=publication_date")
    
    new_q <- openalexR::openalex_api(new_url)
    new_q <- new_q$results
    
    q_final <- new_q %>% filter(id != paper_id) %>% 
      filter(is_retracted == FALSE) %>% 
      filter(publication_date == min(publication_date)) %>% 
      filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract"))
  
    }else{
      q <- openalexR::get_all_data_for_query(url, num_pages)
      #q <- q$results
    
    q_final <- q %>% filter(id != paper_id) %>% 
              filter(is_retracted == FALSE) %>% 
      filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract"))
    
    if(nrow(q_final) == 0){
      new_url <- str_replace_all(url, "publication_date", "from_publication_date")
      new_url <- paste0(new_url, "&sort=publication_date")
      
      new_q <- openalexR::openalex_api(new_url)
      new_q <- new_q$results
      
      q_final <- new_q %>% filter(id != paper_id) %>% 
        filter(is_retracted == FALSE)  %>% 
        filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract")) %>% 
        filter(publication_date == min(publication_date))
    }
    
  }
  
  selected <- q_final[sample(nrow(q_final), 1), ]
  
  return(selected)
  
}



combine_retracted_sampled <- function(retracted_df, sampled_df){
  
  sampled_df_authors <- bind_rows(sampled_df[1:381,], sampled_df[383:517,])
  
  sampled_df_authors <- sampled_df %>% 
                        rowwise() %>% 
                        mutate(author_length = length(authorships)) %>% 
                        filter(author_length != 0) %>% 
                        select(original_paper_id, authorships) %>% 
                        unnest(authorships) %>%
                        mutate(authors_id = author$id,
                               authors_name = author$display_name,
                               authors_orcid = author$orcid) %>% 
                        select(original_paper_id, 
                               authors_id,
                               authors_name,
                               authors_orcid) %>% 
                        distinct() %>% mutate(type = "sampled")
  
  
  retracted_df_authors <- retracted_df %>% 
                          select(original_paper_id = id,
                                 authors_id,
                                 authors_name,
                                 authors_orcid) %>% distinct() %>% 
                          mutate(type = "retracted")
  
  
  all_authors <- bind_rows(retracted_df_authors, 
                           sampled_df_authors)
  
}
