#' Fix Subject Column
#'
#' @param d dataframe of retraction watch database
#'
#' @return long data frame, only including papers tagged Psychology
#' @export
#'
#' @examples
fix_subject_column <- function(d){
  
  d_long <- d %>% janitor::clean_names() %>% separate(subject, into = c("sub_1", "sub_2", "sub_3", "sub_4",
                                             "sub_5", "sub_6", "sub_7", "sub_8",
                                             "sub_9", "sub_10", "sub_11"), 
                           sep = ";") %>% 
            pivot_longer(sub_1:sub_11, names_to = "subject_number", 
                         values_to = "subject") %>% filter(!is.na(subject)) %>% 
            separate(subject, into = c("subject_level_1", "subject_level_2"),
                     sep = "-") %>% 
            filter(subject_level_1 == "(SOC) Psychology") %>% 
            mutate(doi_unavailable = case_when((retraction_doi == "Unavailable" |
                                                 retraction_doi == "unavailable") &
                                                 (original_paper_doi == "Unavailable" |
                                                    original_paper_doi == "unavailable") ~ "no_doi",
                                               TRUE ~ "doi"))
  
}

get_open_alex_info <- function(doi){
  
  if(str_detect(doi, "https")){
    paper_res <- openalex_api(doi)
  } else{
    paper_res <- find_work(id_type = "doi",
                            id = doi)
  }
  
  paper_info <- paper_res %>% 
                mutate(doi_for_requesting = doi) %>% 
                unnest(authors, names_sep= "_")
  
  
  
}

select_doi_to_request <- function(d){
  og_doi_info <- d %>% 
                filter(doi_unavailable == "doi") %>% 
                mutate(doi_for_requesting = case_when(original_paper_doi == "unavailable" |
                                                        original_paper_doi == "Unavailable" ~ retraction_doi,
                                                      TRUE ~ original_paper_doi)) %>% 
                mutate(id = row_number()) %>% 
                select(record_id, title, journal, author, doi_for_requesting, original_paper_date)
  
  return(og_doi_info)
}

get_papers <- function(d){
  
  d_only_with_dois <- d %>% 
                      filter(doi_unavailable == "doi") %>% 
                      mutate(doi_for_requesting = case_when(original_paper_doi == "unavailable" |
                                                              original_paper_doi == "Unavailable" ~ retraction_doi,
                                                            TRUE ~ original_paper_doi)) %>% 
                      mutate(id = row_number())
  
}

identify_issue_dois <- function(d){
  #paper_info <- tibble()
  issue_ids <- tibble()
  
  for (i in 1:nrow(d)) {
    
    possibleError <- tryCatch(
      get_open_alex_info(d$doi_for_requesting[i]),
      error = function(e) e
    )
    
    # if(!inherits(possibleError, "error")){
    #   #REAL WORK
    #   interim_paper_info <- get_open_alex_info(rw_dois$doi_for_requesting[i])
    #   paper_info <- bind_rows(paper_info, interim_paper_info)
    # }
    
    if(inherits(possibleError, "error")){
      issue_recordid <- d$record_id[i]
      issue_ids <- c(issue_ids, issue_recordid)
    }
    
  }  #end for
  
  return(issue_ids)
}


get_paper_info <- function(d){
  paper_info <- tibble()
  issue_ids <- tibble()
  
  for (i in 1:nrow(d)) {
    
    possibleError <- tryCatch(
      get_open_alex_info(d$doi_for_requesting[i]),
      error = function(e) e
    )
    
    if(!inherits(possibleError, "error")){
      #REAL WORK
      interim_paper_info <- get_open_alex_info(d$doi_for_requesting[i])
      paper_info <- bind_rows(paper_info, interim_paper_info)
    }
    
    if(inherits(possibleError, "error")){
      issue_recordid <- d$record_id[i]
      issue_ids <- c(issue_ids, issue_recordid)
    }
    
  }  #end for
  
  return(paper_info)
}

getting_papers_coauthors <- function(ids){
  
  i <- str_remove_all(ids, "https://openalex.org/")
  
  m <- openalexR::get_authors_papers(id_type = "openalex",
                                     id = i)
  
  m_long <- m %>% 
    unnest(authorships) %>% 
    select(id, display_name, publication_date, 
           author, institutions, author_position, concepts, cited_by_count, 
           doi, is_retracted)
  
  return(m_long)
  
}


poss_coauthors = purrr::possibly(.f = getting_papers_coauthors, otherwise = NULL)
