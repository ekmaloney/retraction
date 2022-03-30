#' Get all Retracted Papers
#'
#' @return
#' @export
#'
#' @examples
get_all_retracted_papers <- function(){
  
  n <- get_number_of_pages("https://api.openalex.org/works?filter=is_retracted:TRUE,concepts.id:C15744967")
  
  all_urls <- tibble(page_number = seq(from = 1, to = n, by = 1),
                     url = paste0("https://api.openalex.org/works?filter=is_retracted:TRUE,concepts.id:C15744967&page=",
                                  page_number))
  
  data <- all_urls$url %>% map_df(get_paper_info)
  
}

#' Get Paper Info
#'
#' @param u 
#'
#' @return
#' @export
#'
#' @examples
get_paper_info <- function(u){
  
  openalex_api(u)$results
}

join_data <- function(oa_data, rw){
  
  oa_data_joining <- oa_data %>% 
                     mutate(doi_join = str_remove_all(doi, "https://doi.org/"))
  
  rw_data_joining <- rw %>% 
                     pivot_longer(c(retraction_doi, original_paper_doi), 
                                  names_to = "doi_type",
                                  values_to = "doi_join")
  
  all_data <- left_join(oa_data_joining, rw_data_joining, by = "doi_join") %>% 
              select(-doi_type) %>% 
              distinct() %>% filter(!is.na(doi_join)) %>% janitor::clean_names() %>% 
              mutate(rw = if_else(!is.na(record_id), 1, 0))
  
}

# getting_papers_coauthors <- function(ids){
#   
#   possibleError <- tryCatch(
#     openalexR::get_authors_papers(id_type = "openalex",
#                                   id = ids),
#     error = function(e) e
#   )
#   
#   if(!inherits(possibleError, "error")){
#     #REAL WORK
#     m <- openalexR::get_authors_papers(id_type = "openalex",
#                                        id = ids)
#     
#     m_long <- m %>% 
#       unnest(authorships) %>% 
#       select(id, display_name, publication_date, 
#              author, institutions, author_position, concepts, cited_by_count, 
#              doi, is_retracted)
#     
#     return(m_long)
#   }
#   
# }