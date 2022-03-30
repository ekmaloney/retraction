
setwd("~/Desktop/dissertation/stigma network/")

devtools::install_github("ekmaloney/openalexR")
library(openalexR)
library(httr)
library(tidyverse)
set_config(config(ssl_verifypeer = FALSE))
options(RCurlOptions = list(ssl_verifypeer = FALSE))
options(rsconnect.check.certificate = FALSE)

fix_subject_column <- function(d){
  d_long <- d %>% janitor::clean_names() %>% separate(subject, into = c("sub_1", "sub_2", "sub_3", "sub_4",
                                                                        "sub_5", "sub_6", "sub_7", "sub_8",
                                                                        "sub_9", "sub_10", "sub_11"),
                                                      sep = fixed(";")) %>%
    pivot_longer(sub_1:sub_11, names_to = "subject_number",
                 values_to = "subject") %>% filter(!is.na(subject)) %>%
    separate(subject, into = c("subject_level_1", "subject_level_2"),
             sep = fixed("-")) %>%
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
    paper_res <- find_paper(id_type = "doi",
                            id = doi)
  }
  paper_info <- tibble::tibble(doi = doi,
                               open_alex_id = paper_res$id,
                               open_alex_j_id = paper_res$host_venue$id,
                               author_position = paper_res$authorships$author_position,
                               author_name = paper_res$authorships$author$display_name,
                               author_oa_id = paper_res$authorships$author$id,
                               author_orcid = paper_res$authorships$author$orcid,
                               cited_by = paper_res$cited_by_count,
                               concepts = tidyr::nest(paper_res$concepts, data = everything()))
}
coalesce_all_dois_can_find <- function(d, doi_replacement_file){
  rows_with_issues <- c(8, 33, 55, 57, 74, 102, 117, 124, 129, 131, 137, 143, 156,
                        157, 163, 168, 181, 183, 184, 185, 189, 196, 216, 237, 238,
                        264, 269, 270, 274, 287, 303, 313, 323, 355, 365, 379, 384, 388,
                        393,400, 401, 402, 404, 407, 410, 413, 414, 416, 417, 421,
                        425, 426, 428, 429, 439, 446, 447, 448, 584, 635, 651, 660,
                        667, 671)
  og_doi_info <- d %>%
    filter(doi_unavailable == "doi") %>%
    mutate(doi_for_requesting = case_when(original_paper_doi == "unavailable" |
                                            original_paper_doi == "Unavailable" ~ retraction_doi,
                                          TRUE ~ original_paper_doi)) %>%
    mutate(id = row_number()) %>%
    filter(!(id %in% rows_with_issues)) %>%
    select(title, journal, author, doi_for_requesting)
  replacement_doi_info <- doi_replacement_file %>%
    filter(str_detect(doi_that_works, "https:")) %>%
    select(title, journal, author, doi_for_requesting = doi_that_works)
  all_doi_info <- bind_rows(og_doi_info, replacement_doi_info) %>% filter(!is.na(doi_for_requesting))
  return(all_doi_info)
}
get_papers <- function(d){
  d_only_with_dois <- d %>%
    filter(doi_unavailable == "doi") %>%
    mutate(doi_for_requesting = case_when(original_paper_doi == "unavailable" |
                                            original_paper_doi == "Unavailable" ~ retraction_doi,
                                          TRUE ~ original_paper_doi)) %>%
    mutate(id = row_number())
}
getAllWorksByAuthor <- function(authorID) {
  url <- paste0("https://api.openalex.org/works?filter=author.id:", authorID)
  apiCall <- httr::content(httr::GET(url))
  paperID <- lapply(apiCall$results, '[[', 1)
  pubDate <- lapply(apiCall$results, '[[', 3)
  authorships_ <- lapply(apiCall$results, '[[', 'authorships')
  authorshipsExtract_ <- vector("list", length = length(authorships_))
  for(i in 1:length(authorships_)) {
    authorshipsExtract_[[i]] <- authorships_[[i]] %>% lapply(., '[[', 'author') %>% 
      lapply(., '[', 'id') %>%  
      unlist() %>% 
      data.frame(authors_id_ = .)
  }
  authorBibliogBasicInfo <- tibble(paperID= unlist(paperID), pubDate= unlist(pubDate)) %>%
    mutate(authorID = authorID)
  authorBibliogWCoAuthors <- authorBibliogBasicInfo %>% 
    mutate(authors = map(authorshipsExtract_, "authors_id_")) %>% unnest_longer(authors)
  return(authorBibliogWCoAuthors)
  Sys.sleep(.2)
}

# read in data
retracted_author_info <- readRDS("./retracted_author_info.RDS")
# get list of author IDs
authorIDlist <- as.list(retracted_author_info$author_oa_id) %>% 
  lapply(., str_remove_all, "https://openalex.org/")
# apply function that gets all papers and coauthor info per author
bibliogs <- authorIDlist %>% lapply(., getAllWorksByAuthor)

## flag which have been retracted:
papersRetracted <- retracted_author_info %>% 
  select(paperID = open_alex_id) %>% 
  mutate(isRetracted = 1)

bibliogs <- bibliogs %>% 
  purrr::map(~ left_join(., papersRetracted, by = "paperID") %>% 
               mutate(., isRetracted = ifelse(is.na(isRetracted), 0, isRetracted))
  )

bibliogs <- bibliogs %>% 
  purrr::map(~ unique(.))

saveRDS(bibliogs, "retractedAuthorsAllWorks.RDS")
  
### testing/graveyard
# testing <- httr::content(httr::GET("https://api.openalex.org/works?filter=author.id:A2584487321"))
# 
# url <- paste0("https://api.openalex.org/works?filter=author.id:", authorID)
# apiCall <- httr::content(httr::GET(url))
# paperID <- lapply(testing$results, '[[', 1)
# pubDate <- lapply(testing$results, '[[', 3)
# authorIDs <- lapply(testing$results, '[', 6)  %>% lapply(., '[[', 1) %>%  lapply(., '[[', 3) %>%  lapply(., '[[', 1) %>%  lapply(., '[[', 1)
# authorBibliogBasicInfo <- tibble(paperID= unlist(paperID), pubDate= unlist(pubDate)) %>% 
#   mutate(authorID = authorID, by = "authorID")









