
setwd("~/Desktop/dissertation/stigma network/")

devtools::install_github("ekmaloney/openalexR")
library(openalexR)
library(httr)
library(tidyverse)
set_config(config(ssl_verifypeer = FALSE))
options(RCurlOptions = list(ssl_verifypeer = FALSE))
options(rsconnect.check.certificate = FALSE)

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
}
# read in data
sampledAuthorInfo <- readRDS("./sampled_papers.RDS")
sampledAuthorInfo_simple <- sampledAuthorInfo %>% 
  select(id, doi, authorships) %>% unique()
for(i in 1:nrow(sampledAuthorInfo_simple)) {
  sampledAuthorInfo_simple$authorCount[i] <- sampledAuthorInfo_simple$authorships[[i]] %>% nrow()
}
sampledAuthorInfo_expanded <- sampledAuthorInfo_simple %>% 
  uncount(authorCount, .remove = F)

authors <- sampledAuthorInfo_simple %>% 
  split(.$doi) %>% lapply(., '[[', 3) %>% lapply(., '[[', 1) %>% 
  lapply(., data.frame) #%>% bind_rows() 
for(i in 1:length(authors)) {
  authors[[i]]$sampledPaperDOI <- names(authors)[i]
}
authors <- authors %>% 
  bind_rows() 
sampledPapersAuthors <- cbind(data.frame(author_position = authors$author_position,
                                         sampledPaperDOI = authors$sampledPaperDOI),
      authors$author %>% select(authorID = id, display_name)) %>% 
  left_join(., sampledAuthorInfo_simple, by = c("sampledPaperDOI" = "doi")) %>% 
  dplyr::select(-c(authorships, authorCount)) 

# get list of author IDs
authorIDlist <- as.list(sampledPapersAuthors$authorID) %>% 
  lapply(., str_remove_all, "https://openalex.org/")
# apply function that gets all papers and coauthor info per author
bibliogs <- authorIDlist %>% lapply(., getAllWorksByAuthor)

bibliogs <- bibliogs %>% 
  purrr::map(~ unique(.))

bind_rows(bibliogs) %>% 
  write_csv(., "sampledAuthorsAllWorks.csv")

saveRDS(bibliogs, "sampledAuthorsAllWorks.RDS")

### testing/graveyard

edgelistList <- vector("list", length = nrow(sampledPapersAuthors))
for(i in 844:nrow(sampledPapersAuthors)) {
  url <- paste0("https://api.openalex.org/works?filter=author.id:", str_remove_all(sampledPapersAuthors$authorID[i], "https://openalex.org/"))
  apiCall <- httr::content(httr::GET(url))
  paperID <- lapply(apiCall$results, '[[', 1)
  pubDate <- lapply(apiCall$results, '[[', 4)
  authorships_ <- lapply(apiCall$results, '[[', 'authorships')
  authorshipsExtract_ <- vector("list", length = length(authorships_))
  for(k in 1:length(authorships_)) {
    authorshipsExtract_[[k]] <- authorships_[[k]] %>% lapply(., '[[', 'author') %>% 
      lapply(., '[', 'id') %>%  
      unlist() %>% 
      data.frame(authors_id_ = .)
  }
  authorBibliogBasicInfo <- tibble(paperID= unlist(paperID), pubDate= unlist(pubDate)) %>%
    mutate(authorID = sampledPapersAuthors$authorID[i])
  authorBibliogWCoAuthors <- authorBibliogBasicInfo %>% 
    mutate(authors = map(authorshipsExtract_, "authors_id_")) %>% unnest_longer(authors)
  edgelistList[[i]] <- authorBibliogWCoAuthors
  print(i)
  Sys.sleep(.1)
}

bibliogs <- edgelistList




