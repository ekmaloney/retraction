get_coauthors_network <- function(id){
  
  a_id <- str_remove_all(id, "https://openalex.org/")
  
  #get coauthors
  coauthors <- openalexR::get_coauthors(id_type = "openalex",
                                        id = a_id)
  #get coauthors coauthors
  coauthors_coauthors <- unique(coauthors$alter_author) %>% 
                         map2_df("openalex", ., get_coauthors)
    
  if(nrow(coauthors) == 0){
    network <- NULL
  }else{
    coauthors_coauthors <- coauthors_coauthors %>% 
      filter(alter_author %in% coauthors$alter_author)
    
    #get the ego network
    ego_net <- bind_rows(coauthors, coauthors_coauthors) %>% 
      filter(ego_author != str_remove(a_id, "https://openalex.org/") & 
               alter_author != str_remove(a_id, "https://openalex.org/")) %>% 
      dplyr::mutate(ego_first = paste(ego_author_name, alter_author_name, sep = "_"),
                    alter_first = paste(alter_author_name, alter_author_name, sep = "_")) %>% 
      tidyr::pivot_longer(ego_first:alter_first, 
                          names_to = "order", 
                          values_to = "tie") %>% 
      dplyr::select(paper_id, tie) %>% 
      dplyr::distinct() %>% separate(tie, into = c("ego_author",
                                                   "alter_author"), sep = "_") %>% 
      filter(ego_author != alter_author) %>% 
      group_by(ego_author, alter_author) %>% 
      summarise(weight = n())
    
    
    library(igraph)
    
    network <- graph_from_data_frame(ego_net, directed = FALSE)
  }
  
  
  return(network)
  
}


get_coauthors <- function(id_type = c("orcid", "openalex",
                                      "scopus", "mag"),
                          id){
  
  
  coauthorship_edgelist <- purrr::map2_df(id_type,
                                          id,
                                          inner_coauth_function)
  
  #ego_authors <- find_author(id_type, id)
  
  # ego_authors <- ego_authors %>%
  #                 dplyr::mutate(ego_author_name = name,
  #                               ego_author = stringr::str_remove(openalex_id, "https://openalex.org/")) %>%
  #                 dplyr::select(ego_author_name, ego_author)
  # 
  # coauthorship_edgelist <- dplyr::left_join(coauthorship_edgelist, ego_authors)
  
  
  
  return(coauthorship_edgelist)
  
  
}


inner_coauth_function <- function(id_type, id){
  #get all papers and unnest authorships
  papers <- get_authors_papers(id_type, id) %>%
            tidyr::unnest(authorships, names_sep = "_")
  
  #get ego name 
  ego_name <- papers$authorships_author$display_name[papers$authorships_author$id == paste0("https://openalex.org/", id)][1]
  
  
  #make edgelist
  papers_authors <- tibble::tibble(paper_id = papers$id,
                                   ego_author = stringr::str_remove(id, "https://openalex.org/"),
                                   alter_author = stringr::str_remove(papers$authorships_author$id,
                                                                      "https://openalex.org/"),
                                   alter_author_name = papers$authorships_author$display_name) %>%
    dplyr::filter(ego_author != alter_author) %>% 
    mutate(ego_author_name = ego_name)
  
  return(papers_authors)
}



get_network_covariates <- function(i){
  
  network = get_coauthors_network(id = i)
  
  if(is.null(network)){
    density <- NULL
    clust_coeff <- NULL
    number_components <- NULL
    size_components <- NULL
  }else {
    density <- igraph::edge_density(network)
    clust_coeff <- igraph::transitivity(network, type = "global")
    number_components <- igraph::components(network)$no
    size_components <- igraph::components(network)$csize 
  }
  
  net_info <- tibble::tibble(id = i,
                             density = density,
                             clust_coeff = clust_coeff,
                             number_components = number_components,
                             size_components = list(size_components))
  
  
  return(net_info)
}

network_info <- tibble()
for(i in all_authors$authors_id[83:3283]){
  net_cov <- get_network_covariates(i = i)
  
  network_info <- bind_rows(network_info, net_cov)
}
