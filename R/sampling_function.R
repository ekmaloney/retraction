#' Get Paper Results
#'
#' @param paper_id the openalex paper id matching paper to find a sampled match from
#' @param pub_date the original publication date of the paper
#' @param url the url of the api request
#' @param ieee_name name of IEEE conference else NA
#' @param num_pages number of pages of results from the query 
#'
#' @return
#' @export
#'
#' @examples
get_paper_results <- function(paper_id, 
                              pub_date, 
                              url,
                              ieee_name,
                              num_pages){
  
  
  if(!is.na(ieee_name)){
    
    q_final <- ieee_paper_search(paper_id, 
                                 pub_date, 
                                 url,
                                 ieee_name,
                                 num_pages)
    
    
    if(nrow(q_final) == 0){
      
      q_final <- ieee_oa_date_paper_search(paper_id, 
                                   pub_date, 
                                   url,
                                   ieee_name,
                                   num_pages)
    }
    
    
  }else if(num_pages == 0){
    q_final <- non_ieee_paper_search_no_pages(paper_id, 
                                              pub_date, 
                                              url,
                                              ieee_name,
                                              num_pages)
    
  }else{
    q_final <- non_ieee_paper_search(paper_id, 
                                     pub_date, 
                                     url,
                                     ieee_name,
                                     num_pages)
    
    if(nrow(q_final) == 0){
      
      q_final <- non_ieee_paper_search_no_results(paper_id, 
                                                  pub_date, 
                                                  url,
                                                  ieee_name,
                                                  num_pages)
    }
    
  }
  
  selected <- q_final[sample(nrow(q_final), 1), ]
  
  return(selected)
  
}




#' IEEE Paper Search
#'
#' @param paper_id 
#' @param pub_date 
#' @param url the url of the api call 
#' @param ieee_name name of the IEEE conference
#' @param num_pages number of pages of results from the API call
#'
#' @return
#' @export
#'
#' @examples
ieee_paper_search <- function(paper_id, 
                              pub_date, 
                              url,
                              ieee_name,
                              num_pages){
  
  if(num_pages == 0){
    new_url <- str_replace_all(url, "publication_date", "from_publication_date")
    new_url <- paste0(new_url, "&sort=publication_date")
    q <- openalexR::openalex_api(new_url)
    q <- q$results
  }else{
    
    #take a random page
    random_page <- sample(1:num_pages, 1)
    new_url <- paste0(url, "&page=", random_page)
    q <- openalexR::openalex_api(new_url)
    q <- q$results
  }
  
  #q <- q$results
  
  q_final <- q %>% 
    filter(host_venue$display_name == ieee_name) %>% 
    filter(id != paper_id) %>% filter(is_retracted == FALSE) %>% 
    filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract"))
  
  return(q_final)
}


ieee_oa_date_paper_search <- function(paper_id, 
                                      pub_date, 
                                      url,
                                      ieee_name,
                                      num_pages){
  interim <- openalexR::find_work(id_type = "openalex", id = paper_id)
  new_url <- paste0("https://api.openalex.org/works?filter=host_venue.publisher:ieee,publication_date:",
                    interim$publication_date)
  num_pages <- ceiling(openalexR::openalex_api(new_url)$meta$count/25)
  
  random_page <- sample(1:num_pages, 1)
  new_url <- paste0(new_url, "&page=", random_page)
  q <- openalexR::openalex_api(new_url)
  q <- q$results
  
  q_final <- q %>% 
    filter(host_venue$display_name == ieee_name) %>% 
    filter(id != paper_id) %>% filter(is_retracted == FALSE) %>% 
    filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract"))
  
  return(q_final)
}


non_ieee_paper_search_no_pages <- function(paper_id, 
                                           pub_date, 
                                           url,
                                           ieee_name,
                                           num_pages){
  new_url <- str_replace_all(url, "publication_date", "from_publication_date")
  new_url <- paste0(new_url, "&sort=publication_date")
  
  new_q <- openalexR::openalex_api(new_url)
  new_q <- new_q$results
  
  q_final <- new_q %>% filter(id != paper_id) %>% 
    filter(is_retracted == FALSE) %>% 
    filter(publication_date == min(publication_date)) %>% 
    filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract"))
  
  return(q_final)
}


non_ieee_paper_search <- function(paper_id, 
                                  pub_date, 
                                  url,
                                  ieee_name,
                                  num_pages){
  new_url <- str_replace_all(url, "publication_date", "from_publication_date")
  new_url <- paste0(new_url, "&sort=publication_date")
  
  new_q <- openalexR::openalex_api(new_url)
  new_q <- new_q$results
  
  q_final <- new_q %>% filter(id != paper_id) %>% 
    filter(is_retracted == FALSE)  %>% 
    filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract")) %>% 
    filter(publication_date == min(publication_date))
  
  return(q_final)
}


non_ieee_paper_search_no_results <- function(paper_id, 
                                             pub_date, 
                                             url,
                                             ieee_name,
                                             num_pages){
  new_url <- str_replace_all(url, "publication_date", "from_publication_date")
  new_url <- paste0(new_url, "&sort=publication_date")
  
  new_q <- openalexR::openalex_api(new_url)
  new_q <- new_q$results
  
  q_final <- new_q %>% filter(id != paper_id) %>% 
    filter(is_retracted == FALSE)  %>% 
    filter(!str_detect(display_name, "Retraction|retract|Retracted|retracted|Retract")) %>% 
    filter(publication_date == min(publication_date))
}