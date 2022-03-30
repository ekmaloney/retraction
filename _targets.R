# _targets.R file
library(targets)

#source functions
source("R/functions.R")
source("R/retraction_watch_cleaning.R")
source("R/non_retract_functions.R")
source("R/network_functions.R")
options(tidyverse.quiet = TRUE)

#set packages needed
tar_option_set(packages = c("openalexR", "tidyverse", "lubridate", "stringr"))


#all of the steps needed to run the analysis
list(
  #first, reading in the raw data file from retraction watch
  tar_target(rw_data_raw_file, readxl::read_xlsx("data/retraction_db.xlsx")),

  #second, make the subject column a bit cleaner 
  tar_target(rw_data_cleaner, fix_subject_column(rw_data_raw_file)),
  
  #three: get the DOIS to request from Open Alex API
  tar_target(rw_dois, select_doi_to_request(rw_data_cleaner)),
  
  #four: Identify the dois that openalex does not have a record for 
  tar_target(bad_record_ids, identify_issue_dois(rw_dois)),
  
  #five: write out the dois file to try and get them by hand
  tar_target(write_out_record_ids, rw_dois %>% filter(record_id %in% bad_record_ids) %>% 
               write_csv(file = "data/fix_dois.csv")),
  
  #six: fixed dois :) 
  tar_target(doi_added, read_csv("data/fix_dois_added.csv")),
  
  #seven: final full list of dois to request 
  tar_target(final_dois, rw_dois %>% 
                          filter(!(record_id %in% bad_record_ids)) %>% 
                          mutate(record_id = as.numeric(record_id)) %>% 
                                 bind_rows(doi_added) %>% 
                          filter(!is.na(doi_for_requesting))),
  
  #eight: get the paper information
  tar_target(paper_info, get_paper_info(final_dois)),
  
  #nine: find papers with no original publication date
  tar_target(no_pub_date, identify_no_og_date(paper_info, 
                                               final_dois)),
  
  #ten: write out for 
  tar_target(code_pub_date, write_csv(no_pub_date, file = "data/date_tagging.csv")),
  
  #eleven: read in final dates
  tar_target(code_final_date, read_csv("data/date_tagging_final.csv")),
  
  #twelve: get the sampled papers :) 
  # tar_target(sampled_papers_df, 
  #            join_all_data_together(paper_info_df = paper_info,
  #                                   no_date_info_df = code_final_date))
  
  tar_target(sampled_papers_df, readRDS("data/sampled_papers.RDS")),
  
  #thirteen: get all authors info
  tar_target(all_authors, combine_retracted_sampled(paper_info, sampled_papers_df)),
  
  #fourteen: get network info
  tar_target(network_info, map_df(all_authors$authors_id, get_network_covariates))
  
  #tar_target(network_info, map_df(sampled_papers_df$, get_network_covariates))
  
  
  #nine: get all of the authors papers & the coauthor info on those as well
  # tar_target(all_authors_papers, paper_info$author_oa_id %>% 
  #                                map_df(getting_papers_coauthors)),
  
  #ten: get all coauthors coauthors 
  # tar_target(all_coauthors_papers, all_authors_papers$author_oa_id %>% 
  #                                   map_df(poss_coauthors))

)

