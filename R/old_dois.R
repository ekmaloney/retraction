library(readxl)
library(here)
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

retraction_data <- read_xlsx(here("data/retraction_db.xlsx"))
retraction_data <- clean_names(retraction_data)

psych_papers <- retraction_data %>% 
                mutate(psych = if_else(str_detect(subject, "Psychology"), 1, 0)) %>% 
                filter(psych == 1)

psych_dois <- psych_papers %>% 
              select(original_paper_doi) %>% 
              filter(original_paper_doi != "Unavailable") %>% 
              filter(original_paper_doi != "unavailable")

psych_dois <- psych_papers %>% 
  select(retraction_doi) %>% 
  filter(retraction_doi != "Unavailable") %>% 
  filter(retraction_doi != "unavailable") %>% 
  mutate(doi = paste0("'", retraction_doi, "'")) %>% 
  select(doi)

no_doi <- anti_join(psych_papers, psych_dois)

write_csv(psych_dois, file = here("data/psych_dois.csv"))
