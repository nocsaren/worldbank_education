library(tidyverse)
library(httr)
library(jsonlite)

id_list <- 
  GET("https://api.worldbank.org/v2/indicator?format=json&per_page=21020") %>%
  content(as = "text") %>%
  fromJSON() %>%
  .[[2]] %>%
  select(id, name, sourceNote) %>%
  rename(id_code = id, id_name = name, id_about = sourceNote) %>% 
  select(id_code, id_name, id_about) %>%
  as_tibble()

save(id_list, 
     file = "id_list.rda")