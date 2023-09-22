library(tidyverse)
library(httr)
library(jsonlite)

country_list <- 
  GET("http://api.worldbank.org/v2/country?format=json&per_page=300") %>%
  content(as = "text") %>%
  fromJSON() %>%
  .[[2]] %>%
  rename(country_code = id, country_name = name) %>%
  select(country_code, country_name) %>%
  as_tibble()

save(country_list, 
     file = "country_list.rda")