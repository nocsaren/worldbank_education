library(tidyverse)

search_id <- function(param_1, param_2 = NULL, param_3 = NULL) {
  load("id_list.rda")
  
  search_result <- id_list %>%
    filter(
      str_detect(id_name, regex(param_1, ignore_case = TRUE)) &
        if (!is.null(param_2)) str_detect(id_name, regex(param_2, ignore_case = TRUE)) else TRUE &
        if (!is.null(param_3)) str_detect(id_name, regex(param_3, ignore_case = TRUE)) else TRUE
    ) %>%
    select(id_code, id_name, id_about) %>%
    as_tibble()
  
  assign("search_result", search_result, envir = .GlobalEnv)
  
  return(search_result)
}