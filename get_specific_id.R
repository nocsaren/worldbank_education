library(tidyverse)
library(httr)
library(jsonlite)

get_specific_id <- function(country_1_code, indicator_code, number_values = 20000) {
  url <- paste0("https://api.worldbank.org/v2/country/", country_1_code, "/indicator/", indicator_code, "?format=json&per_page=", number_values)
  
  result <- GET(url)
  
  if (http_type(result) == "application/json") {
    data <- content(result, as = "text") %>%
      fromJSON() %>%
      .[[2]] %>%
      na.omit(value) %>%
      as_tibble()
    
    df_name_var <- paste0(indicator_code, "_", country_1_code)
    assign(df_name_var, data, envir = .GlobalEnv)
    saveRDS(data, file = paste0("./rds/", df_name_var, "_", Sys.Date(), ".rds"))
    
  } else {
    error_msg <- content(result, as = "text")
    cat("API Error:", error_msg, "\n")
    return(NULL)
  }
}