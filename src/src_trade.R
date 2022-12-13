library(tidyverse)
library(countrycode)
library(httr)
library(jsonlite)

reporter_ids <- fromJSON("https://comtrade.un.org/Data/cache/reporterAreas.json")$results |> 
  mutate(ccode = countrycode(text, "country.name", "cown")) |> 
  drop_na(ccode) |> 
  filter(!text %in% c("East and West Pakistan",
                      "Fmr Dem. Rep. of Germany",
                      "Fmr Fed. Rep. of Germany",
                      "India, excl. Sikkim",
                      "Peninsula Malaysia",
                      "Fmr Panama-Canal-Zone",
                      "Fmr Panama, excl.Canal Zone",
                      "Fmr USSR",
                      "Saint Kitts, Nevis and Anguilla",
                      "Fmr Sudan",
                      "US Virgin Isds",
                      "USA (before 1981)",
                      "Fmr Dem. Rep. of Vietnam",
                      "Fmr Rep. of Vietnam",
                      "Fmr Yugoslavia",
                      "Fmr Rhodesia Nyas",
                      "Fmr Ethiopia")) |> 
  pull(id)

pull_comtrade <- function(reporter_id, year) {
  
  query <- glue::glue("https://comtrade.un.org/api/get?r={ reporter_id }&ps={ year }&fmt=json&freq=A&head=M&px=HS&cc=AG2")
  
  result <- tryCatch(
    fromJSON(query),
    error = function(err) {
      
      print(glue::glue("Limit reached, wait one hour from { lubridate::now() }"))
      Sys.sleep(3600)
      return(fromJSON(query))
      
    }
  )
  
  print(glue::glue("Completing { reporter_id } in { year }"))
  print(result$validation$status$name)
  
  if (result$validation$status$name != "Result too large" & length(result$dataset) > 0) {
    
    return(
      tibble(result$dataset) |> 
        mutate(across(everything(), ~ as.character(.x)))
    )
    
  } else if (result$validation$status$name != "Result too large" & length(result$dataset) == 0) {
    
    return(
      tibble(
        rtCode = reporter_id,
        status = "No results found"
      )
    )
    
  } else {
    
    return(
      tibble(
        rtCode = reporter_id,
        status = "Result too large"
      )
    )
    
  }
  
}

safely_pull_comtrade <- safely(~ pull_comtrade(.x, .y))

trade_df <- map2_dfr(reporter_ids, 2013, ~ pull_comtrade(.x, .y))
 