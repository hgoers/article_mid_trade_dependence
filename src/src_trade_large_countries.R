library(tidyverse)
library(countrycode)
library(httr)
library(jsonlite)

# Read in existing trade data
annual_trade_df <- rio::import(here::here("data-raw", "trade_data_2014.csv"))

# Attempt direction-based pull --------------------------------------------

# Find countries for which yearly trade data is too big
pull_scope <- tibble(direction = c(1, 2)) |> 
  full_join(
    rio::import(here::here("data-raw", "trade_data_2014.csv")) |> 
      filter(status == "Result too large") |> 
      select(rtCode),
    by = character()
  )

pull_large_comtrade <- function(reporter_id, direction) {
  
  query <- glue::glue("https://comtrade.un.org/api/get?r={ reporter_id }&ps=2014&rg={ direction }&fmt=json&freq=A&head=M&px=HS&cc=AG2")
  
  result <- tryCatch(
    fromJSON(query),
    error = function(err) {
      
      print(glue::glue("Limit reached, wait one hour from { lubridate::now() }"))
      Sys.sleep(3600)
      return(fromJSON(query))
      
    }
  )
  
  print(glue::glue("Completing { reporter_id } for { direction }"))
  print(result$validation$status$name)
  
  if (result$validation$status$name != "Result too large" & length(result$dataset) > 0) {
    
    return(
      tibble(result$dataset)
    )
    
  } else if (result$validation$status$name != "Result too large" & length(result$dataset) == 0) {
    
    return(
      tibble(
        rtCode = reporter_id,
        status = glue::glue("No results found: { direction }")
      )
    )
    
  } else {
    
    return(
      tibble(
        rtCode = as.integer(reporter_id),
        rgCode = as.integer(direction),
        status = glue::glue("Results too large: { direction }")
      )
    )
    
  }
  
}

safely_pull_large_comtrade <- safely(~ pull_large_comtrade(.x, .y))

large_trade_df <- map2_dfr(
  pull_scope$rtCode, 
  pull_scope$direction, 
  ~ safely_pull_large_comtrade(.x, .y)
)

# Update results
large_annual_trade_df <- annual_trade_df |> 
  as_tibble() |> 
  filter(status != "Result too large") |> 
  bind_rows(
    large_trade_df$result |> 
      mutate(periodDesc = as.integer(periodDesc),
             cmdCode = as.integer(cmdCode))
  )

# Attempt grouped commodity pull ------------------------------------------

pull_scope <- tibble(rtCode = c(124, 251)) |> 
  left_join(
    trade_df$result |> 
      distinct(cmdCode),
    by = character()
  ) |> 
  drop_na() |> 
  mutate(group = if_else(row_number() %% 3 == 0, row_number(), NA_integer_)) |> 
  fill(group, .direction = c("downup")) |> 
  group_by(rtCode, group) |> 
  summarise(cmdCode = paste(cmdCode, collapse = ",")) |> 
  ungroup()
  
pull_very_large_comtrade <- function(reporter_id, cmd_code) {
  
  query <- glue::glue("https://comtrade.un.org/api/get?r={ reporter_id }&ps=2014&cc={ cmd_code }&fmt=json&freq=A&head=M&px=HS&cc=AG2")
  
  result <- tryCatch(
    fromJSON(query),
    error = function(err) {
      
      print(glue::glue("Limit reached, wait one hour from { lubridate::now() }"))
      Sys.sleep(3600)
      return(fromJSON(query))
      
    }
  )
  
  print(glue::glue("Completing { reporter_id } for { cmd_code }"))
  print(result$validation$status$name)
  
  if (result$validation$status$name != "Result too large" & length(result$dataset) > 0) {
    
    return(
      tibble(result$dataset)
    )
    
  } else if (result$validation$status$name != "Result too large" & length(result$dataset) == 0) {
    
    return(
      tibble(
        rtCode = reporter_id,
        status = glue::glue("No results found: {cmd_code}")
      )
    )
    
  } else {
    
    return(
      tibble(
        rtCode = as.integer(reporter_id),
        status = glue::glue("Result too large: {cmd_code}")
      )
    )
    
  }
  
}

safely_pull_very_large_comtrade <- safely(~ pull_very_large_comtrade(.x, .y))

very_large_trade_df <- map2_dfr(pull_scope$rtCode, pull_scope$cmdCode, ~ safely_pull_very_large_comtrade(.x, .y))

very_large_annual_trade_df <- large_annual_trade_df |> 
  filter(status != "Result too large") |> 
  bind_rows(
    very_large_trade_df$result |> 
      mutate(periodDesc = as.integer(periodDesc),
             cmdCode = as.integer(cmdCode))
  )

rio::export(very_large_annual_trade_df, here::here("data-raw", "trade_data_2014.csv"))

