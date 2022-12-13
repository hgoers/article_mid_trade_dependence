library(tidyverse)
library(countrycode)
library(httr)
library(jsonlite)

# Set file path
year <- 2013

# Read in existing trade data
trade_df <- rio::import(here::here("data-raw", glue::glue("trade_data_{ year }.csv")))

# Attempt direction-based pull --------------------------------------------

# Find countries for which yearly trade data is too big
pull_scope <- tibble(direction = c(1, 2)) |> 
  full_join(
    trade_df |> 
      filter(status == "Result too large") |> 
      select(rtCode),
    by = character()
  ) |> 
  mutate(across(everything(), ~ as.character(.x)))

pull_large_comtrade <- function(reporter_id, direction) {
  
  query <- glue::glue("https://comtrade.un.org/api/get?r={ reporter_id }&ps={ year }&rg={ direction }&fmt=json&freq=A&head=M&px=HS&cc=AG2")
  
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
      tibble(result$dataset) |> 
        mutate(across(everything(), ~ as.character(.x)))
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
        rtCode = reporter_id,
        rgCode = direction,
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
large_annual_trade_df <- trade_df |> 
  filter(status != "Result too large") |> 
  mutate(across(everything(), ~ as.character(.x))) |> 
  bind_rows(
    large_trade_df$result
  )

# Attempt grouped commodity pull ------------------------------------------

pull_scope <- large_annual_trade_df |> 
  filter(str_detect(status, "Results too large")) |> 
  select(rgCode, rtCode) |> 
  left_join(
    trade_df |> 
      distinct(cmdCode),
    by = character()
  ) |> 
  drop_na() |> 
  mutate(group = if_else(row_number() %% 3 == 0, row_number(), NA_integer_)) |> 
  fill(group, .direction = c("downup")) |> 
  group_by(rgCode, rtCode, group) |> 
  summarise(cmdCode = paste(cmdCode, collapse = ",")) |> 
  ungroup()
  
pull_very_large_comtrade <- function(df) {
  
  reporter_id <- df$rtCode
  cmd_code <- df$cmdCode
  direction <- df$rgCode
  
  query <- glue::glue("https://comtrade.un.org/api/get?r={ reporter_id }&ps={ year }&rg={ direction }&cc={ cmd_code }&fmt=json&freq=A&head=M&px=HS&cc=AG2")
  
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
      tibble(result$dataset) |> 
        mutate(across(everything(), ~ as.character(.x)))
    )
    
  } else if (result$validation$status$name != "Result too large" & length(result$dataset) == 0) {
    
    return(
      tibble(
        rtCode = reporter_id,
        status = glue::glue("No results found: {cmd_code} and {direction}")
      )
    )
    
  } else {
    
    return(
      tibble(
        rtCode = reporter_id,
        status = glue::glue("Result too large: {cmd_code} and {direction}")
      )
    )
    
  }
  
}

safely_pull_very_large_comtrade <- safely(~ pull_very_large_comtrade(.x))

very_large_trade_df <- map_dfr(1:nrow(pull_scope), ~ safely_pull_very_large_comtrade(slice(pull_scope, .x)))

very_large_annual_trade_df <- large_annual_trade_df |> 
  filter(status != "Result too large") |> 
  bind_rows(
    very_large_trade_df$result
  )

rio::export(very_large_annual_trade_df, here::here("data-raw", "complete_trade_data_2013.csv"))

