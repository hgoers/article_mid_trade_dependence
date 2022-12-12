library(tidyverse)
library(wbstats)
library(countrycode)
library(peacesciencer)
library(jsonlite)

# Scope -------------------------------------------------------------------

scope <- create_dyadyears(system = "cow", mry = F, directed = T) |> 
  filter(year == 2014, !ccode1 %in% c(347, 713)) |> 
  mutate(
    country1 = countrycode(ccode1, "cown", "country.name"),
    country2 = countrycode(ccode2, "cown", "country.name")
  )

# MID ---------------------------------------------------------------------
# Note, both sides can initiiate the war. 
mid_df <- scope |> 
  add_cow_mids(keep = c("dispnum", "sidea1", "sidea2", "fatality", "orig1", "orig2")) |> 
  transmute(
    ccode1,
    country1,
    ccode2,
    country2,
    year,
    mid_onset_all = if_else(cowmidonset == 1 & orig1 == 1, 1, 0)
  ) |> 
  mutate(mid_onset_all = factor(mid_onset_all))

# Trade dependence --------------------------------------------------------

trade_raw <- rio::import(here::here("data-raw", "trade_data_2014.csv")) |> 
  janitor::clean_names() |> 
  transmute(
    year = yr, 
    country1 = countrycode(rt_title, "country.name", "country.name"), 
    country2 = countrycode(pt_title, "country.name", "country.name",
                           custom_match = c("World" = "World")), 
    rg_desc, 
    cmd_code, 
    cmd_desc_e, 
    trade_value
  ) |> 
  filter(rg_desc %in% c("Export", "Import")) |> 
  as_tibble()

trade_bilat <- trade_raw |> 
  filter(country2 != "World")

gdp_df <- wbstats::wb_data("NY.GDP.MKTP.CD", start_date = 2014, end_date = 2014, return_wide = F) |> 
  transmute(
    country1 = countrycode(iso3c, "iso3c", "country.name"), 
    year = date, 
    gdp = value
  )

commodities <- fromJSON("https://comtrade.un.org/Data/cache/classificationHS.json")$results |> 
  filter(parent == "TOTAL") |> 
  transmute(
    cmd_code = as.numeric(id),
    cmd_desc_e = str_sub(text, 6)
  )

trade_dep <- trade_bilat |> 
  group_by(year, country1, country2, cmd_code) |> 
  summarise(trade_value_total = sum(trade_value, na.rm = T)) |> 
  left_join(commodities, by = "cmd_code") |> 
  left_join(gdp_df, by = c("year", "country1")) |> 
  mutate(trade_dep_total = (trade_value_total / gdp)) |> 
  group_by(country1, country2, year) |> 
  slice_max(trade_dep_total, with_ties = F) |> 
  ungroup() |> 
  select(year:cmd_code, cmd_desc_e, trade_dep_total)

trade_dep_broad <- trade_bilat |> 
  group_by(year, country1, country2) |> 
  summarise(total_trade_value = sum(trade_value)) |> 
  left_join(gdp_df, by = c("year", "country1")) |> 
  transmute(
    country1, 
    country2,
    year,
    trade_dep_broad = (total_trade_value / gdp)
  ) |> 
  ungroup()

# Get controls ------------------------------------------------------------

controls <- scope |> 
  add_capital_distance() |> 
  add_contiguity() |> 
  add_democracy() |> 
  add_nmc() |> 
  mutate(
    conttype = if_else(conttype == 1, 1, 0),
    conttype = factor(conttype),
    v2x_polyarchy = abs(v2x_polyarchy1 - v2x_polyarchy2)
  )

# Create full dataset -----------------------------------------------------
# TODO: Work out what are true NAs and what are 0s. 
full_df <- mid_df |> 
  left_join(trade_dep, by = c("country1", "country2", "year")) |> 
  left_join(trade_dep_broad, by = c("country1", "country2", "year")) |>
  left_join(controls, by = c("ccode1", "country1", "ccode2", "country2", "year")) |> 
  mutate(across(trade_dep_total:trade_dep_broad, ~ replace_na(.x, 0)))

rio::export(full_df, here::here("data", "full_df.csv"))


