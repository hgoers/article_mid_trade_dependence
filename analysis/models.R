library(tidyverse)
library(broom)
library(gtsummary)
library(labelled)

# Load in full dataset ----------------------------------------------------

perc_df <- rio::import(here::here("data", "full_df.csv")) |> 
  mutate(across(trade_dep_total:trade_dep_broad, ~ .x * 100)) |> 
  set_variable_labels(trade_dep_total = "Trade dependence, product-level",
                      trade_dep_broad = "Trade dependence, total trade", 
                      conttype = "Contiguous",
                      v2x_polyarchy1 = "Democracy, state i",
                      v2x_polyarchy2 = "Democracy, state j")

# Basic models ------------------------------------------------------------

m1 <- glm(
  mid_onset_all ~ trade_dep_total, 
  family = binomial(link = "logit"), 
  data = perc_df
)

t1 <- tbl_regression(m1, exponentiate = T, intercept = T)

m2 <- glm(
  mid_onset_all ~ trade_dep_broad, 
  family = binomial(link = "logit"), 
  data = perc_df
)

t2 <- tbl_regression(m2, exponentiate = T, intercept = T)

pred_m1_m2 <- augment(
  m1, newdata = tibble(trade_dep_total = seq(0, 100, 1)), type.predict = "response"
) |> 
  mutate(type = "Product-level") |> 
  rename(trade_dep = trade_dep_total) |> 
  bind_rows(
    augment(
      m2, newdata = tibble(trade_dep_broad = seq(0, 100, 1)), type.predict = "response"
    ) |>
      mutate(type = "Total trade") |> 
      rename(trade_dep = trade_dep_broad)
  )

# Controlled models -------------------------------------------------------

m3 <- glm(
  mid_onset_all ~ trade_dep_total + conttype + v2x_polyarchy1 + v2x_polyarchy2 , 
  family = binomial(link = "logit"), 
  data = perc_df
)

t3 <- tbl_regression(m3, exponentiate = T, intercept = T)

m4 <- glm(
  mid_onset_all ~ trade_dep_broad + conttype + v2x_polyarchy1 + v2x_polyarchy2, 
  family = binomial(link = "logit"), 
  data = perc_df
)

t4 <- tbl_regression(m4, exponentiate = T, intercept = T)
