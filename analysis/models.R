library(tidyverse)
library(broom)
library(gtsummary)

# Load in full dataset ----------------------------------------------------

full_df <- rio::import(here::here("data", "full_df.csv"))

# Basic models ------------------------------------------------------------

m1 <- glm(
  mid_onset_all ~ trade_dep_total, 
  family = binomial(link = "logit"), 
  data = full_df
)

t1 <- tbl_regression(m1, exponentiate = T)

m2 <- glm(
  mid_onset_all ~ trade_dep_broad, 
  family = binomial(link = "logit"), 
  data = full_df
)

t2 <- tbl_regression(m2, exponentiate = T)

tbl_merge(list(t1, t2), tab_spanner = c("Product level", "Broad level"))

# Controlled models -------------------------------------------------------

m3 <- glm(
  mid_onset_all ~ trade_dep_total + conttype + v2x_polyarchy1 + v2x_polyarchy2 + v2x_polyarchy, 
  family = binomial(link = "logit"), 
  data = full_df
)

t3 <- tbl_regression(m3, exponentiate = T)

m4 <- glm(
  mid_onset_all ~ trade_dep_broad + conttype + v2x_polyarchy1 + v2x_polyarchy2 + v2x_polyarchy, 
  family = binomial(link = "logit"), 
  data = full_df
)

t4 <- tbl_regression(m4, exponentiate = T)

tbl_merge(list(t3, t4), tab_spanner = c("Product level", "Broad level"))
