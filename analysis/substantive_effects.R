library(mvtnorm)
library(tidyverse)
library(broom)

# Substantive effects -----------------------------------------------------

result_0 <- augment(m3, newdata = mutate(perc_df, trade_dep_total = 0), type.predict = "response") 
result_0

result_avg <- augment(m3, newdata = mutate(perc_df, trade_dep_total = mean(full_df$trade_dep_total) * 100), type.predict = "response") 
result_avg

result_diff <- result_0 |> 
  bind_rows(result_avg) |> 
  group_by(trade_dep_total) |> 
  summarise(.fitted = mean(.fitted, na.rm = T)) |> 
  mutate(diff = .fitted - lag(.fitted))
result_diff

coefs <- tidy(m3) |> pull(estimate)
coefs

coefs_sim <- rmvnorm(n = 100, mean = coefs, sigma = vcov(m3)) |> 
  as_tibble() |> 
  set_names(tidy(m3) |> mutate(term = paste0(term, "_beta")) |> pull(term)) |> 
  mutate(sim_round = row_number())

new_data <- perc_df |> 
  mutate(trade_dep_total = 0) |> 
  bind_rows(
    perc_df |> 
      mutate(trade_dep_total = mean(full_df$trade_dep_total) * 100)
  ) |> 
  group_by(trade_dep_total) |> 
  mutate(id = row_number()) |> 
  select(id, mid_onset_all, trade_dep_total, conttype, v2x_polyarchy1, v2x_polyarchy2)

sim_data <- coefs_sim |> 
  mutate(sim_round = row_number()) |> 
  full_join(new_data, by = character()) |> 
  mutate(
    .fitted = `(Intercept)_beta` +
      trade_dep_total_beta * trade_dep_total +
      conttype_beta * conttype +
      v2x_polyarchy1_beta * v2x_polyarchy1 +
      v2x_polyarchy2_beta * v2x_polyarchy2,
    .fitted = plogis(.fitted)
  ) |> 
  arrange(sim_round, id, trade_dep_total)

result <- sim_data |>
  group_by(sim_round, id) |> 
  mutate(diff = (.fitted - lag(.fitted)) * 100) |> 
  drop_na(diff) |> 
  ungroup() |> 
  summarise(lower_bound = quantile(diff, 0.025),
            mean = quantile(diff, 0.5),
            upper_bound = quantile(diff, 0.975))

rio::export(result, here::here("data", "sim_results.csv"))
