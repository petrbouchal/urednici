source("_targets_packages.R")
conflicted::conflict_prefer("filter", "dplyr")

targets::tar_load(wbi_wagepremiums)



wbi_wagepremiums |>
  drop_na(value) |>
  group_by(iso3c, indicator_lbl) |>
  slice_max(date, n = 3) |>
  summarise(value = mean(value)) |>
  group_by(indicator_lbl) |>
  mutate(rank = rank(value)) |>
  filter(iso3c == "CZE")

