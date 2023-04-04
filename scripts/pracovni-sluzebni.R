source("_targets_packages.R")

syst_pocty_long_uo |> count(kapitola_kod) |> View()

syst_pocty_long_uo |>
  filter(date == "2022-04-01", kapitola_kod != "Celkem") |>
  count(vztah, wt = pocet) |>
  # group_by(level) |>
  mutate(perc = n/sum(n))

