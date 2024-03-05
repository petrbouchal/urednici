targets::tar_load(syst_pocty_long_uo)

library(dplyr)
library(tidyr)

syst_pocty_long_uo |>
  mutate(trida_relev = trida > 10) |>
  filter(date == max(date)) |>
  filter(ustredni_organ) |>
  count(ustredni_organ, kapitola_vladni, kapitola_vladni, trida_relev, level_nazev,
        wt = pocet)

syst_pocty_long_uo |>
  filter(ustredni_organ) |>
  filter(date == max(date)) |>
  mutate(trida_relev = trida > 10) |>
  count(kapitola_vladni, trida_relev, level, wt = pocet) |>
  pivot_wider(names_from = trida_relev, values_from = n)

syst_pocty_long_uo |>
  filter(ustredni_organ, !kapitola_vladni) |>
  filter(date == max(date)) |>
  mutate(trida_relev = trida > 10) |>
  count(trida_relev, level, wt = pocet) |>
  pivot_wider(names_from = trida_relev, values_from = n)

syst_pocty_long_uo |>
  filter(ustredni_organ, kapitola_vladni) |>
  filter(date == max(date)) |>
  mutate(trida_relev = trida > 10) |>
  count(trida_relev, level, wt = pocet) |>
  pivot_wider(names_from = trida_relev, values_from = n)
