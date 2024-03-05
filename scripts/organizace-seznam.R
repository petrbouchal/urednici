library(dplyr)
library(writexl)
library(statnipokladna)

orgs <- sp_get_codelist("ucjed", dest_dir = "data-input/statnipokladna")
druhuj <- sp_get_codelist("druhuj", dest_dir = "data-input/statnipokladna")
poddruhuj <- sp_get_codelist("poddruhuj", dest_dir = "data-input/statnipokladna")
forma <- sp_get_codelist("forma", dest_dir = "data-input/statnipokladna")

orgs_full_current <- orgs |>
  filter(end_date >= Sys.Date(), start_date <= Sys.Date()) |>
  left_join(druhuj, by = join_by(druhuj_id)) |>
  left_join(poddruhuj, by = join_by(poddruhuj_id)) |>
  left_join(forma, by = join_by(forma_id)) |>
  relocate(ico, nazev, druhuj_nazev, poddruhuj_nazev, forma_nazev)

orgs_full_current |>
  count(druhuj_nazev, poddruhuj_nazev) |> View()

orgs_full_current |>
  count(druhuj_nazev, forma_nazev) |> View()

orgs_full_current |>
  count(wt = n_distinct(ico))

write_xlsx(orgs_full_current, "data-export/organizace-dle-statnipokladny.xlsx")
