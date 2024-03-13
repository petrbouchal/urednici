library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(purrr)
library(stringr)
library(ggplot2)

options(scipen = 100)

szu_excel_sheets <- readr::read_rds("data-interim/szu-excel-metadata.rds")

read_szu_excel <- function(path, sheet, year) {
  is_pocet <-
  read_excel(path, sheet, skip = 11) |>
    janitor::remove_empty(which = "rows") |>
    mutate(rok = year, list = sheet, file = basename(path))
}

szu_names <- c("kap_kod", "kap_zkr", "kap_name", "skut_min", "rozp_schv",
               "rozp_pzm", "skut", "plneni_proc", "plneni_rozdil", "skut_skut",
               "rok", "list", "file", "promenna")

szu_uo <- szu_excel_sheets |>
  mutate(file_hash = tools::md5sum(file)) |>
  filter(str_detect(sheets, "ÚO"), str_detect(sheets, "Tab1[10]|Str")) |>
  distinct(file_hash, sheets, rok, .keep_all = TRUE) |>
  select(path = file, sheet = sheets, year = rok) |>
  pmap_dfr(read_szu_excel) |>
  janitor::clean_names() |>
  group_by(rok, list, file) |>
  mutate(promenna = if_else((max(x1_2, na.rm = TRUE) > 1e5), "objemy", "pocty")) |>
  ungroup() |>
  rlang::set_names(szu_names) |>
  drop_na(kap_kod) |>
  mutate(skut_min = if_else(rok < 2013 & promenna == "objemy", skut_min * 1000, skut_min),
         rozp_schv = if_else(rok < 2013 & promenna == "objemy", rozp_schv * 1000, rozp_schv),
         rozp_pzm = if_else(rok < 2013 & promenna == "objemy", rozp_pzm * 1000, rozp_pzm),
         skut = if_else(rok < 2013 & promenna == "objemy", skut * 1000, skut),
         kap_name = if_else(is.na(kap_name), kap_zkr, kap_name)) |>
  rows_update(read_csv("data-input/kapitoly.csv"), by = "kap_kod") |>
  relocate(rok, .after = kap_name)

szu_uo |>
  count(rok, kap_kod, promenna) |>
  count(n)

szu_uo |>
  count(rok, wt = is.na(kap_name))

szu_uo |>
  group_by(rok, kap_kod, promenna) |>
  filter(n() > 1) |>
  arrange(kap_kod)

head(szu_uo)

szu_uo_2009 <- szu_uo |>
  filter(rok == 2010) |>
  mutate(rok = 2009,
         skut = skut_min,
         skut_min = NA,
         plneni_proc = NA,
         plneni_rozdil = NA,
         rozp_schv = NA, rozp_pzm = NA, skut_skut = NA)

source("scripts/inflation.R")
source("scripts/wages.R")

szu_uo_wide <- szu_uo |>
  bind_rows(szu_uo_2009) |>
  pivot_wider(id_cols = c(kap_kod, kap_name, rok, promenna, kap_zkr),
              values_from = c(matches("^skut|^rozp|^plneni")), names_from = promenna, names_glue = "{promenna}_{.value}") |>
  left_join(df_infl) |>
  left_join(wages_later)


szu_uo_wide |>
  mutate(plat = objemy_skut/pocty_skut/12) |>
  filter(plat != 0, kap_zkr != "NSA") |>
  ggplot(aes(rok, plat/1e3)) +
  geom_line() +
  geom_line(aes(y = phasal_all/1e3), color = "grey") +
  facet_wrap(~kap_zkr)

xszu_uo_wide |>
  filter(is.na(kap_name)) |> View()
  count(kap_kod)

szu_uo |>
  distinct(kap_kod, kap_zkr, kap_name) |>
  format_csv() |> cat()

dt_orig <- arrow::read_parquet("data-input/szu/data_all.parquet")

dt_orig |>
  count(faze_rozpoctu)
