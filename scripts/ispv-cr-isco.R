library(ispv)
library(dplyr)
library(stringr)
library(purrr)
library(curl)
library(readr)

pv_isco_cr |>
  filter(!str_detect(isco_id, "^3355|^23")) |>
  group_by(sfera) |>
  mutate(fte_share = fte_thous/sum(fte_thous, na.rm = T)) |>
  slice_max(fte_share, n = 10) |> View()

pv_isco_cr |>
  filter(str_detect(isco_full, "pecialis"),
         isco_digits == 4) |> View()
# https://apl.czso.cz/iSMS/klasdata.jsp?kodcis=80121
# https://apl.czso.cz/iSMS/cisexp.jsp?kodcis=80121&typdat=0&cisvaz=5705&cisjaz=203&format=2&separator=%2C

isco_klas <- read_csv("data-input/czso/KLAS80121_CS.csv", locale = locale(encoding = "Windows-1250"))
