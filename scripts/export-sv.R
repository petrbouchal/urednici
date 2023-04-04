library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

targets::tar_load(isp)
isp |>
  select(nazev_ovm, plat_prumer) |>
  write_xlsx("data-export/isp-2021-pro-sv.xlsx")
