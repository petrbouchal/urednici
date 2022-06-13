library(dplyr)
library(tidyr)
library(wbstats) # remotes::install_github("gshs-ornl/wbstats")
library(ggplot2)

wp_all <- wbstats::wb_data(indicator = c("BI.WAG.PREM.PB.GP", 
                                         "BI.WAG.PREM.PB.PN",
                                         "BI.WAG.PREM.PB.SG",
                                         "BI.WAG.PREM.PB.CK",
                                         "BI.WAG.PREM.PB.TT",
                                         "BI.WAG.PREM.PB.SN"),
                           return_wide = FALSE)

wp_all |> 
  filter(iso3c %in% c("CZE", "GBR", "SVK", "FIN", "DEN", "AUT", "POL", "DEU")) |> 
  ggplot(aes(date, value, colour = iso3c, group = paste(indicator, iso3c))) +
  geom_line(aes(size = if_else(iso3c == "CZE", 1, .6))) +
  scale_size_continuous(range = c(1, 2)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ indicator)

wp_all |> 
  filter(iso2c %in% c("CZ", "GB", "SK", "FI", "DK", "AT", "PL", "DE", "FR", "SE")) |> 
  drop_na(value) |> 
  group_by(iso3c, indicator) |> 
  slice_max(date, n = 3) |> 
  summarise(value = mean(value)) |>
  ggplot(aes(value, iso3c)) +
  geom_col() +
  geom_vline(xintercept = 0) +
  facet_wrap(~ indicator)

wp_all |> 
  drop_na(value) |> 
  group_by(iso3c, indicator) |> 
  slice_max(date, n = 3) |> 
  summarise(value = mean(value)) |>
  group_by(indicator) |> 
  mutate(rank = rank(value)) |> 
  filter(iso3c == "CZE")
  
