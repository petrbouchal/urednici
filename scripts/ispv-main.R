library(ispv)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(forcats)

targets::tar_load(pv_isco_cr)
targets::tar_load(pv_isco_pg)
targets::tar_load(pv_edu_cr)
targets::tar_load(pv_edu_pg)
targets::tar_load(pv_genderage_cr)
targets::tar_load(pv_genderage_pg)

targets::tar_load(pv_isco_cr_long)
targets::tar_load(pv_isco_pg_long)

pv_ga_cr_long <- pv_genderage_cr |>
  mutate(grp = str_extract(category, "^[A-Ž]{4,6}") |> tolower(),
         category = str_remove(category, " - (mzdová|platová) sféra"),
         sfera = tolower(sfera) |>
           recode(mzs = "Mzdová sféra",
                  pls = "Platová sféra")) |>
  fill(grp) |>
  filter(grp == "celkem") |>
  pivot_longer(cols = c(pay_median, matches("pay_[dq]"))) |>
  select(category, name, value, sfera) |>
  mutate(category = as.factor(category) |> fct_relevel("do 20 let"),
         percentile = case_when(name == "pay_d1" ~ 10,
                                name == "pay_q1" ~ 25,
                                name == "pay_median" ~ 50,
                                name == "pay_q3" ~ 75,
                                name == "pay_d9" ~ 90
                                )) |>
  mutate(name = as.factor(name) |>
           fct_relevel("pay_d1", "pay_q1", "pay_median", "pay_q3", "pay_d9")) |>
  ggplot(aes(percentile, value, colour = sfera, group = sfera)) +
  scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
  scale_colour_manual(values = c(PLS = "darkblue", MZS = "darkgrey")) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  # scale_color_viridis_d() +
  facet_wrap(~ category)

pv_ga_pg_long <- pv_genderage_pg |>
  mutate(grp = str_extract(category, "^[A-Ž]{4,6}") |> tolower(),
         category = str_remove(category, " - (mzdová|platová) sféra"),
         sfera = tolower(sfera) |>
           recode(mzs = "Mzdová sféra",
                  pls = "Platová sféra")) |>
  fill(grp) |>
  filter(grp == "celkem") |>
  pivot_longer(cols = c(pay_median, matches("pay_[dq]"))) |>
  select(category, name, value, sfera) |>
  mutate(category = as.factor(category) |> fct_relevel("do 20 let"),
         percentile = case_when(name == "pay_d1" ~ 10,
                                name == "pay_q1" ~ 25,
                                name == "pay_median" ~ 50,
                                name == "pay_q3" ~ 75,
                                name == "pay_d9" ~ 90
                                )) |>
  mutate(name = as.factor(name) |>
           fct_relevel("pay_d1", "pay_q1", "pay_median", "pay_q3", "pay_d9"))

ggplot(pv_ga_pg_long, aes(percentile, value, colour = sfera, group = sfera)) +
  scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
  scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                 `Mzdová sféra` = "darkgrey")) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  # scale_color_viridis_d() +
  facet_wrap(~ category)

pv_edu_pg_long <- pv_edu_pg |>
  mutate(category = str_remove(category, " - (mzdová|platová) sféra"),
         sfera = tolower(sfera) |>
           recode(mzs = "Mzdová sféra",
                  pls = "Platová sféra")) |>
  filter(category == "Vysokoškolské") |>
  pivot_longer(cols = c(pay_median, matches("pay_[dq]"))) |>
  select(category, name, value, sfera) |>
  mutate(percentile = case_when(name == "pay_d1" ~ 10,
                                name == "pay_q1" ~ 25,
                                name == "pay_median" ~ 50,
                                name == "pay_q3" ~ 75,
                                name == "pay_d9" ~ 90
         )) |>
  mutate(name = as.factor(name) |>
           fct_relevel("pay_d1", "pay_q1", "pay_median", "pay_q3", "pay_d9"))

ggplot(pv_edu_pg_long,
       aes(percentile, value, colour = sfera, group = sfera)) +
  scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
  scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                 `Mzdová sféra` = "darkgrey")) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5)

pv_edu_cr_long <- pv_edu_cr |>
  mutate(category = str_remove(category, " - (mzdová|platová) sféra"),
         sfera = tolower(sfera) |>
           recode(mzs = "Mzdová sféra",
                  pls = "Platová sféra")) |>
  filter(category == "Vysokoškolské") |>
  pivot_longer(cols = c(pay_median, matches("pay_[dq]"))) |>
  select(category, name, value, sfera) |>
  mutate(percentile = case_when(name == "pay_d1" ~ 10,
                                name == "pay_q1" ~ 25,
                                name == "pay_median" ~ 50,
                                name == "pay_q3" ~ 75,
                                name == "pay_d9" ~ 90
         )) |>
  mutate(name = as.factor(name) |>
           fct_relevel("pay_d1", "pay_q1", "pay_median", "pay_q3", "pay_d9"))

ggplot(pv_edu_cr_long |> mutate(geo = "ČR") |>
         bind_rows(pv_edu_pg_long |> mutate(geo = "Praha")),
       aes(percentile, value, colour = sfera, group = sfera)) +
  scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
  scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                 `Mzdová sféra` = "darkgrey")) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  facet_wrap(~geo)


pv_isco_cr_long |> mutate(geo = "ČR") |>
  bind_rows(pv_isco_pg_long |> mutate(geo = "Praha")) |>
  filter(isco_digits == 4 | is.na(isco_digits)) |>
  filter(isco_id == "3313" | isco4_id == "2422") |>
  ggplot(aes(percentile, value, colour = sfera, group = sfera)) +
  scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
  scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                 `Mzdová sféra` = "darkgrey")) +
  geom_line(size = 1.5) +
  geom_point(size = 1.5) +
  facet_wrap(~geo)

plot_isco_percentiles <- function(pv_isco_cr_long,
                                  pv_isco_pg_long,
                                  isco_ids) {
  pv_isco_cr_long |> mutate(geo = "ČR") |>
    bind_rows(pv_isco_pg_long |>
                mutate(geo = "Praha") |>
                rename(isco_id = isco4_id,
                       isco_name = isco4_name,
                       isco_full = isco4_full)) |>
    filter(isco_digits == 4 | is.na(isco_digits)) |>
    filter(isco_id %in% isco_ids) |>
    ggplot(aes(percentile, value, colour = sfera, group = sfera)) +
    scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
    scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                   `Mzdová sféra` = "darkgrey")) +
    geom_line(size = 1.5) +
    geom_point(size = 1.5) +
    facet_wrap(. ~  isco_name + geo) +
    theme_ptrr(multiplot = T, gridlines = "both")
}

plot_isco_percentiles(pv_isco_cr_long,
                      pv_isco_pg_long,
                      c("4110", "3343", "2422",
                        "2412", "2619", "3342",
                        "3511", "2411", "2431"))

plot_isco_percentiles(pv_isco_cr_long,
                      pv_isco_pg_long,
                      c("2512", "2514", "1330", "3511", "2529")) +
  labs(title = "Platy v platové a mzdové sféře podle kvantilů")
