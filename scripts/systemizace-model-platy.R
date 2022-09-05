source("_targets_packages.R")
library(broom)
library(janitor)
library(modelr)
library(ggiraph)
library(forcats)

targets::tar_load(syst_pocty_long)
targets::tar_load(syst_all)
syst_all

urady_nazvy_exclude <- c("Český báňský úřad",
                         "Česká správa sociálního zabezpečení",
                         "Státní úřad inspekce práce",
                         "Generální finanční ředitelství")

tribble(~regex, ~label,
        "[Ffi]inanční\\s(úřad|ředitelství)", "FÚ - vše",
        "SSZ|správa sociálního zabezpečení", "ČSSZ - vše",
        "OIP|inspekce práce", "Inspekce práce - vše",
        "OBÚ|báňský úřad", "Báňský úřad - vše")

syst_all_fixed <- syst_all |>
  mutate(organizace_nazev_new =
           case_when(str_detect(organizace_nazev, "[Ffi]inanční\\s(úřad|ředitelství)") ~ "FÚ - vše",
                     str_detect(organizace_nazev, "SSZ|správa sociálního zabezpečení") ~ "ČSSZ - vše",
                     str_detect(organizace_nazev, "OIP|inspekce práce") ~ "Inspekce práce - vše",
                     str_detect(organizace_nazev, "OBÚ|báňský úřad") ~ "Báňský úřad - vše",
                     TRUE ~ organizace_nazev
           )
  ) |>
  group_by(date, rok, organizace_nazev_new, across(starts_with("kapitola")),
           vztah, kapitola_typ) |>
  summarise(across(matches("^pocet_|^platy_celkem|pozad"), sum, na.rm = TRUE), .groups = "drop") |>
  replace_na(list(platy_celkem = 0, pocet_celkem = 0)) |>
  mutate(plat_prumer = platy_celkem/pocet_celkem/12) |>
  rename(organizace_nazev = organizace_nazev_new) |>
  arrange(-plat_prumer)

syst_platy <- syst_all_fixed |>
  filter(date < "2022-04-01", kapitola_kod != "Celkem") |>
  select(date, rok, organizace_nazev, vztah, pocet_celkem, kapitola_typ,
         plat_prumer, pocet_ostat) |>
  mutate(pocet_predst = pocet_celkem - pocet_ostat,
         podil_predst = pocet_predst / pocet_celkem)

syst_platy_uo <- syst_all |>
  filter(date < "2022-04-01", kapitola_kod != "Celkem",
         !organizace_nazev %in% urady_nazvy_exclude) |>
  select(date, rok, organizace_nazev, vztah, pocet_celkem, kapitola_typ,
         plat_prumer, pocet_ostat, ustredni_organ) |>
  mutate(pocet_predst = pocet_celkem - pocet_ostat,
         podil_predst = pocet_predst / pocet_celkem)

syst_pocty_long <- lengthen_syst_data(syst_all_fixed)
syst_pocty_long_0 <- lengthen_syst_data(syst_all)

syst_tridy <- syst_pocty_long |>
  filter(date < "2022-04-01", kapitola_kod != "Celkem",
         !organizace_nazev %in% urady_nazvy_exclude) |>
  replace_na(list(pocet = 0)) |>
  mutate(trida = recode(trida, M = "0") |> as.numeric()) |>
  group_by(date, rok, organizace_nazev, vztah, level, across(starts_with("kapitola"))) |>
  summarise(trida_mean = weighted.mean(trida, pocet, na.rm = TRUE), .groups = "drop")|>
  pivot_wider(names_from = level, values_from = trida_mean, names_prefix = "trida_")

syst_tridy_uo <- syst_pocty_long_0 |>
  filter(date < "2022-04-01", kapitola_kod != "Celkem",
         !organizace_nazev %in% urady_nazvy_exclude) |>
  replace_na(list(pocet = 0)) |>
  mutate(trida = recode(trida, M = "0") |> as.numeric()) |>
  group_by(date, rok, organizace_nazev, vztah, level, across(starts_with("kapitola")), ustredni_organ) |>
  summarise(trida_mean = weighted.mean(trida, pocet, na.rm = TRUE), .groups = "drop")|>
  pivot_wider(names_from = level, values_from = trida_mean, names_prefix = "trida_")


syst_for_model <- syst_platy |>
  left_join(syst_tridy) |>
  mutate(trida_prumer = ((trida_predst * pocet_predst) +
                           (trida_ostat * pocet_ostat))/pocet_celkem,
         trida_diff = trida_ostat - trida_predst,
         perc_predst = podil_predst * 100,
         index_seniority = trida_prumer ) |>
  drop_na()

syst_for_model_uo <- syst_platy_uo |>
  left_join(syst_tridy_uo) |>
  mutate(trida_prumer = ((trida_predst * pocet_predst) +
                           (trida_ostat * pocet_ostat))/pocet_celkem,
         trida_diff = trida_ostat - trida_predst,
         perc_predst = podil_predst * 100,
         index_seniority = trida_prumer ) |>
  drop_na()

syst_for_model_uo_2022 <- syst_for_model_uo |>
  filter(rok == 2022)



model_by_year_uo <- syst_for_model_uo |>
  group_by(rok) |>
  nest() |>
  mutate(model = map(data, annual_model),
         resids = map2(data, model, add_residuals),
         glance = map(model, broom::glance),
         tidy_mdl = map(model, tidy))

model_by_year_uo |>
  unnest(glance) |>
  arrange(-rok)

gg_resid_year <- ggplot(model_by_year_uo |>
                          unnest(resids), aes(rok, resid)) +
  geom_line_interactive(aes(colour = vztah, tooltip = organizace_nazev,
                            group = paste(organizace_nazev, vztah)), alpha = 0.6) +
  facet_wrap(~kapitola_nazev)

girafe(ggobj = gg_resid_year)

ggplot(model_by_year_uo |>
         unnest(tidy_mdl), aes(rok, estimate)) +
  geom_line() +
  facet_wrap(~term)

syst_for_model |> arrange(-plat_prumer)

plat_model <- lm(plat_prumer ~ perc_predst + trida_predst + trida_ostat + vztah,
                 data = syst_for_model_uo_2022)

summary(plat_model)

plat_model_tidy <- tidy(plat_model)
plat_model_aug <- augment(plat_model, syst_for_model_uo_2022) |>
  mutate(ustredni_organ = ifelse(ustredni_organ, "Ústřední", "Neústřední") |>
           as_factor())

gg <- ggplot(plat_model_aug |>
               mutate(hygiena = str_detect(organizace_nazev, "[Hh]yg")),
             aes(plat_prumer/1e3, .fitted/1e3, tooltip = organizace_nazev)) +
  geom_abline(aes(colour = "45°", intercept = 0, slope = 1), linetype = "dotted") +
  geom_point_interactive(aes(shape = vztah, size = pocet_celkem),
                         alpha = .6, colour = "darkblue") +
  scale_color_manual(name = NULL, values = "darkblue") +
  coord_fixed() +
  scale_shape_manual(values = c(21, 19), breaks = c("prac", "sluz")) +
  facet_grid(ustredni_organ ~ kapitola_typ) +
  ptrr::theme_ptrr(gridlines = "scatter", axis_titles = TRUE) +
  labs(x = "Průměrný plat", y = "Modelovaný průměrný plat")

girafe(ggobj = gg)

ggplot(plat_model_tidy |> filter(term != "(Intercept)")) +
  geom_linerange(aes(xmin = estimate - std.error * 2.96, xmax = estimate + std.error * 2.96,
                     y = term)) +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed") +
  geom_point(aes(x = estimate, y = term))

one_model <- function(df) {
  lm(plat_prumer ~ rok, data = df)
}

syst_for_model |>
  group_by(kapitola_typ, vztah) |>
  nest() |>
  mutate(mdl = map(data, one_model),
         resids = map2(data, mdl, add_residuals),
         glance = map(mdl, broom::glance),
         tidy_mdl = map(mdl, tidy)) |>
  unnest(tidy_mdl) |>
  filter(term == "rok") |>
  ggplot(aes(x = estimate, kapitola_typ)) +
  geom_point(aes(estimate, colour = vztah))

gg_pts <- ggplot(plat_model_aug, aes(.resid, kapitola_typ, tooltip = organizace_nazev)) +
  geom_vline(xintercept = 0) +
  geom_point_interactive(aes(colour = vztah), alpha = .7,
                         position = position_jitter(width = .05)) +
  facet_grid(ustredni_organ ~ .)

girafe(ggobj = gg_pts)
