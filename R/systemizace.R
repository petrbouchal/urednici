




# Util fns ----------------------------------------------------------------
lengthen_syst_data <- function(syst_all) {
  syst_all |>
    pivot_longer(cols = matches("predst_|ostat_"), names_to = "trida", values_to = "pocet") |>
    mutate(trida = str_remove(trida, "pocet_")) |>
    separate(trida, into = c("level", "trida")) |>
    mutate(level_nazev = if_else(level == "predst", "Představení", "Běžní zaměstnanci")) |>
    select(-pocet_predst, -pocet_ostat, -platy_celkem, -pozad_obcanstvi, -pozad_zakazkonkurence,
           -pocet_celkem, -plat_prumer)
}

urady_nazvy_exclude <- c("Český báňský úřad",
                         "Česká správa sociálního zabezpečení",
                         "Státní úřad inspekce práce",
                         "Generální finanční ředitelství")

# Core fns ----------------------------------------------------------------

syst_fix_data <- function(syst_all) {
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
             vztah, kapitola_typ, ustredni_organ) |>
    summarise(across(matches("^pocet_|^platy_celkem|pozad"), sum, na.rm = TRUE), .groups = "drop") |>
    replace_na(list(platy_celkem = 0, pocet_celkem = 0)) |>
    mutate(plat_prumer = platy_celkem/pocet_celkem/12) |>
    rename(organizace_nazev = organizace_nazev_new) |>
    arrange(-plat_prumer)

  return(syst_all_fixed)
}

syst_make_platy <- function(syst_all_fixed) {
  syst_all_fixed |>
    filter(date < "2023-04-01", kapitola_kod != "Celkem") |>
    select(date, rok, organizace_nazev, vztah, pocet_celkem, kapitola_typ,
           plat_prumer, pocet_ostat, kapitola_zkr) |>
    mutate(pocet_predst = pocet_celkem - pocet_ostat,
           podil_predst = pocet_predst / pocet_celkem)
}

syst_make_platy_uo <- function(syst_all_fixed) {
  syst_all_fixed |>
    filter(date < "2023-04-01", kapitola_kod != "Celkem",
           !organizace_nazev %in% urady_nazvy_exclude) |>
    select(date, rok, organizace_nazev, vztah, pocet_celkem, kapitola_typ,
           plat_prumer, pocet_ostat, ustredni_organ, kapitola_zkr) |>
    mutate(pocet_predst = pocet_celkem - pocet_ostat,
           podil_predst = pocet_predst / pocet_celkem)
}

syst_make_tridy <- function(syst_pocty_long) {
  syst_pocty_long |>
    filter(date < "2023-04-01", kapitola_kod != "Celkem",
           !organizace_nazev %in% urady_nazvy_exclude) |>
    replace_na(list(pocet = 0)) |>
    mutate(trida = recode(trida, M = "0") |> as.numeric()) |>
    group_by(date, rok, organizace_nazev, vztah, level,
             across(starts_with("kapitola")), ustredni_organ) |>
    summarise(trida_mean = weighted.mean(trida, pocet, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = level, values_from = trida_mean, names_prefix = "trida_")
}

syst_make_tridy_uo <- function(syst_pocty_long_uo) {
  syst_pocty_long_uo |>
    filter(date < "2023-04-01", kapitola_kod != "Celkem",
           !organizace_nazev %in% urady_nazvy_exclude) |>
    replace_na(list(pocet = 0)) |>
    mutate(trida = recode(trida, M = "0") |> as.numeric()) |>
    group_by(date, rok, organizace_nazev, vztah, level,
             across(starts_with("kapitola")), ustredni_organ) |>
    summarise(trida_mean = weighted.mean(trida, pocet, na.rm = TRUE),
              pocet = sum(pocet, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = level, values_from = c(trida_mean, pocet)) |>
    mutate(trida_mean_celkem = (trida_mean_predst * pocet_predst +
                                  trida_mean_ostat * pocet_ostat) /
             (pocet_ostat + pocet_predst))
}

syst_make_platy_for_model <- function(syst_platy, syst_tridy) {
  syst_platy |>
    left_join(syst_tridy) |>
    mutate(trida_prumer = ((trida_predst * pocet_predst) +
                             (trida_ostat * pocet_ostat))/pocet_celkem,
           trida_diff = trida_ostat - trida_predst,
           perc_predst = podil_predst * 100,
           index_seniority = trida_prumer ) |>
    drop_na()
}

syst_make_platy_for_model_uo <- function(syst_platy_uo, syst_tridy_uo) {
  syst_platy_uo |>
    left_join(syst_tridy_uo) |>
    mutate(trida_prumer = ((trida_predst * pocet_predst) +
                             (trida_ostat * pocet_ostat))/pocet_celkem,
           trida_diff = trida_ostat - trida_predst,
           perc_predst = podil_predst * 100,
           index_seniority = trida_prumer ) |>
    drop_na()
}

syst_annual_model <- function(data) {
  lm(plat_prumer ~ perc_predst + trida_predst + trida_ostat + vztah + ustredni_organ + kapitola_vladni,
     data = data)
}

syst_run_model_annual <- function(syst_for_model_uo) {
  syst_for_model_uo |>
    group_by(rok) |>
    nest() |>
    mutate(model = map(data, syst_annual_model),
           resids = map2(data, model, add_residuals),
           glance = map(model, broom::glance),
           tidy_mdl = map(model, tidy))

}

syst_run_model <- function(syst_platy_for_model) {
  syst_platy_for_model_2023 <- syst_platy_for_model |>
    filter(rok == 2023)
  plat_model <- lm(plat_prumer ~ perc_predst + trida_predst + trida_ostat + vztah + ustredni_organ + kapitola_vladni,
                   data = syst_platy_for_model_2023)
}

syst_make_plot_tridy <- function(syst_tridy_uo) {
  dt_all <- syst_tridy_uo |>
    filter(ustredni_organ, kapitola_vladni,
           date == "2023-01-01", vztah == "sluz") |>
    mutate(kapitola_zkr = as.factor(kapitola_zkr) |> fct_reorder(trida_mean_celkem, min))

  ggplot(dt_all, aes(y = kapitola_zkr)) +
    geom_point(aes(x = trida_mean_predst, colour = "Představení"), size = 3) +
    geom_point(aes(x = trida_mean_ostat, colour = "Ostatní"), size = 3) +
    geom_point(aes(x = trida_mean_celkem, colour = "Celkem"), size = 3) +
    scale_colour_manual(name = "Úroveň řízení", values = c(Ostatní = "grey50",
                                                           Celkem = "black",
                                                           Představení = "darkblue")) +
    scale_x_continuous(limits = c(10, 15)) +
    ptrr::theme_ptrr("both", multiplot = FALSE, legend.position = "top") +
    labs(title = "Průměrná platová třída služebních míst (2023)",
         subtitle = "Podle systemizace 2023.\nJen ústřední orgány ve vládních kapitolách, jen služební místa",
         caption = "Plánovaný stav podle systemizace 2023")
}

syst_make_plot_predstaveni <- function(syst_pocty_long_uo) {
  syst_pocty_long_uo |>
    filter(ustredni_organ, kapitola_vladni,
           date == "2023-01-01", vztah == "sluz") |>
    drop_na(pocet) |>
    group_by(kapitola_zkr, level) |>
    summarise(pocet = sum(pocet), .groups = "drop") |>
    group_by(kap = kapitola_zkr) |>
    mutate(podil = pocet/sum(pocet)) |>
    filter(level == "predst") |>
    ungroup() |>
    mutate(kap = as.factor(kap) |> fct_reorder(podil)) |>
    ggplot(aes(podil, kap)) +
    geom_col() +
    ptrr::scale_x_percent_cz(n.breaks = 6) +
    ptrr::theme_ptrr("x") +
    labs(title = "Podíl představených na všech zaměstnancích",
         subtitle = "Systemizace 2023, jen služební místa, jen centrální úřady")
}

syst_make_plot_platy <- function(syst_platy_uo) {
  syst_platy_uo |>
    filter(ustredni_organ, kapitola_typ == "Ministerstva a ÚV",
           date == "2023-01-01", vztah == "sluz") |>
    mutate(kapitola_zkr = as.factor(kapitola_zkr) |> fct_reorder(plat_prumer, mean)) |>
    ggplot(aes(y = kapitola_zkr)) +
    geom_col(aes(x = plat_prumer), colour = "grey30", width = .85) +
    scale_x_number_cz(expand = flush_axis) +
    ptrr::theme_ptrr("x", multiplot = FALSE, legend.position = "top") +
    labs(title = "Průměrný plat na služebních místech (2023)",
         subtitle = "Podle systemizace 2023.\nJen ústřední orgány ve vládních kapitolách, jen služební místa",
         caption = "Plánovaný stav podle systemizace 2023")
}

syst_make_plot_residyr <- function(syst_model_annual) {
  gg_resid_year <- ggplot(syst_model_annual |>
           unnest(resids), aes(rok, resid)) +
    geom_line_interactive(aes(colour = vztah, tooltip = organizace_nazev,
                              group = paste(organizace_nazev, vztah)), alpha = 0.6) +
    facet_wrap(~kapitola_nazev) +
    theme_ptrr(multiplot = TRUE, legend.position = "top")

  girafe(ggobj = gg_resid_year, pointsize = 8)
}

syst_make_plot_coefyr <- function(syst_model_annual) {
  ggplot(syst_model_annual |>
           unnest(tidy_mdl), aes(rok, estimate)) +
    geom_line() +
    facet_wrap(~term) +
    theme_ptrr(multiplot = TRUE)
}

syst_make_predictions <- function(syst_model, syst_platy_for_model) {
  syst_platy_for_model_2023 <- syst_platy_for_model |>
    filter(date == "2023-01-01")

  syst_model_predictions <- augment(syst_model, syst_platy_for_model_2023) |>
    mutate(ustredni_organ = ifelse(ustredni_organ, "Ústřední", "Neústřední") |>
             as_factor())

  return(syst_model_predictions)
}

syst_make_plot_model <- function(syst_model_tidy) {
  ggplot(syst_model_tidy |> filter(term != "(Intercept)")) +
    geom_linerange(aes(xmin = estimate - std.error * 2.96, xmax = estimate + std.error * 2.96,
                       y = term)) +
    geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed") +
    geom_point(aes(x = estimate, y = term)) +
    theme_ptrr("both")
}

syst_make_plot_model_predictions_all <- function(syst_model_predictions) {
  gg <- ggplot(syst_model_predictions |>
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
    labs(x = "Průměrný plat (tis. Kč)", y = "Modelovaný průměrný plat (tis. Kč)")

  girafe(ggobj = gg, pointsize = 8)
}

syst_make_plot_model_by_grp <- function(syst_platy_for_model) {
  one_model <- function(df) {
    lm(plat_prumer ~ rok, data = df)
  }

  syst_platy_for_model |>
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
}

syst_make_plot_model_resid <- function(syst_model_predictions) {

  gg_pts <- ggplot(syst_model_predictions, aes(.resid, 1,
                                       tooltip = organizace_nazev)) +
    geom_vline(xintercept = 0) +
    geom_point_interactive(aes(colour = vztah), alpha = .7,
                           position = position_jitter(width = .05)) +
    facet_grid(ustredni_organ ~ kapitola_typ) +
    theme_ptrr("x", multiplot = T, axis.text.y = element_blank())

  girafe(ggobj = gg_pts, height_svg = 2, pointsize = 8)
}

syst_make_plot_model_resid_uo <- function(syst_model_predictions) {
  ggplot(syst_model_predictions |>
           filter(vztah == "sluz", ustredni_organ == "Ústřední",
                  kapitola_typ == "Ministerstva a ÚV") |>
           mutate(organizace_nazev = as.factor(organizace_nazev) |>
                    fct_reorder(.resid) |> fct_rev()),
         aes(.resid, organizace_nazev)) +
    geom_col() +
    theme_ptrr("x")
}

