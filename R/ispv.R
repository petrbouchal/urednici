pv_make_plot_isco_percentiles <- function(pv_isco_long,
                                  isco_ids) {
  pv_isco_long |>
    filter(isco_digits == 4 | is.na(isco_digits)) |>
    filter(isco_id %in% isco_ids) |>
    ggplot(aes(percentile, value/1e3, colour = sfera, group = sfera)) +
    scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
    scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                   `Mzdová sféra` = "darkgrey")) +
    geom_line(size = 1) +
    geom_point(size = 1) +
    facet_wrap(. ~  isco_full + geo, ncol = 4) +
    theme_ptrr(base_size = 9, multiplot = T, legend.position = "top",
               strip.text = element_text(lineheight = 1))
}

pv_make_plot_isco_dist <- function(pv_isco, isco_ids) {
  pv_isco |>
    filter(isco_digits == 4 | is.na(isco_digits)) |>
    filter(isco_id %in% isco_ids) |>
    arrange(sfera) |>
    ggplot(aes(colour = sfera, y = geo)) +
    geom_pointrange(aes(x = pay_median/1e3, xmin = pay_q1/1e3, xmax = pay_q3/1e3),
                    position = position_dodge2(width = .5)) +
    facet_wrap(~ isco_full, ncol = 1, scales = "free_x") +
    scale_x_number_cz(limits = c(18, 135), breaks = seq(from = 30, to = 135, by = 15)) +
    scale_color_manual(values = c("grey40", "darkblue")) +
    theme_ptrr(multiplot = T, gridlines = "x", legend.position = "top") +
    labs(colour = "Sféra")
}

isco_recode <- function(data) {
  data |>
    mutate(sfera = tolower(sfera) |>
             recode(mzs = "Mzdová sféra",
                    pls = "Platová sféra"))
}

pv_fix_totals <- function(data) {
  data |>
    mutate(category = if_else(str_detect(category, "CELKEM"), "Celkem", category))
}

pv_dist_lengthen <- function(data, vars) {
  data |>
    pivot_longer(cols = c(pay_median, matches("pay_[dq]"))) |>
    select(name, value, sfera, {{vars}}) |>
    mutate(percentile = case_when(name == "pay_d1" ~ 10,
                                  name == "pay_q1" ~ 25,
                                  name == "pay_median" ~ 50,
                                  name == "pay_q3" ~ 75,
                                  name == "pay_d9" ~ 90
    )) |>
    mutate(name = as.factor(name) |>
             fct_relevel("pay_d1", "pay_q1", "pay_median", "pay_q3", "pay_d9"))
}

pv_isco_lengthen <- function(data) {
  data |>
    pivot_longer(cols = c(pay_median, matches("pay_[dq]"))) |>
    select(starts_with("isco"), sfera, name, value, fte_thous) |>
    mutate(percentile = case_when(name == "pay_d1" ~ 10,
                                  name == "pay_q1" ~ 25,
                                  name == "pay_median" ~ 50,
                                  name == "pay_q3" ~ 75,
                                  name == "pay_d9" ~ 90
    )) |>
    mutate(name = as.factor(name) |>
             fct_relevel("pay_d1", "pay_q1", "pay_median", "pay_q3", "pay_d9"))
}

pv_isco_bind <- function(data_cr, data_reg) {
  data_cr |>
    mutate(geo = "ČR") |>
    filter(isco_digits == 4) |>
    bind_rows(data_reg |>
                mutate(geo = "Praha", isco_digits = 4) |>
                rename(isco_id = isco4_id, isco_name = isco4_name,
                       isco_full = isco4_full))
}

pv_bind <- function(data_cr, data_pg) {
  data_cr |>
    mutate(geo = "ČR") |>
    bind_rows(data_pg |>
                mutate(geo = "Praha"))
}

pv_make_plot_ga_cr <- function(pv_genderage_cr) {
  pv_ga_cr_long <- pv_genderage_cr |>
    mutate(category = str_remove(category, " - (mzdová|platová) sféra"),
           grp = str_extract(category, "^[A-Ž]{4,6}") |> tolower()) |>
    fill(grp) |>
    filter(grp == "celkem") |>
    pv_dist_lengthen(category)

  ggplot(pv_ga_cr_long, aes(percentile, value, colour = sfera, group = sfera)) +
    scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
    scale_y_number_cz() +
    scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                   `Mzdová sféra` = "darkgrey")) +
    geom_line(size = 1.5) +
    geom_point(size = 1.5) +
    # scale_color_viridis_d() +
    facet_wrap(~ category) +
    theme_ptrr("both", multiplot = T)
}

pv_make_plot_ga_pg <- function(pv_genderage_pg) {
  pv_ga_pg_long <- pv_genderage_pg |>
    mutate(category = str_remove(category, " - (mzdová|platová) sféra"),
           grp = str_extract(category, "^[A-Ž]{4,6}") |> tolower()) |>
    fill(grp) |>
    filter(grp == "celkem") |>
    pv_dist_lengthen(category)

  ggplot(pv_ga_pg_long, aes(percentile, value, colour = sfera, group = sfera)) +
    scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
    scale_y_number_cz() +
    scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                   `Mzdová sféra` = "darkgrey")) +
    geom_line(size = 1.5) +
    geom_point(size = 1.5) +
    # scale_color_viridis_d() +
    facet_wrap(~ category) +
    theme_ptrr("both", multiplot = T)
}

pv_make_plot_edu <- function(pv_edu_pg, pv_edu_cr) {
  pv_edu_pg_long <- pv_edu_pg |>
    filter(category == "Vysokoškolské") |>
    pv_dist_lengthen(category)

  pv_edu_cr_long <- pv_edu_cr |>
    filter(category == "Vysokoškolské") |>
    pv_dist_lengthen(category)

  ggplot(pv_bind(pv_edu_cr_long, pv_edu_pg_long),
         aes(percentile, value, colour = sfera, group = sfera)) +
    scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
    scale_y_number_cz() +
    scale_colour_manual(values = c(`Platová sféra` = "darkblue",
                                   `Mzdová sféra` = "darkgrey")) +
    geom_line(size = 1.5) +
    geom_point(size = 1.5) +
    facet_wrap(~geo) +
    theme_ptrr("both", multiplot = T)
}
