plot_isco_percentiles <- function(pv_isco_long,
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
    theme_ptrr(multiplot = T, legend.position = "top",
               strip.text = element_text(lineheight = 1))
}

plot_isco_dist <- function(pv_isco, isco_ids) {
  pv_isco |>
    filter(isco_digits == 4 | is.na(isco_digits)) |>
    filter(isco_id %in% isco_ids) |>
    arrange(sfera) |>
    ggplot(aes(colour = sfera, y = geo)) +
    geom_pointrange(aes(x = pay_median, xmin = pay_q1, xmax = pay_q3),
                    position = position_dodge2(width = .5), key_glyph = "pointrange_h") +
    facet_wrap(~ isco_full, ncol = 1, scales = "free_x") +
    scale_x_number_cz(limits = c(18e3, 98e3), breaks = seq(from = 30e3, to = 90e3, by = 15e3)) +
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

pv_isco_lengthen <- function(data) {
  pv_isco_pg_long <- data |>
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
  return(pv_isco_pg_long)
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

pv_bind <- function(data_cr, data_reg) {
  data_cr |>
    mutate(geo = "ČR") |>
    bind_rows(data_reg |>
                mutate(geo = "Praha"))
}
