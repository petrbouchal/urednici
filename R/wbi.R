wbi_get_wagepremiums <- function() {
  wb_data(indicator = c("BI.WAG.PREM.PB.GP",
                        "BI.WAG.PREM.PB.PN",
                        "BI.WAG.PREM.PB.SG",
                        "BI.WAG.PREM.PB.CK",
                        "BI.WAG.PREM.PB.TT",
                        "BI.WAG.PREM.PB.SN"),
          return_wide = FALSE) |>
    mutate(indicator_lbl = str_remove(indicator, "Public sector wage premium, ") |>
             str_replace(": ", "\n") |> as.factor() |> fct_rev())
}

wbi_make_plot_line <- function(wbi_wagepremiums) {
  wbi_wagepremiums |>
    filter(iso2c %in% c("CZ", "SK", "FI", "AT", "PL", "DE")) |>
    mutate(iso2c = as.factor(iso2c) |> fct_relevel("CZ", after = 0)) |>
    arrange(desc(iso2c)) |>
    ggplot(aes(date, value, colour = iso2c, group = paste(indicator_lbl, iso2c))) +
    geom_hline(yintercept = 0) +
    geom_line(aes(size = iso2c == "CZ",
                  alpha = iso2c == "CZ")) +
    scale_size_manual(values = c(`TRUE` = 1.6, `FALSE` = 1)) +
    scale_alpha_discrete(range = c(0.4, 1)) +
    facet_wrap(~ indicator_lbl) +
    guides(size = "none", alpha = "none") +
    scale_y_percent_cz() +
    scale_color_manual(values = c("darkblue", "darkred", "goldenrod", "steelblue", "red", "forestgreen")) +
    theme_ptrr(multiplot = T) +
    labs(title = "Public Sector Wage Premium: vývoj v čase",
         colour = "Stát",
         subtitle = "Pozitivní = platy ve veřejném sektoru vyšší než v soukromém")

}

wbi_make_plot_bar <- function(wbi_wagepremiums) {
  wbi_wagepremiums |>
    filter(iso2c %in% c("CZ", "SK", "FI", "DK", "AT", "PL", "DE")) |>
    drop_na(value) |>
    group_by(iso2c, indicator_lbl) |>
    slice_max(date, n = 3) |>
    summarise(value = mean(value), .groups = "drop") |>
    ggplot(aes(value, iso2c)) +
    geom_col(aes(fill = iso2c == "CZ")) +
    geom_vline(xintercept = 0) +
    scale_x_percent_cz() +
    facet_wrap(~ indicator_lbl) +
    theme_ptrr(multiplot = T) +
    scale_fill_manual(values = c("darkgrey", "darkblue")) +
    guides(fill = "none") +
    labs(title = "Public Sector Wage Premium: srovnání",
         subtitle = "Pozitivní = platy ve veřejném sektoru vyšší než v soukromém\nPrůměr za poslední 3 dostupné roky")

}
