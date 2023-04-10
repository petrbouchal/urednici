ec_make_plot_multi <- function(ec_pay) {
  ec_pay |>
    filter(unit != "NAC", values != "0", geo != "EU27_2020") |>
    arrange(geo, time) |>
    mutate(geo = as.factor(geo) |> fct_reorder(values, last, .desc = TRUE)) |>
    ggplot(aes(time, values)) +
    geom_line(aes(alpha = unit), colour = "darkblue", size = 1) +
    facet_wrap(~geo) +
    scale_alpha_discrete(range = c(.6, 1)) +
    ptrr::theme_ptrr(gridlines = "both", multiplot = T, legend.position = "top") +
    labs(title = "Platy úředníků centrální státní správy zemí EU: vývoj v čase",
         subtitle = "EUR / EUR v paritě kupní sily (PPS)\nStáty seřazeny podle nejnovější hodnoty",
         caption = "Zdroj: Eurostat, sada PRC_REM_AVG",
         alpha = "Jednotka / vyjádření")

}

ec_make_plot_bar <- function(ec_pay) {
  dta <- ec_pay |>
    filter(unit != "NAC", values != "0", geo != "EU27_2020") |>
    group_by(geo, unit) |>
    slice_max(time, n = 3) |>
    summarise(values = mean(values, na.rm = T), .groups = "drop") |>
    arrange(unit, values) |>
    mutate(order = row_number())

  ggplot(dta, aes(order, values)) +
    geom_bar(aes(fill = geo == "CZ"), stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = c("darkgrey", "darkblue")) +
    facet_wrap(~unit, scales = "free_y") +
    scale_x_continuous(breaks = dta$order, labels = dta$geo, expand = c(0.01, 0)) +
    scale_y_number_cz(expand = flush_axis) +
    guides(fill = "none") +
    ptrr::theme_ptrr(gridlines = "x", multiplot = T, legend.position = "top") +
    labs(title = "Platy úředníků centrální státní správy zemí EU: srovnání",
         subtitle = "průměr za poslední 3 roky v EUR / EUR v paritě kupní sily (PPS)",
         caption = "Zdroj: Eurostat, sada PRC_REM_AVG",
         alpha = "Jednotka / vyjádření")
  }

ec_make_plot_count_bar <- function(ec_counts) {
  dta <- ec_counts |>
    filter(statinfo == "TOTAL", values != "0",
           geo != "EU27_2020", time == max(time)) |>
    group_by(geo, unit) |>
    summarise(values = mean(values, na.rm = T), .groups = "drop") |>
    mutate(geo = as.factor(geo) |> fct_reorder(values))

  ggplot(dta, aes(geo, values)) +
    geom_bar(aes(fill = geo == "CZ"), stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = c("darkgrey", "darkblue")) +
    scale_y_number_cz(expand = flush_axis) +
    guides(fill = "none") +
    ptrr::theme_ptrr(gridlines = "x", multiplot = T, legend.position = "top") +
    labs(title = "Počty úředníků centrální státní správy zemí EU: srovnání",
         caption = "Zdroj: Eurostat, sada PRC_REM_NR")
}

process_oecd_wages <- function(earn_oecd) {
  earn_oecd |>
    filter(Time == 2021, SERIES == "CPNCU") |>
    select(unit = UNIT, geo = COUNTRY, wage_avg = ObsValue, time = Time) |>
    mutate(geo = countrycode(geo, "iso3c", "eurostat"),
           income_oecd = as.numeric(wage_avg)/12) |>
    select(-wage_avg)
}

process_ec_pay <- function(ec_pay) {
  ec_pay |>
    filter(unit != "NAC", values != "0", geo != "EU27_2020") |>
    group_by(geo, unit) |>
    slice_max(time, n = 3) |>
    summarise(values = mean(values, na.rm = T), .groups = "drop") |>
    arrange(unit, values) |>
    mutate(order = row_number())
}

ec_make_plot_earnoecd <- function(ec_pay, earn_oecd_sub) {
  dta_by_oecd <- ec_pay |>
    filter(unit == "NAC", values != "0", geo != "EU27_2020") |>
    group_by(geo, unit) |>
    slice_max(time, n = 3) |>
    summarise(values = mean(values, na.rm = T), .groups = "drop") |>
    left_join(earn_oecd_sub |> select(-unit), by = c("geo")) |>
    mutate(values = values/income_oecd, geo =
             as.factor(geo) |>
             fct_reorder(values, mean) |>
             fct_relevel("RO", "CY", "MT", "HR", "BG"))

  ggplot(dta_by_oecd, aes(values, geo)) +
    geom_col(aes(fill = geo == "CZ")) +
    geom_vline(xintercept = 1, linetype = "dotted", colour = "white") +
    guides(fill = "none") +
    scale_fill_manual(values = c("darkgrey", "darkblue")) +
    scale_x_percent_cz(expand = flush_axis, breaks = seq(0, 1.5, .25)) +
    theme_ptrr("x", axis_titles = T) +
    labs(title = ,
         subtitle = "",
         y = NULL,
         x = ""
         )
}

ec_make_plot_earnses <- function(ec_pay, earn_ses_monthly) {
  dta_by_ses <- earn_ses_monthly |>
    filter(isco08 == "TOTAL", age == "TOTAL", sex == "T", worktime == "TOTAL",
           time == "2018-01-01", indic_se == "MED_E_EUR", nace_r2 == "B-S_X_O") |>
    select(geo, wage_median = values) |>
    mutate(unit = "EUR") |>
    inner_join(ec_pay |>
                 filter(unit != "NAC", values != "0", geo != "EU27_2020",
                        time == "2018-07-01"),
               by = c("geo", "unit")) |>
    mutate(values = values/wage_median, geo = as.factor(geo) |>
             fct_reorder(values))

  ggplot(dta_by_ses, aes(values, geo)) +
    geom_vline(xintercept = 1, linetype = "dotted", colour = "grey40") +
    geom_col(aes(fill = geo == "CZ")) +
    geom_vline(xintercept = 1, linetype = "dotted", colour = "white") +
    scale_fill_manual(values = c("darkgrey", "darkblue")) +
    guides(fill = "none") +
    scale_x_percent_cz(expand = flush_axis, breaks = seq(0, 2, .25)) +
    theme_ptrr("x", axis_titles = T, plot.margin = unit(c(5.5, 15, 5.5, 5.5), "pt")) +
    labs(title = "",
         subtitle = "",
         y = NULL,
         x = ""
    )
}

