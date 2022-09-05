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
