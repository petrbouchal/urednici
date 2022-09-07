


czso_infl_process <- function(czso_infl) {
  infl_qonq <- czso_infl |>
    filter(casz_txt == "stejné období předchozího roku",
           is.na(ucel_txt)) |> # všechny druhy zboží/služeb
    mutate(ctvrtletí = quarter(obdobiod)) |>
    group_by(rok, ctvrtletí) |>
    summarise(inflace_qonq = (mean(hodnota) - 100)/100, .groups = "drop")
  return(infl_qonq)
}

czso_nace_process <- function(czso_pmz_nace, czso_infl_sub) {
  czso_pmz_nace |>
    filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
           typosoby_txt == "přepočtený") |>
    mutate(ctvrtletí = as.numeric(ctvrtletí),
           tm = make_date(rok, ctvrtletí * 3),
           clr = case_when(odvetvi_kod == "O" ~ "veřejná správa",
                           # odvetvi_kod %in% c("P") ~ "vzdělávání",
                           odvetvi_kod %in% c("M") ~ "profesní",
                           odvetvi_kod %in% c("J") ~ "ICT",
                           TRUE ~ "ostatní") |>
             as_factor() |> fct_relevel("ostatní", "profesní", "ICT", "veřejná správa"),
           public = odvetvi_kod %in% c("O")) |>
    arrange(tm, odvetvi_kod) |>
    group_by(odvetvi_kod) |>
    mutate(zmena_qonq = (hodnota - lag(hodnota, n = 4))/lag(hodnota, n = 4)) |>
    left_join(czso_infl_sub, by = ) |>
    select(tm, zmena_qonq, clr, hodnota, public, inflace_qonq) |>
    ungroup() |>
    arrange(clr)
}

czso_make_plot_nace <- function(czso_pmz_nace_clean) {
  library(lubridate)
  ggplot(czso_pmz_nace_clean,
                   aes(tm - lubridate::days(15), zmena_qonq - inflace_qonq, colour = clr, size = public)) +
    guides(colour = guide_legend(reverse = T, title = "Skupina NACE"), size = "none") +
    scale_size_manual(values = c(1, 2), expand = expansion(0, 0)) +
    ptrr::scale_y_percent_cz(limits = c(-.2, .2), expand = expansion(0, 0), breaks = seq(-.2, .2, .05)) +
    labs(title = "Reálná změna platů podle NACE skupin za předchozí rok, 2003 - 2Q 2022",
         subtitle = "Meziroční změna průměrného platu očištěná o změnu spotřebitelských cen",
         x = NULL, y = "Reálná meziroční změna (očištěno o inflaci)",
         caption = "Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y",
                 expand = expansion(0, 0),
                 limits = c(as.Date("2003-01-01"), as.Date("2022-07-01"))) +
    geom_hline(yintercept = 0, colour = "grey10", linetype = "solid") +
    scale_color_manual(values = c("grey40", "blue3", "goldenrod", "red3")) +
    geom_point(alpha = .6) +
    theme_ptrr("both", axis_titles = TRUE, legend.position = "bottom") +
    theme(panel.grid.major = element_line(color = "white"))
}
