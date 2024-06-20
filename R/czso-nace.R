


czso_infl_process_quarterly <- function(czso_infl) {
  infl_qonq <- czso_infl |>
    filter(casz_txt == "stejné období předchozího roku",
           is.na(ucel_txt)) |> # všechny druhy zboží/služeb
    mutate(ctvrtleti = quarter(obdobiod)) |>
    group_by(rok, ctvrtleti) |>
    summarise(inflace_qonq = (mean(hodnota) - 100)/100, .groups = "drop")
  return(infl_qonq)
}

czso_nace_process_quarterly <- function(czso_pmz_nace, czso_infl_sub) {
  czso_pmz_nace |>
    filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
           typosoby_txt == "přepočtený") |>
    mutate(ctvrtleti = as.numeric(ctvrtleti),
           tm = make_date(rok, ctvrtleti * 3),
           clr = case_when(odvetvi_kod == "O" ~ "Veřejná správa",
                           # odvetvi_kod %in% c("P") ~ "vzdělávání",
                           is.na(odvetvi_kod) ~ "Celá ekonomika",
                           odvetvi_kod %in% c("M") ~ "Profesní",
                           odvetvi_kod %in% c("J") ~ "ICT",
                           TRUE ~ "Ostatní") |>
             as_factor() |> fct_relevel("Ostatní", "Profesní", "ICT", "Veřejná správa", "Celá ekonomika"),
           public = odvetvi_kod %in% c("O")) |>
    arrange(tm, odvetvi_kod) |>
    group_by(odvetvi_kod, odvetvi_txt) |>
    mutate(zmena_qonq = (hodnota - lag(hodnota, n = 4))/lag(hodnota, n = 4)) |>
    left_join(czso_infl_sub, by = ) |>
    select(tm, zmena_qonq, clr, hodnota, public, inflace_qonq, odvetvi_txt) |>
    ungroup() |>
    arrange(clr)
}

czso_make_plot_nace_quarterly <- function(czso_pmz_nace_clean) {
  library(lubridate)
  ggplot(czso_pmz_nace_clean,
                   aes(tm - lubridate::days(15), zmena_qonq - inflace_qonq, colour = clr, size = public)) +
    guides(colour = guide_legend(reverse = T, title = "Skupina NACE"), size = "none") +
    scale_size_manual(values = c(1, 2), expand = expansion(0, 0)) +
    ptrr::scale_y_percent_cz(limits = c(-.2, .2), expand = expansion(0, 0), breaks = seq(-.2, .2, .05)) +
    labs(title = "Reálná změna platů podle NACE skupin za předchozí rok, 2003 - 1Q 2024",
         subtitle = "Meziroční změna průměrného platu očištěná o změnu spotřebitelských cen",
         x = NULL, y = "Reálná meziroční změna (očištěno o inflaci)",
         caption = "Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y",
                 expand = expansion(0, 0),
                 limits = c(as.Date("2003-01-01"), as.Date("2024-07-01"))) +
    geom_hline(yintercept = 0, colour = "grey10", linetype = "solid") +
    scale_color_manual(values = c("grey40", "blue3", "goldenrod", "red3", "black")) +
    geom_point(alpha = .6) +
    theme_ptrr("both", axis_titles = TRUE, legend.position = "bottom") +
    theme(panel.grid.major = element_line(color = "white"))
}

czso_infl_process_annual <- function(czso_infl) {
  czso_infl |>
    filter(casz_txt == "stejných 12 měsíců předchozího roku", mesic == 12,
           is.na(ucel_txt)) |> # všechny druhy zboží/služeb
    group_by(rok) |>
    select(inflace_yony = hodnota, rok)
}

czso_nace_process_annual <- function(czso_pmz_nace, czso_infl_annual) {
  czso_pmz_nace |>
    filter(stapro_txt == "Průměrná hrubá mzda na zaměstnance",
           typosoby_txt == "přepočtený") |>
    group_by(rok, odvetvi_kod, odvetvi_txt) |>
    summarise(hodnota = mean(hodnota), .groups = "drop") |>
    mutate(tm = make_date(rok, 01, 01),
           clr = case_when(odvetvi_kod == "O" ~ "Veřejná správa",
                           # odvetvi_kod %in% c("P") ~ "vzdělávání",
                           is.na(odvetvi_kod) ~ "Celá ekonomika",
                           odvetvi_kod %in% c("M") ~ "Profesní",
                           odvetvi_kod %in% c("J") ~ "ICT",
                           TRUE ~ "Ostatní") |>
             as_factor() |> fct_relevel("Ostatní", "Profesní", "ICT", "Veřejná správa"),
           public = odvetvi_kod %in% c("O"),
           public_broad = odvetvi_kod %in% c("O", "P", "Q")) |>
    ungroup() |>
    left_join(czso_infl_annual, by = "rok") |>
    select(tm, clr, hodnota, public, public_broad, inflace = inflace_yony, odvetvi_kod, odvetvi_txt) |>
    arrange(odvetvi_kod, tm) |>
    group_by(odvetvi_kod) |>
    # počítáme nominální a reálnou změnu
    mutate(nominalni_zmena = hodnota/lag(hodnota, 1),
           realna_zmena = nominalni_zmena/inflace*100 - 1) |>
    ungroup() |>
    # select(tm, mzda = hodnota, realna_mzda, realna_zmena, clr, hodnota, public, realna_zmena_2) |>
    ungroup() |>
    arrange(clr)
}

czso_make_plot_nace_annual <- function(data, add_years = 5) {
  data <- data |>
    drop_na(realna_zmena) |>
    group_by(tm) |>
    mutate(is_minmax = realna_zmena == max(realna_zmena) | realna_zmena == min(realna_zmena)) |>
    ungroup() |>
    mutate(is_last_period = tm == max(tm),
           needs_label = is_last_period & (is_minmax | clr != "Ostatní"),
           name_for_label = ifelse(is_minmax, odvetvi_txt, as.character(clr)) |> str_wrap(30)) |>
    mutate(nudge = case_match(clr,
                              "Celá ekonomika" ~ .01,
                              "Veřejná správa" ~ .0,
                              "Profesní" ~ -.01,
                              "ICT" ~ .01,
                              )
           )

  new_labels <- c(seq(year(min(data$tm)) - 1,
                      year(max(data$tm)) + 1, by = 2),
                  rep(" ", times = add_years))

  new_breaks <- make_date(seq(from = year(min(data$tm)) - 1,
                              to = year(max(data$tm)) + 1 + add_years, by = 2))

  fmt_pct_change <- scales::label_number(.1, 100, suffix = " %", decimal.mark = ",",
                                         style_positive = "plus", style_negative = "minus")
  fmt_pct_change_axis <- scales::label_number(1, 100, suffix = " %", decimal.mark = ",",
                                              style_positive = "plus", style_negative = "minus")

  zm_plt <- ggplot(data |>
                     # filter(month(tm) == 12) |> mutate(tm = floor_date(tm, "year")) |>
                     filter(),
                   aes(tm, realna_zmena, colour = clr, size = clr != "Ostatní",
                       linewidth = clr != "Ostatní",
                       group = clr)) +
    geom_hline(yintercept = 0, colour = "grey10", linetype = "solid") +
    geom_point(aes(alpha = clr != "Ostatní"), fill = "white") +
    scale_alpha_discrete(range = c(.6, 1), guide = "none") +
    geom_line(data = ~subset(., clr == "Celá ekonomika")) +
    geom_point(data = ~subset(., clr == "Celá ekonomika"), size = 2) +
    geom_point(data = ~subset(., clr == "Celá ekonomika"), colour = "white", size = 1.2) +
    geom_line(data = ~subset(., public == TRUE)) +
    geom_point(data = ~subset(., public == TRUE), size = 2) +
    geom_point(data = ~subset(., public == TRUE), colour = "white", size = 1.2) +
    geom_label(data = ~subset(., needs_label),
               aes(label = paste(fmt_pct_change(realna_zmena), name_for_label),
                   fill = clr, y = realna_zmena + nudge),
               label.padding = unit(0.2, "lines"),
               hjust = 0, nudge_x = 40, color = "white", family = "Arial", size = 3, fontface = "bold") +
    scale_color_manual(values = c(Ostatní = "grey40", Profesní = "blue3",
                                  `Celá ekonomika` = "grey20",
                                  `ICT` = "goldenrod", `Veřejná správa` = "red3")) +
    scale_fill_manual(values = c(Ostatní = "grey40", Profesní = "blue3",
                                 `Celá ekonomika` = "grey20",
                                 `ICT` = "goldenrod", `Veřejná správa` = "red3")) +
    scale_size_manual(values = c(1, 2), guide = "none") +
    scale_linewidth_manual(values = c(1, 2), guide = "none") +
    scale_x_date(date_breaks = "2 years",
                 # date_labels = "%Y",
                 # breaks = 2001:2021,
                 labels = function(x) {
                   lbls <- lubridate::year(x)
                   lbls[lbls > lubridate::year(max(data$tm))] <- " "
                   lbls
                 },
                 expand = expansion(add = c(0, 365 * add_years)),
                 limits = c(as.Date("2000-09-01"), as.Date("2024-03-31")),
    ) +
    scale_y_continuous(expand = expansion(add = c(.02, 0.001)),
                       # limits = c(-.2, .2),
                       labels = fmt_pct_change_axis,
                       breaks = seq(-.3, .3, .05)) +
    guides(colour = guide_legend(reverse = T, title = "Skupina NACE (odvětví)"), size = "none", fill = "none") +
    labs(title = "Meziroční změny průměrných reálných mezd (v odvětvích dle NACE), v %",
         x = "Rok", y = "Reálná meziroční změna (očištěno o inflaci)",
         caption = "Zdroj: vlastní výpočet z dat ČSÚ (sady 110079 Mzdy, náklady práce - časové řady a 010022 Indexy spotř. cen)") +
    # theme_urednici +
    theme(panel.grid.major = element_line(color = "white")) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    ptrr::theme_ptrr(gridlines = "both", legend.position = "none")
  zm_plt
}
