plot_mini_bar <- function(szu_sections, variable, rok = 2022, title, subtitle,
                           zdroj =  "ISP/Státní závěrečný účet") {

  rk <- rok

  data <- szu_sections |>
    filter(faze_rozpoctu == "SKUT",
           rok == rk,
           kap_zkr != "MI",
           kategorie_2014 == "Ministerstva") |>
    mutate(var = {{variable}},
           kap_zkr = fct_reorder(kap_zkr, var))

  var_slcted <- pull(data, {{variable}})

  plt <- ggplot(data, aes(var, kap_zkr)) +
    geom_col() +
    geom_text(aes(label = label_number_cz(1)(var)),
              hjust = 1,
              nudge_x = -300,
              colour = "white", family = "IBM Plex Sans Condensed") +
    theme_ptrr("x", panel.grid.minor.x = element_line(linewidth = .5)) +
    scale_x_number_cz(n.breaks = 7) +
    labs(title = title,
         subtitle = subtitle,
         caption = zdroj)

  return(plt)
}

plot_mini_line <- function(szu_sections, variable, title, subtitle,
                           zdroj =  "ISP/Státní závěrečný účet",
                           highlight_mini = NA,
                           fn = geom_line_interactive, girafe = TRUE) {

  data <- szu_sections |>
    filter(faze_rozpoctu == "SKUT",
           # rok == 2022,
           kap_zkr != "MI",
           kategorie_2014 == "Ministerstva")

  if (!is.na(highlight_mini)) {
    data <- data |>
      mutate(kap_zkr = as.factor(kap_zkr) |> fct_relevel(highlight_mini) |> fct_rev())
  }

  var_slcted <- pull(data, {{variable}})

  format_fn <- if(max(var_slcted, na.rm = TRUE) > 2) ptrr::label_number_cz(accuracy = 1) else ptrr::label_percent_cz(.1)
  line_yintercept <- ifelse(max(var_slcted, na.rm = TRUE) > .9, 1, 0)

  plt <- data |>
    ungroup() |>
    mutate(id = row_number()) |>
    ggplot(aes(rok, {{variable}}, group = kap_zkr)) +
    # facet_wrap(~kap_name) +
    geom_point_interactive(aes(tooltip = paste0(kap_zkr, ": ", format_fn({{variable}})),
                               data_id = id),
                           hover_css = "{fill:orange;r:20px;}",
                           color = "white") +
    fn(aes(data_id = kap_zkr), hover_nearest = TRUE, color = "grey40") +
    geom_text_interactive(aes(label = kap_zkr,
                              data_id = kap_zkr), color = NA,
                          hjust = 0, nudge_x = .5,
                          family = "IBM Plex Sans",
                          data = ~ subset(., rok == max(rok))) +
    scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
    ptrr::scale_y_number_cz() +
    ptrr::theme_ptrr("both") +
    scale_x_continuous(limits = c(2003, 2024), breaks = seq(2004, 2022, by = 3)) +
    labs(title = title,
         subtitle = subtitle,
         caption = zdroj)


  if(max(var_slcted, na.rm = TRUE) < 2) {plt <- plt +
    ptrr::scale_y_percent_cz(n.breaks = 10) +
    geom_hline(yintercept = line_yintercept)}

  pltg <- girafe(ggobj = plt,
                 fonts = list(sans = "IBM Plex Sans; Helvetica; Arial; sans-serif")) |>
    girafe_options(opts_hover_inv(css = "opacity:0.2;"),
                   opts_tooltip(css = gir_tooltip_css),
                   opts_hover(nearest_distance = 20,
                              css = girafe_css(css = "fill:darkblue;color:red; stroke: darkblue;",
                                               text = "fill:darkblue; stroke: none;",
                                               line = "stroke:darkblue;fill:none;")))

  if(!is.na(highlight_mini) & !girafe) {plt <- plt +
    geom_line(data = ~subset(., kap_zkr %in% highlight_mini), color = "darkblue", linewidth = 1) +
    geom_text(data = ~subset(., kap_zkr %in% highlight_mini & rok == max(rok)),
              aes(label = kap_zkr,
                  hjust = 0, nudge_x = .5))}

  rslt <- if(girafe) pltg else plt
  return(rslt)
}

make_szu_plot_bump <- function(szu_sections) {
  data <- szu_sections |>
    filter(faze_rozpoctu == "SKUT",
           # rok == 2022,
           kap_zkr != "MI",
           kategorie_2014 == "Ministerstva")

  plt_bump <- data |>
    group_by(rok) |>
    mutate(rnk = rank(prumerny_plat)) |>
    ungroup() |>
    ggplot(aes(rok, rnk, group = kap_zkr, data_id = kap_zkr)) +
    geom_point_interactive(color = "white") +
    geom_bump(lineend = "round", linejoin = "round", alpha = .6, smooth = 5,
              color = "grey50") +
    geom_text_interactive(data = . %>% filter(rok == max(rok)),
                          aes(label = kap_zkr),
                          hjust = 0, nudge_x = 0.5, color = "grey20") +
    geom_point_interactive(data = . %>% filter(rok == max(rok)),
                           color = "grey20") +
    expand_limits(x = 2024) +
    scale_x_continuous(breaks = seq(2004, 2022, by = 3), expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(n.breaks = 15) +
    guides(color = "none", size = "none") +
    ptrr::theme_ptrr(gridlines = "none") +
    labs(title = "Pořadí - průměrné platy na ministerstvech, 2003-2022",
         subtitle = c("Služební i pracovní místa; jen ústřední orgány"),
         caption = "ISP/Státní závěrečný účet")

  girafe(ggobj = plt_bump,
         fonts = list(sans = "IBM Plex Sans; Helvetica; Arial; sans-serif"),) |>
    girafe_options(opts_hover(css = "color: red; fill: blue"),
                   opts_tooltip(css = gir_tooltip_css))

}
