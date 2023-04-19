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

  plt <- data |>
    ungroup() |>
    mutate(id = row_number()) |>
    ggplot(aes(rok, {{variable}}, group = kap_zkr)) +
    # facet_wrap(~kap_name) +
    geom_point_interactive(aes(tooltip = round({{variable}}), data_id = id),
                           hover_css = "{fill:orange;r:20px;}",
                           color = "white") +
    fn(aes(data_id = kap_zkr), hover_nearest = TRUE, color = "grey40") +
    geom_text_interactive(aes(label = kap_zkr,
                              data_id = kap_zkr), color = NA,
                          hjust = 0, nudge_x = .5,
                          data = ~ subset(., rok == max(rok))) +
    scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
    ptrr::scale_y_number_cz() +
    ptrr::theme_ptrr("both") +
    scale_x_continuous(limits = c(2003, 2024), breaks = seq(2004, 2022, by = 3)) +
    labs(title = title,
         subtitle = subtitle,
         caption = zdroj)
  pltg <- girafe(ggobj = plt) |> girafe_options(opts_hover_inv(css = "opacity:0.2;"),
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
    mutate(kap_name = as.factor(kap_zkr) |> fct_relevel("MZV") |> fct_rev()) |>
    ggplot(aes(rok, rnk, group = kap_zkr, data_id = kap_zkr)) +
    geom_point_interactive(color = "white") +
    geom_bump(lineend = "round", linejoin = "round", alpha = .6, smooth = 5,
              # position = position_jitter(),
              mapping = aes(color = kap_name == "MZV",
                            linewidth = kap_name == "MZV")) +
    scale_color_manual(values = c("grey60", "darkblue")) +
    scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
    geom_text_interactive(data = . %>% filter(rok == max(rok)),
                          aes(label = kap_name, color = kap_name == "MZV"),
                          hjust = 0, nudge_x = 0.5) +
    geom_point_interactive(data = . %>% filter(rok == max(rok)),
                           aes(color = kap_zkr == "MZV")) +
    expand_limits(x = 2024) +
    scale_x_continuous(breaks = seq(2004, 2022, by = 3), expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(n.breaks = 15) +
    guides(color = "none", size = "none") +
    ptrr::theme_ptrr(gridlines = "none") +
    labs(title = "Pořadí - průměrné platy na ministerstvech, 2003-2022",
         subtitle = c("Služební i pracovní místa; jen ústřední orgány"),
         caption = "ISP/Státní závěrečný účet")

  girafe(ggobj = plt_bump) |> girafe_options(opts_hover(css = "color: red; fill: blue"))

}
