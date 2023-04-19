zm |>
  filter(rok %in% 2021:2022, ctvrtletí == 4,
         odvetvi_txt == "Zdravotní a sociální péče",
         typosoby_txt == "přepočtený",
         stapro_txt == "Průměrná hrubá mzda na zaměstnance") |>
  select(odvetvi_txt, hodnota, rok)

zm |>
  filter(rok %in% 2021:2022, ctvrtletí == 4,
         odvetvi_kod == "O",
         typosoby_txt == "přepočtený",
         stapro_txt == "Průměrná hrubá mzda na zaměstnance") |>
  select(odvetvi_txt, hodnota, rok, ctvrtletí)

infl_index |> filter(rok %in% 2021:2022)

134/116

1.04/1.15

zm_plt_dt_y |>
  filter(year(tm) %in% 2022) |>
  select(odvetvi_txt, nominalni_zmena, realna_zmena, hodnota)
