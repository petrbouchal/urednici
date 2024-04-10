filenames <- list.files("data-input/csu-rocenky", full.names = TRUE)
names(filenames) <- str_extract(filenames, "[0-9]{4}")
filenames


rocenky_trhprace_data <- map_dfr(filenames,
    .id = "rocenka_rok",
    ~ readxl::read_excel(.x, skip = 4, col_types = "text")
)

rocenky_lng <- rocenky_trhprace_data |>
    pivot_longer(cols = matches("^20")) |>
    # empty rows
    drop_na(value) |>
    # rows with notes
    drop_na(Sectors) |>
    # remove notes from years
    mutate(rok = str_sub(name, 1, 4)) |>
    mutate(value = as.numeric(value)) |>
    # distinguish variables
    mutate(variable = if_else(value > 10000, "plat", "pocet")) |>
    # remove notes from category names
    mutate(Sektory = str_remove(Sektory, "[0-9]\\)") |> str_squish()) |>
    # distinguish duplicate categories by what group they appear in
    group_by(rocenka_rok, variable, rok, Sektory) |>
    mutate(Sektory2 = case_when(
        n() == 1 ~ Sektory,
        n() == 2 & row_number() == 1 ~ paste("nefinanční", Sektory),
        n() == 2 & row_number() == 2 ~ paste("finanční", Sektory)
    )) |>
    group_by(rok, variable, Sektory) |>
    # only get the latest year where it appears in multiple yearbooks
    mutate(latest_rocenka = rocenka_rok == max(as.numeric(rocenka_rok))) |>
    ungroup() |>
    filter(latest_rocenka) |>
    mutate(rok = as.numeric(rok)) |>
    arrange(Sektory2, variable, rok) |>
    mutate(base = first(value), .by = c(Sektory2, variable))

dt_latest <- tribble(
    ~rok, ~variable, ~Sektory2, ~value,
    2022, "plat", "místní", 40376,
    2022, "plat", "ústřední", 47834,
    2022, "plat", "nefinanční podniky", 39689,
    2022, "plat", "finanční instituce", 68578,
    2022, "plat", "Průměrné hrubé měsíční mzdy úhrnem", 38277,
    2023, "plat", "místní", 42583,
    2023, "plat", "ústřední", 50964,
    2023, "plat", "nefinanční podniky", 42831,
    2023, "plat", "finanční instituce", 72975,
    2023, "plat", "Průměrné hrubé měsíční mzdy úhrnem", 43341
) |>
    mutate(rocenka_rok = "9999")

rocenky_lng_ltst <- rocenky_lng |>
    bind_rows(dt_latest)

rocenky_lng_ltst |>
    ungroup() |>
    count(rok)

rocenky_lng |>
    count(Sektory2, rok) |>
    count(n)

rocenky_lng_ltst |>
    count(Sektory2) |>
    arrange(n)

rocenky_lng_ltst |>
    count(rok, sort = TRUE)

dta <- rocenky_lng_ltst |>
    filter(
        variable == "plat",
        Sektory2 %in% c(
            "místní", "ústřední",
            "nefinanční podniky"
        )
    )
dta |>
    ggplot(aes(rok, value, colour = Sektory2, group = Sektory2)) +
    geom_line() +
  scale_color_manual(values = c(`finanční instituce` = "grey10",
                                `nefinanční podniky` = "grey40",
                                `ústřední` = "darkred",
                                `místní` = "goldenrod"), guide = "none") +
  geom_text_repel(data = dta |> filter(rok == max(rok)),
              aes(x = rok, label = Sektory2),
            hjust = "outward", nudge_x = .5) +
  geom_point(data = dta |> filter(rok == max(rok))) +
  ptrr::theme_ptrr("both") +
  scale_y_number_cz() +
  scale_x_continuous(expand = expansion(add = c(0, 6)),
                     breaks = seq(from = 2005, to = 2023, by = 3)) +
  labs(title = "Průměrná měsíční mzda podle sektorů",
       subtitle = "Vybrané sektory",
       caption = "Zdroj: ČSÚ (VDB, statistické ročenky ČR)")


rocenky_lng_ltst |>
    filter(
        variable == "pocet",
        Sektory2 %in% c(
            "místní", "ústřední", "nefinanční veřejné",
            "nefinanční soukromé pod zahraniční kontrolou",
            "nefinanční soukromé národní", "finanční instituce"
        )
    ) |>
    mutate(Sektory2 = as.factor(Sektory2) |>
        fct_relevel(
            "finanční instituce", "nefinanční soukromé národní",
            "nefinanční soukromé pod zahraniční kontrolou"
        )) |>
    ggplot(aes(rok, value, fill = Sektory2, group = Sektory2)) +
    geom_area()
