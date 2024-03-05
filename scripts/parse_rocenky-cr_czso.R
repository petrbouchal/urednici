library(rvest)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)


links_html <- rvest::read_html("https://www.czso.cz/csu/czso/statisticka-rocenka-ceske-republiky-2023")
links_urls <- tibble::tibble(
  url = links_html |>
    html_elements("ul.archiv li a") |>
    html_attr("href"),
  year = links_html |>
    html_elements("ul.archiv li a") |>
    html_text2()
)

get_trh_url <- function(url) {
  html <- read_html(paste0("https://www.czso.cz/", url))
  urls <- html |>
    rvest::html_elements(css = "table.prilohy-publikace td span.odkaz-style-wrapper a") |>
    rvest::html_attr("href")
  urls[grepl("(/csu/czso/10)|(1000$)", urls)]
}

trh_urls <- links_urls |>
  mutate(url_trh = map_chr(url, get_trh_url))

get_10a3_xlsx <- function(url) {
  elmnts <- read_html(paste0("https://www.czso.cz/", url)) |>
    html_element("table.prilohy-publikace")

  itms <- tibble(
    nazev = elmnts |> html_elements("td.nazev") |> html_text2(),
    url = elmnts |> html_elements("td.odkazy a") |> html_attr("href"))

  rslt <- itms |>
    filter(str_detect(nazev, "Zaměstnanci a jejich průměrné hrubé měsíční mzdy v národním hospodářství podle sektorů")) |>
    pull(url)

  rslt
}

trh_xlsx_urls <- trh_urls |>
  mutate(xls_url = map_chr(url_trh, get_10a3_xlsx))

trh_xlsx_urls2 <- trh_xlsx_urls |>
  mutate(ext = str_extract(xls_url, "\\.xlsx?"),
         filename = paste0("data-input/csu-rocenky/rocenka", year, ".", ext))

trh_xlsx_urls$xls_url
trh_xlsx_urls2$ext

download.file(trh_xlsx_urls$xls_url, trh_xlsx_urls2$filename)

filenames <- trh_xlsx_urls2$filename
names(filenames) <- trh_xlsx_urls2$year

rocenky_trhprace_data <- map_dfr(filenames, .id = "rocenka_rok",
                                 ~readxl::read_excel(.x, skip = 4, col_types = "text"))

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
  mutate(Sektory2 = case_when(n() == 1 ~ Sektory,
                              n() == 2 & row_number() == 1 ~ paste("nefinanční", Sektory),
                              n() == 2 & row_number() == 2 ~ paste("finanční", Sektory))) |>
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
  2023, "plat", "místní", 40426,
  2023, "plat", "ústřední", 49529,
  2023, "plat", "nefinanční podniky", 42159,
  2023, "plat", "finanční instituce", 73646,
  2023, "plat", "Průměrné hrubé měsíční mzdy úhrnem", 42427) |>
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

rocenky_lng_ltst |>
  filter(variable == "plat",
         Sektory2 %in% c("místní", "ústřední",
                         "nefinanční podniky", "finanční instituce")) |>
  ggplot(aes(rok, value, colour = Sektory2, group = Sektory2)) +
  geom_line()

rocenky_lng_ltst |>
  filter(variable == "pocet",
         Sektory2 %in% c("místní", "ústřední", "nefinanční veřejné",
                         "nefinanční soukromé pod zahraniční kontrolou",
                         "nefinanční soukromé národní", "finanční instituce")) |>
  mutate(Sektory2 = as.factor(Sektory2) |>
           fct_relevel("finanční instituce", "nefinanční soukromé národní",
                       "nefinanční soukromé pod zahraniční kontrolou")) |>
  ggplot(aes(rok, value, fill = Sektory2, group = Sektory2)) +
  geom_area()


