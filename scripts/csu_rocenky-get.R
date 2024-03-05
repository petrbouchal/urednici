library(rvest)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(ggplot2)
library(readxl)
library(httr)


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
         filename = paste0("data-input/csu-rocenky/rocenka", year, ext))

trh_xlsx_urls$xls_url
trh_xlsx_urls2$ext

map2(trh_xlsx_urls$xls_url, trh_xlsx_urls2$filename, download.file, cacheOK = FALSE, mode = "w", method = "libcurl")

download_excel <- function(url, filename) {
  httr::GET(url) |>
    httr::content() |>
    writeBin(filename)
}

walk2(trh_xlsx_urls$xls_url, trh_xlsx_urls2$filename, download_excel)



