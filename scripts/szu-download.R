library(rvest)
library(dplyr)
library(readxl)
library(stringr)
library(readr)

szu_lst <- readr::read_csv("data-input/szu_tisky-links.csv")

get_szu_excel <- function(url, filename, year) {
  tisk_html <- read_html(url)

  filename_split <- str_split(filename, ";", simplify = FALSE)
  filename_clean <- str_trim(filename_split[[1]])

  yr <- year

  files_tbl <- tibble(
    name = tisk_html |> html_elements("td > span.file.xls > a") |> html_text2(),
    url = tisk_html |> html_elements("td > span.file.xls > a") |> html_attr("href"),
    year = yr
  )

  print(filename_clean)

  if(!is.na(filename)) rslt <- files_tbl |> filter(name %in% filename_clean) else rslt <- files_tbl

  return(rslt)
}

# get_szu_excel(szu_lst$url[10], NA)
# get_szu_excel(szu_lst$url[10], szu_lst$filename[10])
# get_szu_excel(szu_lst$url[7], szu_lst$filename[7])
# get_szu_excel(szu_lst$url[20], szu_lst$filename[20])

szu_lst_sub <- szu_lst |> select(url, filename, year = rok) |> filter(year > 2009)

szu_xls_urls <- pmap_dfr(szu_lst_sub, .f = get_szu_excel) |>
  mutate(order = row_number(), count = n(), .by = year)

download_szu_excel <- function(name, url, year, dir) {
  outfile <- file.path(dir, paste0(year, "_", name))
  url <- paste0("https://psp.cz", url)
  print(year)
  print(url)
  curl::curl_download(url, outfile)
  rslt <- tibble(url = url, file = outfile, rok = year)
}

szu_excel <- pmap_dfr(szu_xls_urls[,1:3], download_szu_excel, "data-input/szu-psp/")

szu_excel_sheets <- szu_excel |>
  mutate(sheets = map(file, readxl::excel_sheets)) |>
  unnest(cols = sheets)

readr::write_rds(szu_excel_sheets, "data-interim/szu-excel-metadata.rds")

szu_excel_sheets |>
  filter(str_detect(sheets, "([Tt]ab1[01])|(Str\\.)")) |>
  count(rok, file)


