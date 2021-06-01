library(rvest)
library(tidyverse)

stm_url_base <- "https://www.mvcr.cz/sluzba/"
stm_url <- "https://www.mvcr.cz/sluzba/clanek/systemizace-sluzebnich-a-pracovnich-mist.aspx"
dir_out <- "systemizace_files-20210129"

stm_html <- read_html(stm_url)

page_links_abs <- stm_html %>%
  html_elements("div#content ul") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  url_absolute(stm_url_base)

all_links <- map_dfr(page_links_abs, ~{
  page <- read_html(.x)

  source_title <- page %>%
    html_element("div#content h1") %>%
    html_text()

  items <- page %>%
    html_element("div#content ul") %>%
    html_elements("a")

  urls <- items %>%
    html_attr("href") %>%
    url_absolute(stm_url_base)

  titles <- items %>%
    html_text()

  tibble(title = titles, url = urls) %>%
    mutate(source = .x,
           source_title = source_title)
})

all_links$url

all_links_completed <- all_links %>%
  mutate(extension = str_extract(url, "(xlsx)|(pdf)"),
         filename = str_extract(url, "(?<=soubor\\/).+(?=\\.aspx)") %>%
           str_replace("\\-(pdf|xlsx)", "\\.\\1"))


dir.create(dir_out, showWarnings = F, recursive = T)
walk2(all_links_completed$url, file.path(dir_out, all_links_completed$filename),
      ~curl::curl_download(.x, .y))

