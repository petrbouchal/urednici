library(dplyr)
library(stringr)
library(purrr)
library(httr)
library(readr)
library(readxl)
library(tibble)
library(tidyr)

szu_excel_links <- tribble(~year, ~url,
                 2018, "http://www.psp.cz/sqw/text/orig2.sqw?idd=161089",
                 2017, "http://www.psp.cz/sqw/text/orig2.sqw?idd=135042",
                 2016, "http://www.psp.cz/sqw/text/orig2.sqw?idd=132749",
                 2015, "http://www.psp.cz/sqw/text/orig2.sqw?idd=126409",
                 2014, "http://www.psp.cz/sqw/text/orig2.sqw?idd=114369",
                 2013, "http://www.psp.cz/sqw/text/orig2.sqw?idd=98537",
                 2012, "http://www.psp.cz/sqw/text/orig2.sqw?idd=173826",
                 2011, "http://www.psp.cz/sqw/text/orig2.sqw?idd=88836",
                 2010, "http://www.psp.cz/sqw/text/orig2.sqw?idd=70611") %>%
  mutate(year = as.character(year))

szu_sheet_defs <- szu_excel_links %>%
  filter(year > 2012) %>% # older reports are different and not needed anyway
  pull(url) %>%
  set_names(szu_excel_links$year[szu_excel_links$year > 2012]) %>%
  imap_dfc(function(.x, .y) {
                      fname <- here::here("data-input", str_glue("szu_{.y}.xls"))
                      if(!file.exists(fname)) download.file(.y, fname)
                      shts <- readxl::excel_sheets(fname)
                      shts <- shts[str_detect(shts, "Tab6 ÚSC financování", T)]})

# tt <- tempfile()
# download.file("http://www.psp.cz/sqw/text/orig2.sqw?idd=135042", tt)
# readxl::read_excel(tt, "Tab10-2-jedn.OSS státní správy", skip = 11)

szu_sheet_metadata <- szu_sheet_defs %>%
  pivot_longer(everything(), names_to = "year", values_to = "sheet") %>%
  mutate(relevant = str_detect(sheet, "(Tab10)|(Tab11)|objemy|počty")) %>%
  filter(relevant) %>%
  mutate(pocty = str_detect(sheet, "počty|Tab10"),
         objemy = str_detect(sheet, "objemy|Tab11"),
         type = str_replace(sheet, "-SS", "státní správy") %>%
           str_remove(".*[-][/w]?") %>%
           str_remove("\\(počty\\)|\\(objemy\\)") %>%
           str_trim() %>%
           str_replace("(jedn.OSS )|(OSS-SS )", "jedn. OSS ") %>%
           str_replace("Přísl. a voj.", "Příslušníci a vojáci")) %>%
  left_join(szu_excel_links)

download_and_munge <- function(year, sheet, type, url, objemy) {
  fname <- here::here("data-input", str_glue("szu_{year}.xls"))
  if(!file.exists(fname)) download.file(url, fname)
  e <- readxl::read_excel(fname, sheet, skip = 11) %>%
    set_names(c("kap_num", "kap_name", "org", "last", "rozp", "upraveny", "skutecnost",
                "plneni", "rozdil", "index")) %>%
    mutate(year = year,
           type = type,
           indicator = ifelse(objemy, "cost", "count"))
  return(e)
}

szu_data <- szu_sheet_metadata %>%
  select(year, sheet, type, url, objemy) %>%
  pmap_dfr(download_and_munge)

write_rds(szu_data, here::here("data-interim/objemy_pocty_scraped_raw_2012_2018.rds"))
write_csv(dt, here::here("data-interim/szu_staff_scraped_raw_2012_2018.csv"))

szu_data %>%
  mutate(kap_num = ifelse(kap_num == "C E L K E M", "celkem", kap_num),
         # kap_name = ifelse(kap_num == "celkem", "celkem", kap_name),
         # org = ifelse(kap_num == "celkem", "celkem", org)
         ) %>%
  fill(kap_num) %>%
  filter(year == 2017 & str_detect(type, "jedn.") & indicator == "count") %>% View()
