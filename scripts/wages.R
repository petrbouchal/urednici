wages_later <- czso_get_table("110080", force_redownload = TRUE) %>%
  filter(is.na(POHLAVI_txt), is.na(SPKVANTIL_txt), uzemi_kod %in% c(19, 3018)) %>%
  select(rok, hodnota, uzemi_kod) %>%
  spread(key = uzemi_kod, value = hodnota) %>%
  rename("czsal_all" = 2, "phasal_all" = 3)
