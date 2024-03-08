library(czso)

df_infl <- czso_get_table("010022", dest_dir = "data-input/czso", force_redownload = T) %>%
  filter(is.na(ucel_txt)) %>%
  filter(casz_txt == "stejné období předchozího roku") %>%
  group_by(rok) %>%
  summarise(hodnota = mean(hodnota)) %>%
  select(contains("obdobi"),rok, hodnota) %>%
  mutate(inflation=hodnota/100)%>%
  arrange(rok)%>%
  filter(rok>=2003,rok<=2022)

df_infl$base_2003 <- 0
df_infl$base_2022 <- 0
df_infl[1, "base_2003"] <- 1
df_infl[nrow(df_infl), "base_2022"] <- 1


for (i in 2:nrow(df_infl)) {
  df_infl[i, "base_2003"] <- df_infl[i - 1, "base_2003"] * df_infl[i, "inflation"]
}


for (i in (nrow(df_infl) - 1):1) {
  df_infl[i, "base_2022"] <- df_infl[i + 1, "base_2022"] * df_infl[i+1, "inflation"]
}

df_infl
