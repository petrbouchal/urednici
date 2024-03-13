infl_index <- infl_yony |>
  mutate(index = (1 + infl))

czso_pmz_nace_annual_real <- czso_pmz_nace_clean_annual |>
  mutate(rok = year(tm)) |>
  left_join(infl_index) |>
  mutate(mzda_ch2016 = hodnota/inflace_index*100)

czso_pmz_nace_annual_real |>
  ggplot(aes(x = tm)) +
  geom_line(aes(y = hodnota), colour = "black") +
  geom_line(aes(y = mzda_ch2016), colour = "red") +
  facet_wrap(~odvetvi_txt)

czso_pmz_nace_annual_real |>
  ungroup() |>
  left_join(czso_pmz_nace_annual_real |>
              filter(tm == "2019-01-01") |>
              select(odvetvi_txt, mzda_start = mzda_ch2016)) |>
  replace_na(list(odvetvi_txt = "Všechna odvětví")) |>
  filter(tm == "2023-01-01") |>
  mutate(zmena_real = (mzda_ch2016 - mzda_start)/mzda_start,
         odvetvi_txt = as.factor(odvetvi_txt) |> fct_reorder(-zmena_real)) |>
  select(tm, odvetvi_txt, zmena_real) |>
  ggplot(aes(zmena_real, odvetvi_txt)) +
  geom_col()
