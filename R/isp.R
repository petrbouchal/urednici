process_isp <- function(isp_raw, ovm) {
  isp_raw |>
    distinct() |>
    mutate(ico = str_pad(AA0017, 8, pad = "0")) |>
    rename(pocet_fte_prum = AA0114, platy_vse = AA0118) |>
    left_join(ovm |> select(ico, nazev_ovm), by = "ico") |>
    mutate(plat_prumer = platy_vse/pocet_fte_prum/12)
}
