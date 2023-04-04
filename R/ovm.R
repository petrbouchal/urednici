load_ovm <- function(file) {
  ovm <- jsonlite::read_json(file)
  ovm_df <- ovm$položky |>
    map_dfr(function(x) {
      # print(x$`adresa-sídla`)
      tibble(ico = x$ičo,
             nazev_ovm = x$název$cs,
             id_ds = x$`datové-schránky` |> map_chr(`[[`, "identifikátor-ds"),
             id_ovm = x$identifikátor,
             sidlo_adm = as.character(x$`adresa-sídla`['kód']),
             sidlo_adresa = x$`adresa-sídla-txt`,
             # pracoviste = if("pracoviště-ovm" %in% names(x)) x["pracoviště-ovm"] else list(),
             vnitrni = x$`vnitřní-organizační-jednotka`)
    })
  return(ovm_df)
}
