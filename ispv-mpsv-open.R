library(dplyr)

rscp <- jsonlite::fromJSON("https://data.mpsv.cz/od/soubory/regionalni-statistika-ceny-prace/regionalni-statistika-ceny-prace.json")$polozky
rscp |> filter(obdobi == "" & prumernyVydelek != 0)

char <- jsonlite::fromJSON("https://data.mpsv.cz/od/soubory/ispv-charakteristika/ispv-charakteristika.json")$polozky

zam <- jsonlite::fromJSON("https://data.mpsv.cz/od/soubory/ispv-zamestnani/ispv-zamestnani.json")$polozky
zam$obdobi |> table()

zam |> filter(obdobi == "" & prumernyVydelek != 0)
