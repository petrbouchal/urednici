options(renv.config.install.transactional = FALSE)

"colorspace" |>
    renv::install(prompt = FALSE, type = "binary") |>
    renv::record()

renv::restore(prompt = FALSE)

renv::restore(packages = c("ggplot2", "forcats"))

install.packages("eurostat")

httr::GET("https://google.com")

utils::install.packages("classInt")

renv::restore()
