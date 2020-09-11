## code to prepare `covid` dataset goes here

covid <- readr::read_rds("data-raw/covid.rds")
usethis::use_data(covid, overwrite = TRUE)

