## code to prepare `DATASET` dataset goes here
save(house_htmls, file="data-raw/house_htmls.Rdata")
house_htmls <- load("data-raw/house_htmls.Rdata")
usethis::use_data(DATASET, overwrite = TRUE)
