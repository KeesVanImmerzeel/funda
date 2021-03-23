setwd("C:/Users/NLCIMM/OneDrive/myR_on_GitHub/packages/funda/data-raw/Example2")

library(dplyr)
library(funda)

## code to prepare `DATASET` dataset goes here
gemeenten <- read.csv(file = 'Gemeenten alfabetisch 2020.txt')
gemeenten <- gemeenten[,1]
house_htmls <- fd_create_house_htmls(gemeenten)
pattern <-
      "puntstuk|bron|waterbron|beregeningsbron|put|waterput|welput"
df <- data.frame(
      gemeente=fd_gemeenten(house_htmls),
      adres=fd_addresses(house_htmls),
      perceel_opp=fd_plot_area(house_htmls),
      type_bouw=fd_type(house_htmls),
      beschrijving=fd_description(house_htmls),
      puntstuk=fd_descr_match(house_htmls, pattern = "puntstuk|bron|waterbron|beregeningsbron|put|waterput|welput")
)
saveRDS(df,".rds")
df <- readRDS("data-raw/df.rds")

usethis::use_data(DATASET, overwrite = TRUE)
