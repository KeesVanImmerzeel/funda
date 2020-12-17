# Schat:
# - de som van de van kleine (< 10 m3/u) particuliere grondwateronttrekkingen in het WSIJ (Mm3/jaar);
#      als een jaargift van 50 mm wordt verondersteld;
# - het aantal kleine (< 10 m3/u) particuliere grondwateronttrekkingen in het WSIJ;
# Bepaal de onzekerheid in deze cijfers.

setwd("P:/410/375781/2. Do Work/Rapportage/Kleine particuliere grondwateronttrekkingen")

gemeenten <- c("Aalten","Arnhem","Doesburg","Doetinchem","Duiven", "Lochem","Rheden","Rozendaal", "Westervoort", "Winterswijk",
               "Zevenaar", "Zutphen", "Oude IJsselstreek", "Oost Gelre", "Berkelland", "Bronckhorst", "Montferland")
opp_WRIJ <- 194904 # Oppervlak WRIJ (ha)
opp_gemeenten <- 185196 # Oppervlak geselecteerde gemeenten in WRIJ (ha)
# f = Vermenigvuldigingsfactor van de eindresultaten per gemeenten.
#     voor de vertaling' van deze resultaten naar het oppervlak van het WRIJ.
f <- opp_WRIJ / opp_gemeenten 
# Voorraad koopwoningen per gemeente (Statistisch zakboek gelderland, 2019).
voorraad_koopwoningen <- readRDS("voorraad_koopwoningen.rds")

library(funda)
library(dplyr)
library(magrittr)
library(boot)
library(leaflet)
library(ggmap)

################################################################################################
# Haal informatie over puntstukken van funda.nl en zet deze in een data frame 'df'.
house_htmls <- fd_create_house_htmls(gemeenten)
pattern <-
  "puntstuk|bron|waterbron|beregeningsbron|put|waterput|welput"
df <- data.frame(
  gemeente=fd_gemeenten(house_htmls),
  adres=fd_addresses(house_htmls),
  perceel_opp=fd_plot_area(house_htmls), # [m2]
  type_bouw=fd_type(house_htmls),
  beschrijving=fd_description(house_htmls),
  puntstuk=fd_descr_match(house_htmls, pattern = "puntstuk|bron|waterbron|beregeningsbron|put|waterput|welput")
)
#saveRDS(df,"df15-12-2020.rds")
df <- readRDS("df15-12-2020.rds")

# Bekijk welke typen bouw er zijn en filter nieuwbouwhuizen eruit.
df$type_bouw %>% unique()
df %<>% dplyr::filter(type_bouw != "Nieuwbouw" | is.na(type_bouw))

# Selecteer gewenste kolommen.
df %<>% dplyr::select(gemeente,adres,perceel_opp,puntstuk)

# res = overzichtstabel.
res <- voorraad_koopwoningen

# Combineer de informatie uit funda.nl met de informatie over de voorraad koopwoningen.
#df %<>% dplyr::left_join(voorraad_koopwoningen, by="gemeente")

# Aantal huizen per gemeente dat te koop staat op funda 
op_funda <- df %>% 
            dplyr::group_by(gemeente) %>% 
            summarise(op_funda=n())
res %<>% dplyr::left_join(op_funda, by="gemeente") # Voeg 'op_funda' info toe aan overzichtstabel.

# Fractie van de koopwoningen per gemeente dat te koop staat op funda.nl (-).
res %<>% dplyr::mutate(fractie_op_funda=op_funda / voorraad_koop)

# Voeg 'fractie_op_funda' toe aan de tabel met huizen die te koop staan of funda.nl
#df %<>% dplyr::left_join(res$fractie_op_funda, by="gemeente")

# Bepaal van de huizen die te koop staan op funda.nl per gemeente:
#    - het totaal aantal puntstukken [-].
ps_funda <- df %>% 
            dplyr::group_by(gemeente) %>% 
            summarise(ps_funda=sum(puntstuk))
res %<>% dplyr::left_join(ps_funda, by="gemeente") # Voeg 'ps_funda' info toe aan overzichtstabel.
#df %<>% dplyr::left_join(ps_funda, by="gemeente")

# Bepaal per gemeente:
#    - het aantal geschatte puntstukken.
res %<>% dplyr::mutate(ps_geschat=ps_funda/fractie_op_funda)
res$ps_geschat %<>% round(0)

# Bepaal van de huizen die te koop staan op funda.nl per gemeente:
#    - Het gemiddelde oppervlak van de percelen met puntstuk [m2].
gem_opp <- df %>% 
  dplyr::group_by(gemeente) %>% 
  dplyr::filter(puntstuk==TRUE) %>% 
  summarise(gem_opp=mean(perceel_opp))
gem_opp %<>% 
  dplyr::full_join(op_funda, by="gemeente") %>% 
  select(-c(op_funda))
gem_opp$gem_opp[is.na(gem_opp$gem_opp)] <- 0
gem_opp$gem_opp %<>% round(0)
res %<>% dplyr::left_join(gem_opp, by="gemeente") # Voeg 'gem_opp' info toe aan overzichtstabel.

# Bepaal per gemeente:
#    - Het geschatte oppervlak beregend (ha).
res %<>% dplyr::mutate(opp_beregend=ps_geschat * gem_opp / 10^4) %>% 
  mutate(across(opp_beregend, round, 0))

# Bepaal per gemeente:
#    - De totale hoeveelheid opgepompt water voor beregening bij een gift van 50 mm (x1000 m3).
res %<>% dplyr::mutate(gift_50mm=opp_beregend * 50 * 10^4 / (10^6)) %>% 
  mutate(across(gift_50mm, round, 0))

# Kopieer overzichtstabel naar clipboard voor gebruik in excel.
write.table(res,"clipboard-16384",sep="\t",row.names=FALSE)

# Maak totalen.
# giftMm3 = De schatting van de som van de van kleine (< 10 m3/u) particuliere grondwateronttrekkingen 
#           in het WSIJ (Mm3/jaar) als een jaargift van 50 mm wordt verondersteld.
giftMm3 <- f * sum(res$gift_50mm) / 1000 
# ps_totaal = het geschatte aantal kleine (< 10 m3/u) particuliere grondwateronttrekkingen in het WSIJ;
ps_totaal <- f * sum(res$ps_geschat) 
ps_totaal %<>% round(0)
totaal <- data.frame(giftMm3,ps_totaal)

# Kopieer totalen naar clipboard voor gebruik in excel.
write.table(totaal,"clipboard-16384",sep="\t",row.names=FALSE)

#####################################################

# Bepaal de kansverdeling en confidence interval van 
#    - het aantal kleine particuliere puntstukken in het WRIJ.
# Voeg eerst de fractie van de koopwoningen per gemeente dat te koop staat op funda.nl (-) toe aan het data frame 'df'.
tmp <- res %>% dplyr::select(gemeente,fractie_op_funda)
df %<>% dplyr::left_join(tmp,by="gemeente")

sm <- function(formula, data, indices) {
  return(formula(data[indices]))
}
# bootstrapping with 10000 replications.
results <- boot::boot(data=f * df$puntstuk/df$fractie_op_funda, statistic=sm, R=10000, formula=sum)

# view results
results
plot(results)

# get 95% confidence interval
boot::boot.ci(results,type="bca")

#####################################################

# Bepaal de kansverdeling en confidence interval van 
#    - De de som van de van kleine (< 10 m3/u) particuliere grondwateronttrekkingen 
#      in het WSIJ (Mm3/jaar) als een jaargift van 50 mm wordt verondersteld.

# function to obtain R-Squared from the data

# bootstrapping with 10000 replications.
results <- boot(data=f * df$puntstuk * df$perceel_opp * 50 /(df$fractie_op_funda * 10^9), statistic=sm, R=10000, formula=sum)

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results,type="bca")

#####################################################

# Voeg coordinaten toe aan de tabel met huizen die te koop staan op funda.nl en een puntstuk.
df %<>% dplyr::filter(puntstuk == TRUE)
key = "Add-your-key-here"
ggmap::register_google(key)
lon_lat <- df$adres %>% ggmap::geocode()
df %<>% cbind(lon_lat)

# Maak kaart van puntstukken.
lon_lat %>% leaflet::leaflet() %>%  leaflet::addTiles()  %>%  leaflet::addCircleMarkers()

# Vertaal naar epsg projection 28992 - amersfoort / rd new (Spatial points)
xy <- suppressWarnings(lon_lat %>%
                         SpatialPoints(proj4string = CRS("+proj=longlat")) %>%
                         spTransform(CRS("+init=EPSG:28992")))
x <- xy@coords[, 1]
y <- xy@coords[, 2]
df %<>% cbind(x, y)

# Bewaar het data frame met informatie over de lokaties met puntstukken.
saveRDS(df, "df15-12-2020 output puntstukken met coordinaten.rds")

# Maak een shape met informatie over de lokaties met puntstukken.
writeOGR(
  SpatialPointsDataFrame(xy, df),
  ".",
  "df15-12-2020 output puntstukken",
  "ESRI Shapefile"
)
