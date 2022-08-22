# Voir dans C:\Users\pascal.irz\Documents\R\R-4.1.1\library\hydroportail\doc les fichiers html

library(hydroportail)
library(sf)
library(mapview)


# régions hydro
data(sf_hydro_regions_met)

ggplot(data = sf_hydro_regions_met,
       aes(fill = CdRegionHy)) +
  geom_sf()

mapviewOptions(fgb = FALSE)
sf::st_is_valid(sf_hydro_regions_met)
mapview::mapview(sf_hydro_regions_met,
                 zcol = "LbRegionHy",
                 legend = FALSE)

# stations et sites hydro
sf_hydro_all <- get_sf_hydro()
class(sf_hydro_all)

sf_hydro_bzh <- get_sf_hydro(code = "J")

# a sepcific site with its stations
get_sf_hydro(code = "J001401001")

# météo
sf_meteo_29 <- get_sf_meteo(code = "29")
sf_meteo_22 <- get_sf_meteo(code = "22")
sf_meteo_35 <- get_sf_meteo(code = "35")
sf_meteo_56 <- get_sf_meteo(code = "56")

sf_meteo_bzh <- rbind(sf_meteo_29, sf_meteo_22, sf_meteo_35, sf_meteo_56)

mapview(sf_meteo_bzh)
