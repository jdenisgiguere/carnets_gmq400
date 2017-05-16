## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(rgdal)
require(readr)
require(rgeos)
require(jsonlite)
require(sp)
require(maptools)
require(spdep)
require(plotly)
require(knitr)

## ------------------------------------------------------------------------
latitude.from.llstring <- function (ll.string) {
  latitude <- as.numeric(unlist(strsplit(ll.string, ","))[1])
  return(latitude)
}

## ------------------------------------------------------------------------
longitude.from.llstring <- function (ll.string) {
  latitude <- as.numeric(unlist(strsplit(ll.string, ","))[2])
  return(latitude)
}

## ------------------------------------------------------------------------
locale.sherbrooke <- locale(date_names = "fr", tz = "EST", encoding = "UTF-8")
oeuvre.json.string <- read_file("../geodata/oeuvres.json", locale.sherbrooke)
oeuvres.from.json <- fromJSON(oeuvre.json.string, simplifyVector = TRUE)

## ------------------------------------------------------------------------
oeuvres.from.json["latitude"] <- apply(oeuvres.from.json["location"], 1, latitude.from.llstring)
oeuvres.from.json["longitude"] <- apply(oeuvres.from.json["location"], 1, longitude.from.llstring)

## ------------------------------------------------------------------------
coordinates(oeuvres.from.json) <- ~ longitude + latitude

## ------------------------------------------------------------------------
wgs84 <-  CRS("+init=epsg:4326")
proj4string(oeuvres.from.json) <- wgs84

## ------------------------------------------------------------------------
scopq.7 <- CRS("+init=epsg:2949")
oeuvres.from.json <- spTransform(oeuvres.from.json, scopq.7 )

## ------------------------------------------------------------------------
ggplot(data=oeuvres.from.json@data) + geom_point(aes(x=coordinates(oeuvres.from.json)[,1], 
                                                     y=coordinates(oeuvres.from.json)[,2],
                                                     color=medium))

## ------------------------------------------------------------------------
arrondissements <- readOGR("../geodata/DonneesSherbrooke_2013", "Arrondissement")
arrondissements@data$id <- rownames(arrondissements@data)
arrondissements.fort <- fortify(arrondissements, region='id') 
arrondissements.fort <- arrondissements.fort[order(arrondissements.fort$order),]

## ------------------------------------------------------------------------
ggplot.oeuvre <- ggplot(data=arrondissements.fort) + geom_polygon(aes(long, lat, group=group), colour='black', fill='white') + geom_point(data=oeuvres.from.json@data, aes(x=coordinates(oeuvres.from.json)[,1], 
                                                     y=coordinates(oeuvres.from.json)[,2],
                                                     color=medium))


## ------------------------------------------------------------------------
ggplotly(ggplot.oeuvre)

## ------------------------------------------------------------------------
fleurimont <- arrondissements[arrondissements$NOM == "Arrondissement de Fleurimont",]
in.fleurimont <- gIntersects(oeuvres.from.json, fleurimont, byid = TRUE)
oeuvres.fleurimont <- oeuvres.from.json[as.logical(in.fleurimont),]

## ----echo=FALSE, results='hide'------------------------------------------
purl("traitement_oeuvres.Rmd")

