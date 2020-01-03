library(maptools)

important.cities = new.env()

path = paste(PCLOUD_DIR, "mestrado/4-semestre/r-scripts/mestrado/dados-espaciais/cities.shp", sep='/');path
cities.shape <- readShapeSpatial(path)

nrow(cities.shape)
cities.shape[5,11]
class(cities.shape)
cities.shape$POP2007[cities.shape$NAME == "Los Angeles"]

sum(cities.shape$POP2007)

#max(cities.shape)

head(as.data.frame(cities.shape), 1)

as.data.frame(cities.shape)

path = paste(PCLOUD_DIR, 'mestrado/4-semestre/gogeo/TIGER-county-form.csv', sep='/');path
cities <- as.data.frame(read.table(path, sep=";", dec=".", header=TRUE))
cities$the_geom = c()
nrow(cities)
subset(cities, cities$name == "New York")
head(cities$name, 2)

important.cities$generate.places = function() {
  path = paste(PCLOUD_DIR, 'mestrado/4-semestre/gogeo/places/places.shp', sep='/');path
  places.shape <- readShapeSpatial(path)
  nrow(places.shape)
  places.shape = subset(places.shape, places.shape$CLASS == "City")
  max(places.shape$POP2000)
  places.shape
}

head(places.shape, 5)

head(places.shape, 10)

library(foreign)

read.dbf()
cb.2014 <- read.dbf("/media/arquivos/pcloud/mestrado/4-semestre/gogeo/tl_2014_36_place/tl_2014_36_place.dbf")
head(cb.2014, 10)
subset(cb.2014$"polygons", cb.2014$NAME == "New York")
