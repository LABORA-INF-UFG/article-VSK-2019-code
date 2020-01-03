source("./util.R")

library(e1071)
library(MASS)
library(ADGofTest)
library(fitdistrplus)
library(markovchain)
library(xtable)

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/points-per-tile-twittmap.csv"
twitter.points = read.table(file=path, sep=",", dec=".", header=F)

head(twitter.points, 10)
points <- subset(twitter.points, twitter.points[,2] != 0)
points <- points[order(points[,1]),]
head(points, 100)
nrow(points)

file.path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/google-tiles-info/src/data/tiles-to-call.csv"

cat("",file=file.path,append=FALSE)

for (i in 1:nrow(points)) {
  tile <- as.character(points[i,1])
  cat(paste(tile, collapse="\n"), file=file.path, append=TRUE, sep="\n")
}

populars <- points[points[,2] > max(points[,2])/2,]

for (i in 1:nrow(populars)) {
  row <- populars[i,]
  tile <- as.character(row[,1])
  print(tile)
  t.x <- unlist(strsplit(tile, "/", fixed=T))[2]
  t.y <- unlist(strsplit(tile, "/", fixed=T))[3]
  t.z <- unlist(strsplit(tile, "/", fixed=T))[1]
  print(paste("http://mt1.google.com/vt/x=", t.x, "&y=", t.y, "&z=", t.z, sep=""))
}

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/bigest-cities-usa2.csv"
important.cities = read.table(file=path, sep="\t", dec=",", header=F)

head(important.cities, 295)


path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/points-per-tile-popular.csv"
points.popular = read.table(file=path, sep=";", dec=".", header=F, quote="")

head(points.popular)

library("jsonlite")

tiles.to.look <- data.frame(tile=c(), number.of.points=c())

for (i in 1:nrow(points.popular)) {
  cluster.info <- fromJSON(as.character(points.popular[i, 2]))
  number.of.points <- cluster.info$`0`$count
  geometry <- cluster.info$`0`$stats$hull
  print(i)
  if (length(geometry) > 0 && grepl("POLYGON", geometry)) {
    tiles.to.look <- rbind(tiles.to.look, data.frame(tile=points.popular[i, 1], 
                                                     number.of.points = cluster.info$`0`$count))
  }
}

head(tiles.to.look)
nrow(tiles.to.look)

file.path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/points-per-tile-polygons.csv"

cat("",file=file.path,append=FALSE)

for (i in 1:nrow(tiles.to.look)) {
  tile.name <- tiles.to.look[i, 1]
  number.of.points <- tiles.to.look[i, 2]
  print(i)
  
  cat(paste(tile.name, number.of.points, sep=",", collapse="\n"), file=file.path, append=TRUE, sep="\n")
}

for (i in 1:nrow(tiles.to.look)) {
  cluster.info <- fromJSON(as.character(points.popular[i, 2]))
  number.of.points <- cluster.info$`0`$count
  
  if (number.of.points == 1) {
    print(paste(points.popular[i, 1], points.popular[i, 2], sep=","))
  }
}

path = "/media/arquivos/Dropbox/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/points-per-tile-polygons.csv"
points.per.polygon = read.table(file=path, sep=",", dec=".", header=F, quote="")

sum(points.per.polygon[,2])