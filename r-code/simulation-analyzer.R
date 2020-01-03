source("/media/arquivos/pcloud/mestrado/4-semestre/r-scripts/util.R")

random.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/random-tiles.log"
random.path2 = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/random-tiles-2.log"
model.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/model-tiles.log"
guan.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/guan-tiles.log"

coordinates.random.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/random-coordinates.log"
coordinates.random.path2 = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/random-coordinates-2.log"
coordinates.model.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/model-coordinates.log"
coordinates.guan.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/guan-coordinates.log"

generateCoordinatesList(random.path, coordinates.random.path)
generateCoordinatesList(random.path2, coordinates.random.path2)
generateCoordinatesList(model.path, coordinates.model.path)
generateCoordinatesList(guan.path, coordinates.guan.path)

generateCoordinatesList <- function(tests.file, coord.file) {
  cat("latitude,longitude,zoom", file=coord.file, append=FALSE, sep="")
  tests.results = scan(file=tests.file, what=character())
  
  for (i in 1:length(tests.results)) {
    tile <- as.character(tests.results[i])
    tile.parts <- strsplit(tile, "/", fixed=T)
    
    x <- as.numeric(tile.parts[[1]][1])
    y <- as.numeric(tile.parts[[1]][2])
    z <- as.numeric(tile.parts[[1]][3])
    
    if (z >= 13) {
      lon <- ((x/(2^z)) * 360) - 180
      lat <- atan(sinh(pi - (y/(2^z)) * 2*pi)) * (180/pi)
      
      cat(paste(round(lat, 8) ,round(lon, 8), z, sep=","), file=coord.file, append=TRUE, sep="\n")
    }
  }
}
