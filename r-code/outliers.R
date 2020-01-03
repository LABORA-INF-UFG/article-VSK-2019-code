path = "/media/arquivos/Dropbox/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/ZoomLevelPerUser.csv"
zoomPerUser = read.table(path, sep=",", dec=".", header=FALSE)

user.names <- as.vector(zoomPerUser[[1]])
zoomsAccessed <- zoomPerUser[[2]]
zoomsAccessedTimes <- zoomPerUser[[3]]

count <- 1
filter.acesses.byzoom <- function(usersArg, zoom, result=data.frame()) {
  #result <- data.frame(users=c(), times=c())
  print(class(usersArg[count]))
  for (z in zoomsAccessed) {
    if (z == zoom) {
      result <- data.frame(users=c(result$users, usersArg[count]),
                           times=c(result$times, zoomsAccessedTimes[count]),
                           stringsAsFactors=FALSE)
    }
    count <- count + 1
  }
  result
}

by.zoom <- filter.acesses.byzoom(user.names, 11)

by.zoom
summary(by.zoom)
quantile(by.zoom[[2]], 0.95)

count <- 1
filter.acesses.byuser <- function(usersArg, user, result=data.frame()) {
  #result <- data.frame(users=c(), times=c())
  for (u in usersArg) {
    if (u == user) {
      result <- data.frame(zooms=c(result$zooms, zoomsAccessed[count]),
                           times=c(result$times, zoomsAccessedTimes[count]),
                           stringsAsFactors=FALSE)
    }
    count <- count + 1
  }
  result
}

unique.users <- levels(zoomPerUser[[1]])

for (user.name in unique.users) {
  by.user <- filter.acesses.byuser(user.names, user.name)  
  #summary(by.user)
  png(paste("/media/arquivos/Dropbox/mestrado/4-semestre/r-scripts/estudo/charts/", user.name, ".png", sep=""))
  plot(by.user, type="h", main=user.name)
  dev.off()
}

path = "/media/arquivos/Dropbox/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/ZoomLevelPerUserAndPerAccess.csv"
zoomPerUserPerAccess = read.table(path, sep=",", dec=".", header=FALSE)

user.names <- as.vector(zoomPerUserPerAccess[[1]])
accessOrder <- zoomPerUserPerAccess[[2]]
zoomsAccessed <- zoomPerUserPerAccess[[3]]
zoomsAccessedTimes <- zoomPerUserPerAccess[[4]]

count <- 1
filter.acesses.byuserAndOrder <- function(usersArg, user, order, result=data.frame()) {
  #result <- data.frame(users=c(), times=c())
  for (u in usersArg) {
    if (u == user && accessOrder[count] == order) {
      result <- data.frame(zooms=c(result$zooms, zoomsAccessed[count]),
                           times=c(result$times, zoomsAccessedTimes[count]),
                           stringsAsFactors=FALSE)
    }
    count <- count + 1
  }
  result
}

unique.users <- levels(zoomPerUserPerAccess[[1]])

user.name = "usernum.234"

for (order in 46:86) {
  by.user <- filter.acesses.byuserAndOrder(user.names, user.name, order)  
  #summary(by.user)
  png(paste("/media/arquivos/Dropbox/mestrado/4-semestre/r-scripts/estudo/charts/", user.name, "-" , order, ".png", sep=""))
  plot(by.user, type="h", main=paste(user.name, "-", order, sep=""))
  dev.off()
}
