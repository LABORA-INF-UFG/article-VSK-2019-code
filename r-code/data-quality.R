source("/media/arquivos/Dropbox/mestrado/4-semestre/r-scripts/util.R")

library(e1071)
library(MASS)
library(ADGofTest)
library(fitdistrplus)
library(markovchain)
library(xtable)
library(fpc)
library(mclust)
library(cluster)

path = paste(PCLOUD_DIR, "/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/NumSessionsPerUser.csv", sep="")
sessions.per.user = read.table(file=path, sep=",", dec=".", header=F)

users.list <- levels(as.factor(sessions.per.user[,1]))

dim(as.array(users.list))

num.sessions.per.user <- data.frame(user=c(), num.sessions=c())

apply(as.array(users.list), MARGIN=1, FUN=function(x) {
  users.sessions <- as.vector(sessions.per.user[sessions.per.user[,1] == x,1])
  num.sessions.per.user <<- rbind(num.sessions.per.user, data.frame(user=x, num.sessions=length(users.sessions)))
});

sum(num.sessions.per.user[,2])

plot(density(num.sessions.per.user[,2]))

path = paste(PCLOUD_DIR, "/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/NumSessionsPerUserJustRoadMap.csv", sep="")
sessions.per.user = read.table(file=path, sep=",", dec=".", header=F)

users.list <- levels(as.factor(sessions.per.user[,1]))

dim(as.array(users.list))

num.sessions.per.user <- data.frame(user=c(), num.sessions=c())

apply(as.array(users.list), MARGIN=1, FUN=function(x) {
  users.sessions <- as.vector(sessions.per.user[sessions.per.user[,1] == x,1])
  num.sessions.per.user <<- rbind(num.sessions.per.user, data.frame(user=x, num.sessions=length(users.sessions)))
});

sum(num.sessions.per.user[,2])

sd(num.sessions.per.user[,2])

quantile(num.sessions.per.user[,2], 0.9)

plot(density(num.sessions.per.user[,2]))

cluster1 <- kmeans(num.sessions.per.user[,2], 3)$cluster1

sd(num.sessions.per.user[cluster1 == 1,][,2])

plot(density(num.sessions.per.user[cluster1 == 1,][,2]))

dim(num.sessions.per.user[cluster1 == 2,])

(cluster1 <- pamk(data=num.sessions.per.user[,2]))
cluster1 <- cluster1$pamobject$clustering

clusplot(num.sessions.per.user, cluster1, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

80^2 + 2*90^2 + 3*100^2 + 2*110^2 + 120^2

(20^2 + 2*10^2 + 2*10^2 + 20^2)

(20^2 + 20^2)/3


path = paste(PCLOUD_DIR, "/mestrado/publicacao/peval16/peval16/vinicius/r-code/data/collected/session-dates.csv", sep="")
session.dates = read.table(file=path, sep=";", header=F)
head(session.dates)

barplot(table(session.dates[,2]))
bp = barplot(table(session.dates[,3]), xaxt="n", xlab="Hour", ylab="Frequency")
axis(1, at=bp, labels=0:23, las=2)

barplot(table(session.dates[,3]))
