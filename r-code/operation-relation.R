QUANTILE <- 0.95

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/ZoomLevelPerUser.csv"
zoomPerUser = read.table(path, sep=",", dec=".", header=FALSE)

user.names <- as.vector(zoomPerUser[[1]])
zoomsAccessed <- zoomPerUser[[2]]
zoomsAccessedTimes <- zoomPerUser[[3]]

head(zoomPerUser)

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

black.list <- c()

for (z in 1:21) {
  by.zoom <- filter.acesses.byzoom(user.names, z)
  quant <- quantile(by.zoom[[2]], QUANTILE)
  indexes <- which(by.zoom[[2]] > quant)
  print(indexes)
  for (index in indexes) {
    black.list <- c(black.list, by.zoom[[index, 1]])
  }
}

black.list

path = paste(PCLOUD_DIR, "/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/OperationsFrequency.csv", sep="")
operacoes = read.table(path, sep=" ", dec=".", header=FALSE)
head(operacoes)

util$plot.eps("operations-frequency", margin=c(3.6, 3.5, 0.6, 0.3))
bp <- barplot(operacoes[[2]]/sum(operacoes[[2]]), ylim=c(0, 0.5), names.arg=operacoes[[1]], xlab="Operation", ylab="% of total",
        col=c("red"))
text(bp, 0, round(operacoes[[2]]/sum(operacoes[[2]]), 2),cex=1,pos=3)
dev.off()

path = paste(PCLOUD_DIR, "/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/OperationsFrequencyPerZoomLevel.csv", sep="")
operacoes = read.table(path, sep=",", dec=".", header=FALSE)

head(operacoes)
nrow(subset(operacoes, operacoes[,1] == 2))

op.matrix <- matrix(nrow=3, ncol=21, dimnames=list(c("pan", "zoom_in", "zoom_out"), 1:21))
(op.matrix <- apply(op.matrix, c(1, 2), function(x) 0))
apply(op.matrix, c(2), function(x) if (sum(x) == 0) {x} else {x/sum(x)})

length(which(rownames(op.matrix) == "Arraste", arr.ind = TRUE))

n.zooms.use <- matrix(nrow=1, ncol=21, dimnames=list(c(), 1:21))
(n.zooms.use <- apply(n.zooms.use, c(1, 2), function(x) 0))

for (i in 1:max(operacoes[,1])) {
  sub <- subset(operacoes, operacoes[,1] == i)
  if (nrow(sub) <= 10) {
    next
  }
  
  op.matrix.partial <- matrix(nrow=3, ncol=21, dimnames=list(c("pan", "zoom_in", "zoom_out"), 1:21))
  op.matrix.partial <- apply(op.matrix.partial, c(1, 2), function(x) 0)
  
  as.op <- FALSE
  for (j in 1:nrow(sub)) {
    op.name <- sub[j,3]
    
    op.id <- which(rownames(op.matrix) == op.name, arr.ind = TRUE)
    z.id <- sub[j,2]
    
    if (length(op.id) == 0 || is.na(op.id)) next
    as.op <- TRUE
    op.matrix.partial[op.id, z.id] <- op.matrix.partial[op.id, z.id] + 1
  }
  
  op.matrix.partial <- apply(op.matrix.partial, c(2), function(x) if (sum(x) <= 1) {c(0, 0, 0)} else {x})
  
  if (!as.op) {
    next
  }
  
  op.matrix.partial <- apply(op.matrix.partial, c(2), function(x) if (sum(x) == 0) {x} else {x/sum(x)})
  n.zooms.use <- n.zooms.use + apply(op.matrix.partial, c(2), function(x) if (sum(x) != 0) {1} else {0})
  op.matrix <- op.matrix + op.matrix.partial  
}
n.zooms.use
op.matrix
op.matrix.freq <- apply(op.matrix, c(1), function(x) x/n.zooms.use)

op.matrix.freq[1,] <- c(0, 1, 0)

l.max <- 21
# lambda.arg <- 0.5
# omega.arg <- 0.4
lambda.arg <- guan$lambda
omega.arg <- guan$w

zoomIn <- function(l, lambda) {
  1 - (l/l.max)^lambda
}

zoomOut <- function(l, w, lambda) {
  w * (l/l.max)^lambda
}

pan <- function(l, w, lambda) {
  (1 - w) * (l/l.max)^lambda
}

zoom.in.val <- c()
zoom.out.val <- c()
pan.val <- c()

for (l in 0:21) {
  zoom.in.val <- c(zoom.in.val, zoomIn(l, lambda.arg))
  zoom.out.val <- c(zoom.out.val, zoomOut(l, omega.arg, lambda.arg))
  pan.val <- c(pan.val, pan(l, omega.arg, lambda.arg))
  #lambda.arg <- lambda.arg + 0.15
}

util$plot.eps("operation-access-per-zoom-level")
plot(op.matrix.freq[,1], type="n", col=1, ylim=c(0, 1), xlim=c(0, 21), ylab="Frequency", xlab="Zoom level")

lines(x=1:21, y=op.matrix.freq[,1], lw=6, col=2)
lines(x=1:21, y=op.matrix.freq[,2], lw=6, col=3)
lines(x=1:21, y=op.matrix.freq[,3], lw=6, col=4)
lines(x=0:21, y=pan.val, lw=6, lty=2, col=2)
lines(x=0:21, y=zoom.in.val, lw=6, lty=2, col=3)
lines(x=0:21, y=zoom.out.val, lw=6, lty=2, col=4)

legend("topright", legend=c("Pan", "Zoom in", "Zoom out"), fill=c(2, 3, 4))
dev.off()

#############################################################################################
l.max <- 21
lambda.arg <- 3
omega.arg <- 0.9

zoomIn <- function(l, lambda) {
  1 - (l/l.max)^lambda
}

zoomOut <- function(l, w, lambda) {
  w * (l/l.max)^lambda
}

pan <- function(l, w, lambda) {
  (1 - w) * (l/l.max)^lambda
}

d=op.matrix.freq[1:21,1];d
dfr <- data.frame(x=1:21, d);dfr
m = nls(d~((1 - w) * (x/21)^lamb), data=dfr, start=list(w=0.5, lamb=3), trace=TRUE)
lines(1:21, predict(m, list(x = 1:21)), col = "red")
summary(m)

d=op.matrix.freq[1:21,2];d
dfr <- data.frame(x=1:21, d);dfr
m = nls(d~(1 - (x/21)^lamb), data=dfr, start=list(lamb=3), trace=TRUE)
lines(1:21, predict(m, list(x = 1:21)), col = "green")
summary(m)

d=op.matrix.freq[1:21,3];d
dfr <- data.frame(x=1:21, d);dfr
m = nls(d~(w * (x/21)^lamb), data=dfr, start=list(w=0.5, lamb=3), trace=TRUE)
lines(1:21, predict(m, list(x = 1:21)), col = "blue")
summary(m)

#mean of nls above estimates
lambda.arg = (0.73763+0.45764+0.17741)/3
omega.arg = (0.37335+0.48050)/2

guan$lambda.arg = (0.73763+0.45764+0.17741)/3
guan$omega.arg = (0.37335+0.48050)/2

zoom.in.val <- c()
zoom.out.val <- c()
pan.val <- c()

for (l in 0:21) {
  zoom.in.val <- c(zoom.in.val, zoomIn(l, lambda.arg))
  zoom.out.val <- c(zoom.out.val, zoomOut(l, omega.arg, lambda.arg))
  pan.val <- c(pan.val, pan(l, omega.arg, lambda.arg))
  #lambda.arg <- lambda.arg + 0.15
}

util$plot.eps("operations-relation-guan")
plot(op.matrix.freq[,1], type="n", col=1, ylim=c(0, 1), xlim=c(1, 21), ylab="Frequency", xlab="Zoom level")

lines(x=1:21, y=op.matrix.freq[,1], lw=6, col=2)
lines(x=1:21, y=op.matrix.freq[,2], lw=6, col=3)
lines(x=1:21, y=op.matrix.freq[,3], lw=6, col=4)
lines(x=0:21, y=pan.val, lw=6, lty=1, col=2)
lines(x=0:21, y=zoom.in.val, lw=6, lty=2, col=3)
lines(x=0:21, y=zoom.out.val, lw=6, lty=4, col=4)

legend("left", legend=c("Pan", "Zoom in", "Zoom out"), lty=c(1, 2, 4), lw=c(3, 3, 3), col=c(2, 3, 4))
dev.off()

#############################################################################################

users <- as.vector(operacoes[[1]])
zoom.levels <- as.vector(operacoes[[2]])
operations <- as.vector(operacoes[[3]])
frequency <- operacoes[[4]]

zoom.levels <- as.numeric(zoom.levels)
zoom.levels

black.list <- c()

count <- 1
filter.operation <- function(operation, zoom.level, result=c()) {
  for (op in operations) {
    if (op == operation && zoom.levels[count] == zoom.level
        && !(users[count] %in% black.list)) {
      result <- c(result, frequency[count])
    }
    count <- count + 1
  }
  result
}

freqAll <- list()
freqAll.byzoom <- list()

for (z in 1:21) {
  freqAll.byzoom[[as.character(z)]] = c()
  for (op in c("zoom_in", "zoom_out", "pan")) {
    freqAll[[paste(op, z, sep="-")]] = filter.operation(op, z)
    freqAll.byzoom[[as.character(z)]] = c(freqAll.byzoom[[as.character(z)]], freqAll[[paste(op, z, sep="-")]])
  }
}

freqAll.byzoom[["21"]]

func.filter <- function(op, freqAll.relative = data.frame(zoom=c(), freq=c())) {
  for (z in 1:21) {
    op.in.zoom <- sum(freqAll.byzoom[[as.character(z)]])    
    relative <- sum(freqAll[[paste(op, z, sep="-")]]) / op.in.zoom
    zooms <- c(freqAll.relative$zoom, z)
    freqs <- c(freqAll.relative$freq, relative)
    freqAll.relative <- data.frame(zoom=zooms, freq=freqs)
  }
  freqAll.relative
}

zoom_in <- func.filter("zoom_in")
zoom_out <- func.filter("zoom_out")
pan <- func.filter("pan")
search <- func.filter("search")
route <- func.filter("route")

zoom_in

plot(zoom_in, type="n", col=1, ylim=c(0, 1), xlim=c(1, 21), xlab="Frequência", ylab="Nível de zoom")

lines(x=zoom_in$zoom, y=zoom_in$freq, lw=3)
lines(x=zoom_out$zoom, y=zoom_out$freq, col=2, lw=3)
lines(x=pan$zoom, y=pan$freq, col=3, lw=3)
lines(x=search$zoom, y=search$freq, col=4, lw=3)
lines(x=route$zoom, y=route$freq, col=5, lw=3)

legend("topright", legend=c("Zoom-in", "Zoom-out", "Arraste"), fill=c(1, 2, 3))

count <- 1
freq.by.zoom <- function(zoom, result=0) {
  for (z in zoom.levels) {
    op <- operations[count]
    if ((op == "zoom_in" || op == "zoom_out" || op == "pan") 
        && (z == zoom || is.na(z))) {
      result <- result + frequency[count]
    }
    count <- count + 1
  }
  result
}

freq.by.zoom(13)

count <- 1
freq.by.operation <- function(operation, result=0) {
  for (op in operations) {
    if (op == operation) {
      result <- result + frequency[count]
    }
    count <- count + 1
  }
  result
}

freq.by.operation("zoom_in")

count <- 1
filter.by.operation <- function(operation, result = data.frame(zoom = c(1:21), freq = vector(mode="numeric", length=21))) {
  for (op in operations) {
    if (op == operation) {
      freq <- freq.by.zoom(zoom.levels[count])
      result$freq[zoom.levels[count]] <- frequency[count] / freq
      result <- data.frame(zoom=result$zoom,
                           freq=result$freq)
    }
    count <- count + 1
  }
  result
}

ylim <- c(0, 1)
xlim <- c(1, 21)

zoom_in <- filter.by.operation("zoom_in")
zoom_out <- filter.by.operation("zoom_out")
pan <- filter.by.operation("pan")

zoom_in

plot(zoom_in, main="Relation Between Operations", type="n", col=1, ylim=ylim, xlim=xlim)

lines(x=zoom_in$zoom, y=zoom_in$freq)
lines(x=zoom_out$zoom, y=zoom_out$freq, col="red")
lines(x=pan$zoom, y=pan$freq, col="blue", lty=2)

#legend("topright", legend=c("Zoom-in", "Zoom-out", "Pan"), fill=c("black", "red", "blue"))

#plot(zoom_out, main="Zoom Out", type="h", col=2, ylim=ylim)

#plot(pan, main="Pan", type="h", col=3, ylim=ylim)

#plot(zoom_in, main="Zoom In", type="h", col=1, ylim=ylim, xlim=xlim)


############################################################

l.max <- 21
lambda.arg <- 3
omega.arg <- 0.4

zoomIn <- function(l, lambda) {
  1 - (l/l.max)^lambda
}

zoomOut <- function(l, w, lambda) {
  w * (l/l.max)^lambda
}

pan <- function(l, w, lambda) {
  (1 - w) * (l/l.max)^lambda
}

zoom.in.val <- c()
zoom.out.val <- c()
pan.val <- c()

for (l in 1:l.max) {
  zoom.in.val <- c(zoom.in.val, zoomIn(l, lambda.arg))
  zoom.out.val <- c(zoom.out.val, zoomOut(l, omega.arg, lambda.arg))
  pan.val <- c(pan.val, pan(l, omega.arg, lambda.arg))
}

par(mfrow=c(1,1))
plot(0:l.max, type="n", ylim=c(0, 1), xlim=c(0,21))

lines(x=0:l.max, y=zoom.in.val)
lines(x=0:l.max, y=zoom.out.val, col="red")
lines(x=0:l.max, y=pan.val, col="blue", lty=2)
legend("topright", legend = c("Zoom In", "Zoom Out", "Pan"),
       col = c(1, 2, "blue"), pch=c(NA,NA), lty = c(1, 1, lty=2),
       lwd=c(3,3),bty="n", cex=0.9)

zoomIn.func <- function(lambda, result=c()) {
  for (l in 0:l.max) {
    result <- c(result, zoomIn(l,lambda))
  }
  result
}

zoomIn.func(0.5)
zoom_in[[2]]

zoomIn.LL <- function(lambda) {
  -sum(log(zoomIn.func(lambda)))
}

library(stats4)
mle(zoomIn.LL, start=list(lambda=0.3))

