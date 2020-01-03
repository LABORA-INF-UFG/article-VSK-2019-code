util$pkgRequire("maptools")
util$pkgRequire("actuar")
util$pkgRequire("EnvStats")
util$pkgRequire("doParallel")
library(doParallel)

guan = new.env()

guan$simulated.durations.guan <- data.frame(duration=c(), num.operations=c())
guan$simulated.zooms.guan <- data.frame(last=c(), current=c())
guan$simulated.zooms.array.guan <- c()
guan$simulated.pans.guan <- data.frame(x=c(), y=c())

cat(paste(guan$simulated.durations.guan[,1], collapse="\n"), file=paste(SIMULATED_DATA_PATH, "/guan-durations.csv", sep=""), append=T, sep="\n")
cat(paste(guan$simulated.durations.guan[,2], collapse="\n"), file=paste(SIMULATED_DATA_PATH, "/guan-number-of-actions.csv", sep=""), append=T, sep="\n")

#cat(paste(guan$simulated.durations.guan[,1], collapse="\n"), file=paste(SIMULATED_DATA_PATH, "/guan-durations-10min.csv", sep=""), append=T, sep="\n")
#cat(paste(guan$simulated.durations.guan[,2], collapse="\n"), file=paste(SIMULATED_DATA_PATH, "/guan-number-of-actions-10min.csv", sep=""), append=T, sep="\n")

max.zoom = 21
l.max = 21
PAN.SIZE = 256

#file.path = "/media/arquivos/Dropbox/mestrado/4-semestre/scala-scripts/data/guan-actions4.csv"
#file.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/data/guan-actions.csv"
file.path = paste(SIMULATED_DATA_PATH, "/guan-actions-1.csv", sep="")

#cat("",file=file.path,append=FALSE)

guan$choose.resolution.guan <- function() {
  resolution <- "768x768"
  resolution
}

guan$choose.initial.point.guan <- function(zoom) {
  coordinate <- ""
  
  U <- runif(1)    
  for (j in 1:length(probs.places.sorted)) {
    if (U < sum(probs.places.sorted[1:j])) {
      city <- places[j,]
      lat <- city$coords.x2
      lon <- city$coords.x1
      break
    }
  }
  
  paste(lat, lon, zoom, sep=",")
}

rpareto1(1, 1.5, 1)

guan$simulate.interval.guan <- function(alpha, k) {
  interval <- rpareto1(1, alpha, k)
  if (is.na(interval)) {
    print(interval)
  }
  #interval <- rlnorm(1, 0.4983022, 0.5702684)
  total.interval <<- total.interval + as.integer(interval * 1000)
  
  as.integer(interval * 1000)
}

guan$simulate.operation.guan <- function(zoom) {
  interval <- guan$simulate.interval.guan(guan$interval.alpha, guan$interval.k)
  result <- guan$choose.operation.guan(zoom)
  zoom <- result[1]
  operation <- result[2]
  c(zoom, paste(operation, "+", interval, sep=""))
}

guan$zoomIn <- function(l, lambda) {
  1 - (l/l.max)^lambda
}

guan$zoomOut <- function(l, w, lambda) {
  w * ((l/l.max)^lambda)
}

guan$pan <- function(l, w, lambda) {
  (1 - w) * ((l/l.max)^lambda)
}

num_pans = 0
num_zoom_in = 0
num_zoom_out = 0

guan$choose.operation.guan <- function(zoom) {
  U <- runif(1)
  result = 0
  
  if (U < guan$zoomOut(as.numeric(zoom), guan$w, guan$lambda)) {
    num_zoom_out <<- num_zoom_out + 1
    result = guan$simulate.zoom.guan("out", zoom)
  } else if (U < guan$zoomOut(as.numeric(zoom), guan$w, guan$lambda) + guan$pan(as.numeric(zoom), guan$w, guan$lambda)) {
    num_pans <<- num_pans + 1
    result = guan$simulate.pan.guan(zoom)
  } else {
    num_zoom_in <<- num_zoom_in + 1
    result = guan$simulate.zoom.guan("in", zoom)
  }
  
  result
}

guan$simulate.pan.guan <- function(zoom) {
  U <- runif(1, 1, 5)
  
  result <- ""
  if (floor(U) == 1) {#SOUTH
    result <- c(0, PAN.SIZE)
  } else if (floor(U) == 2) {#WEST
    result <- c(-PAN.SIZE, 0)
  } else if (floor(U) == 3) {#NORTH
    result <- c(0, -PAN.SIZE)
  } else if (floor(U) == 4) {#EAST
    result <- c(PAN.SIZE, 0)
  }  
  
  #guan$simulated.pans.guan <<- rbind(guan$simulated.pans.guan, data.frame(x=result[1], y=result[2]))
  c(zoom, paste("PAN(", result[1], ",", result[2], ")", sep=""))
}

guan$simulate.zoom.guan <- function(type, last.zoom) {
  U <- 0
  print(max.zoom - as.numeric(last.zoom))
  
  if (type=="in") {
    U <- runif(1, 1, max.zoom - as.numeric(last.zoom) + 1)
  } else {
    U <- runif(1, -as.numeric(last.zoom), -1)
  }  
  
  print(U)
  
  new.zoom <- as.numeric(last.zoom) + floor(U)
  
  #guan$simulated.zooms.guan <<- rbind(guan$simulated.zooms.guan, data.frame(last=last.zoom, current=new.zoom))
  
  result = c(new.zoom, paste("ZOOM(", last.zoom, "," ,new.zoom, ",", 0, ",", 0, ")", sep=""))
  result
}

guan$simulate.zoom.guan("out", 18)

places <- as.data.frame(places.shape)
places <- places[with(places, order(POP2000)),]
probs.places <- places$POP2000/(sum(places$POP2000))
probs.places.sorted <- sort(probs.places)

tail(places$POP2000)

probs.initial.points <- points.per.polygon[,2]/(sum(points.per.polygon[,2]))
probs.initial.points.sorted <- sort(probs.initial.points)

#0.311 * e^(-0.2912*2)

# Guan session length
sls <- c()

for (i in 1:10000) {
  U <- runif(1)
  sls <- c(sls, -log(U/0.311)/0.2912)
}

plot(density(sls))

plot(density(rweibull(1000, 0.311, 0.2912)))

# use this to generate sessions with original parameters, from paper
guan$w = 0.9
guan$lambda = 3
guan$w.shape = 0.2912
guan$w.scale = 0.311
guan$interval.alpha = 1.5
guan$interval.k = 1
guan$session.length.estimated.from.our.data$estimate[[1]]
# use this to generate sessions with estimated parameters, from our collected data
guan$w = guan$omega.arg;guan$w
guan$lambda = guan$lambda.arg;guan$lambda
guan$w.shape = guan$session.length.estimated.from.our.data$estimate[[1]];guan$w.shape
guan$w.scale = guan$session.length.estimated.from.our.data$estimate[[2]];guan$w.scale
guan$interval.alpha = guan$think.time.pareto.estimated.from.our.data$exponent;guan$interval.alpha
guan$interval.k = guan$think.time.pareto.estimated.from.our.data$xmin;guan$interval.k

registerDoParallel(16)

foreach(procnum=1:16) %dopar% {
  guan$simulated.durations.guan <- data.frame(duration=c(), num.operations=c())
  guan$simulated.zooms.guan <- data.frame(last=c(), current=c())
  guan$simulated.zooms.array.guan <- c()
  guan$simulated.pans.guan <- data.frame(x=c(), y=c())

  total.interval <- 0
  
  file.path = paste(SIMULATED_DATA_PATH, "/custom-help-to-generate-charts-", procnum, ".csv", sep="")
  
  # This for simulates the guan's actions
  for (g in 1:1000000) {
    num.actions <- ceiling(rweibull(1, guan$w.shape, guan$w.scale))
    #print(num.actions)
    
    SIZE <- 100
    
    zoom <- 6
  
    resolution <- guan$choose.resolution.guan()
    
    passos <- c(paste("SET_RESOLUTION(", resolution, ")", sep=""))
    
    initial.point <- guan$choose.initial.point.guan(zoom)
    
    passos <- c(passos, paste("START(", initial.point, ")+", guan$simulate.interval.guan(guan$interval.alpha, guan$interval.k), sep=""))
    
    for (i in 1:num.actions) {
      guan$simulated.zooms.array.guan <- c(guan$simulated.zooms.array.guan, zoom)
      result <- guan$simulate.operation.guan(zoom)
      newZoom <- result[1]
      operation <- result[2]
      zoom <- newZoom
      passos <- c(passos, operation)
    }
    guan$simulated.zooms.array.guan <- c(guan$simulated.zooms.array.guan, zoom) #adicionar o último zoom
    
  #   while (total.interval < 30 * 60 * 1000) {
  #     result <- simulate.operation.guan(zoom)
  #     newZoom <- result[1]
  #     operation <- result[2]
  #     zoom <- newZoom
  #     passos <- c(passos, operation)
  #   }
    passos <- c(passos)
    #print(passos, quote = FALSE)
  #  if (length(passos) > 3) {
      cat(paste(passos, collapse=";"), file=file.path, append=TRUE, sep="\n")
  #   } else {
  #     g <- g - 1
  #   }
    
    #print(paste("Total interval:", total.interval / (1000 * 60), "min", "NUM_OP:", length(passos), sep=" "))
    guan$simulated.durations.guan <- rbind(guan$simulated.durations.guan, data.frame(duration=total.interval, num.operations=length(passos)))
    total.interval <- 0
    
    print(g)
  }
}

summary(guan$simulated.durations.guan[,2])
# summary(op.per.access)
quantile(muse.gm$simulated.durations[,2], 0.75)
quantile(guan$simulated.durations.guan[,2], 0.75)
quantile(muse.gm$simulated.durations[,1]/1000, 0.75)
quantile(guan$simulated.durations.guan[,1]/1000, 0.75)
quantile(op.per.access, 0.95)
dim(guan$simulated.durations.guan)

dim(guan$simulated.durations.guan)
summary(guan$simulated.durations.guan[,1]/1000)
summary(durations)

#util$plot.eps("duration-guan-simulated")
plot(ecdf(guan$simulated.durations.guan[,1]/1000), log="x", xlim=c(min(muse.gm$simulated.durations[,1]/1000), max(muse.gm$simulated.durations[,1]/1000)),
     ylab=expression("P(Session duration " <= " x)"), xlab="Session duration (s)", main="", lw=3)
#dev.off()

util$plot.eps("duration-comparison-data-mp-simulated")
ecdfPlot(durations, log="x", ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Session duration (s)", main="")
ecdfPlot(muse.gm$simulated.durations[,1]/1000, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Session duration (s)", main="", add=T)
legend("bottomright", legend = c("Data", "MUSeGen"),
       col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

summary(durations)
plot(density(muse.gm$simulated.durations[,1]/1000))
head(muse.gm$simulated.durations, 20)
head(guan$simulated.durations.guan, 20)

util$plot.eps("duration-comparison-guan-mp-simulated")
ecdfPlot(muse.gm$simulated.durations[,1]/1000, log="x", ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Session duration (s)", main="", xlim=c(min(muse.gm$simulated.durations[,1]/1000), 1e6))
ecdfPlot(guan$simulated.durations.guan[,1]/1000, ecdf.lty=3, ecdf.lw=6, ecdf.col=4, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Session duration (s)", main="", add=T)
legend("bottomright", legend = c("MUSeGen", "HELP"),
       col = c("red","blue"), pch=c(NA,NA), lty=c(2, 3),
       lwd=c(6,6),bty="n", pt.cex=2)
dev.off()

#util$plot.eps("duration-comparison-simulated-3")
ecdfPlot(durations, log="x", ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Session duration (s)", main="")
ecdfPlot(muse.gm$simulated.durations[,1]/1000, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Session duration (s)", main="", add=T)
ecdfPlot(guan$simulated.durations.guan[,1]/1000, ecdf.lty=3, ecdf.lw=6, ecdf.col=4, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Session duration (s)", main="", add=T)
legend("bottomright", legend = c("Data", "MUSeGen", "HELP"),
       col = c("black","red", "blue"), pch=c(NA,NA), lty = c(1, 2, 3),
       lwd=c(6,6,6),bty="n", pt.cex=2)
#dev.off()

lines(ecdf(durations), col=2)

util$plot.eps("number-of-operations-comparison-data-mp-simulated")
ecdfPlot(op.per.access, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of operations" <= " x)"), 
         xlab="Number of operations", main="")
ecdfPlot(muse.gm$simulated.durations[,2], ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Number of operations" <= " x)"), 
         xlab="Number of operations", main="", add=T)
legend("bottomright", legend = c("Data", "MUSeGen"),
       col = c("black", "red"), pch=c(NA,NA), lty = c(1, 2),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

summary(muse.gm$simulated.durations[,1]/1000)
summary(guan$simulated.durations.guan[,1]/1000)

summary(muse.gm$simulated.durations[,2] - 1)
summary(guan$simulated.durations.guan[,2] - 2)

quantile(muse.gm$simulated.durations.guan[,2], 0.99)
length(guan$simulated.durations.guan[guan$simulated.durations.guan[,2] >= 10,2])

util$plot.eps("number-of-operations-comparison-guan-mp-simulated")
ecdfPlot(muse.gm$simulated.durations[,2] - 1, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Number of operations" <= " x)"), 
         xlab="Number of operations", main="", xlim=c(0, max(guan$simulated.durations.guan[,2])))
ecdfPlot(guan$simulated.durations.guan[,2] - 1, ecdf.lty=3, ecdf.lw=6, ecdf.col=4, ylab=expression("P(Number of operations" <= " x)"), 
         xlab="Number of operations", main="", add=T)
legend("bottomright", legend = c("MUSeGen", "HELP"),
       col = c("red", "blue"), pch=c(NA,NA), lty = c(2, 3),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

#util$plot.eps("number-of-operations-comparison-guan-mp-10-min-simulated")
ecdfPlot(muse.gm$simulated.durations[,2] - 1, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Number of operations" <= " x)"), 
         xlab="Number of operations", main="", xlim=c(0, max(guan$simulated.durations.guan[,2]) - 1))
ecdfPlot(guan$simulated.durations.guan[,2] - 1, ecdf.lty=3, ecdf.lw=6, ecdf.col=4, ylab=expression("P(Number of operations" <= " x)"), 
         xlab="Number of operations", main="", add=T)
par(cex=1.6)
legend("bottomright", legend = c("MUSeGen", "HELP"),
       col = c("red", "blue"), pch=c(NA,NA), lty = c(2, 3),
       lwd=c(4,4),bty="n", pt.cex=1)
#dev.off()

?ecdfPlot

lines(ecdf(op.per.access), col=2)

require(mtcars)
counts <- table(mtcars$vs, mtcars$gear)
counts
pans.guan.to.chart <- matrix(c(0.25, 0.5, 0.25, 0.25, 0.5, 0.25), ncol=3, byrow=T)

colnames(pans.guan.to.chart) <- c(256, 0, 256)
pans.guan.to.chart

util$plot.eps("pan-size-guan-simulated")
bp <- barplot(pans.guan.to.chart,
              xlab="Pan size (px)", ylab="Frequency",
              mgp=c(2,0.5,.5), beside=TRUE, col=c(2, 4), legend=T, ylim=c(0, 0.61))

arrows(bp[1,1], 0.3, bp[1,1], 0.255, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)
arrows(bp[2,1], 0.3, bp[2,1], 0.255, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)
arrows(bp[1,3], 0.3, bp[1,3], 0.255, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)
arrows(bp[2,3], 0.3, bp[2,3], 0.255, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)

text(cex=1, x=bp[1,1], y=0.4, labels="Left", srt=-90, xpd=TRUE)
text(cex=1, x=bp[2,1], y=0.4, labels="Upward", srt=-90, xpd=TRUE)
text(cex=1, x=bp[1,3], y=0.4, labels="Right", srt=-90, xpd=TRUE)
text(cex=1, x=bp[2,3], y=0.4, labels="Downward", srt=-90, xpd=TRUE)

legend(x=7, y=0.62, legend = c("X", "Y"),
       col = c("red", "blue"), pch=c(NA,NA), fill=c(2, 4),
       pt.cex=2, bty="n")
dev.off()

util$plot.eps("zoom-level-guan-simulatation-6-21", margin=c(3, 3, 1, 0))
bp <- barplot(table(as.numeric(guan$simulated.zooms.array.guan)) / length(as.numeric(guan$simulated.zooms.array.guan)), ylim=c(0, 0.25), 
              names.arg="", xlab="Zoom level", ylab="Frenquency",
              col=c("red"), mgp=c(2,0.5,.5))
text(cex=0.8, x=bp, y=-0.015, labels=levels(as.factor(as.numeric(guan$simulated.zooms.array.guan))), srt=-90, xpd=TRUE)
dev.off()

tile.to.coord("17/24882/48342", 17)

interval.sum <- 0

for (i in 1:20) {
  inter <- muse.gm$simulate.interval()
  print(inter)
  interval.sum <- interval.sum + inter
}

print(interval.sum/20)


num_zoom_out = 0
num_zoom_in = 0
num_pans = 0
guan$w = 0.4
guan$lambda = 0.45

guan$w = 0.9
guan$lambda = 3

startZoom = 6
zoomLevels = c(startZoom)
lastZoomLevel = startZoom
count = 0
weib = rweibull(1, 0.311, 0.2912)
for (i in 1:10000) {
  count = count + 1
  print(i)
  lastZoomLevel = guan$choose.operation.guan(lastZoomLevel)[1]
  if (count >= ceiling(weib)) {
    weib = rweibull(1, 0.311, 0.2912)
    lastZoomLevel = startZoom
    print(weib)
    count = 0
  }
  lastZoomLevel = as.integer(lastZoomLevel)
  zoomLevels = c(zoomLevels, lastZoomLevel)
}
total = num_zoom_out + num_zoom_in + num_pans
print(total)
print(num_zoom_out/total)
print(num_zoom_in/total)
print(num_pans/total)

bp <- barplot(table(zoomLevels)/length(zoomLevels),
              xlab="Zoom Level", ylab="Frequency",
              mgp=c(2,0.5,.5), beside=TRUE, col=c(2))


