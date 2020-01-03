# This file is a refactoring of user-simulation file
library(maptools)
library(evmix)

muse.gm = new.env()

muse.gm$simulate.operation <- function(lastOp, lastZoom, last.pan.dir, last.zoom.dir) {
  interval <- muse.gm$simulate.interval() 
  muse.gm$simulated.zooms.array <<- c(muse.gm$simulated.zooms.array, as.numeric(lastOp))
  if (lastOp != lastZoom) {
    muse.gm$simulated.zooms.jump <<- c(muse.gm$simulated.zooms.jump, as.numeric(lastOp) - as.numeric(lastZoom))
    return(paste(muse.gm$simulate.zoom(lastZoom, lastOp, last.zoom.dir), "+", interval, sep=""))
  } else {
    return(paste(muse.gm$simulate.pan(lastOp, last.pan.dir), "+", interval, sep=""))
  }
}

muse.gm$simulate.pan <- function(zoom.level, last.dir, result=c()) {
  first.pan.probs <- all.directions.freq[9,]
  
  parameter.x <- -0.0001376 * as.numeric(zoom.level) + 0.0079955
  parameter.y <- -0.0002038* as.numeric(zoom.level) + 0.0085857
  
  pan.x <- rgeom(1, parameter.x)
  pan.y <- rgeom(1, parameter.y)
  
  freq.frame <- as.data.frame(all.directions.freq)  
  
  if (last.dir == "") {
    last.dir <- util$sort.probabilities(first.pan.probs, last.dir)
  } else {
    probs <- freq.frame[last.dir,]
    last.dir <- util$sort.probabilities(probs, last.dir)
  }
  
  result <- c()
  if (last.dir == "0/PB") {
    result <- (c(0, pan.y))
  } else if (last.dir == "0/PC") {
    result <- (c(0, -pan.y))
  } else if (last.dir == "D/0") {
    result <- (c(pan.x, 0))
  } else if (last.dir == "D/PB") {
    result <- (c(pan.x, pan.y))
  } else if (last.dir == "D/PC") {
    result <- (c(pan.x, -pan.y))
  } else if (last.dir == "E/0") {
    result <- (c(-pan.x, 0))
  } else if (last.dir == "E/PB") {
    result <- (c(-pan.x, pan.y))
  } else if (last.dir == "E/PC") {
    result <- (c(-pan.x, -pan.y))
  }
  
  muse.gm$simulated.pans <<- rbind(muse.gm$simulated.pans, data.frame(x=result[1], y=result[2]))
  
  paste("PAN(", result[1], ",", result[2],")", sep="")
}

muse.gm$simulate.zoom <- function(last.zoom, current.zoom, last.dir, result=c()) {
  first.zoom.probs <- all.directions.z.freq[10,]
  
  pan.x <- rgeom(1, fit.geo.x$estimate[1])
  pan.y <- rgeom(1, fit.geo.y$estimate[1])
  
  freq.frame <- as.data.frame(all.directions.z.freq)  
  
  if (last.dir == "") {
    last.dir <- util$sort.probabilities(first.zoom.probs, last.dir)
  } else {
    probs <- freq.frame[last.dir,]
    last.dir <- util$sort.probabilities(probs, last.dir)
  }
  
  result <- c()
  if (last.dir == "0/PB") {
    result <- (c(0, pan.y))
  } else if (last.dir == "0/PC") {
    result <- (c(0, -pan.y))
  } else if (last.dir == "D/0") {
    result <- (c(pan.x, 0))
  } else if (last.dir == "D/PB") {
    result <- (c(pan.x, pan.y))
  } else if (last.dir == "D/PC") {
    result <- (c(pan.x, -pan.y))
  } else if (last.dir == "E/0") {
    result <- (c(-pan.x, 0))
  } else if (last.dir == "E/PB") {
    result <- (c(-pan.x, pan.y))
  } else if (last.dir == "E/PC") {
    result <- (c(-pan.x, -pan.y))
  } else if (last.dir == "0/0") {
    result <- (c(0, 0))
  }
  
  muse.gm$simulated.zooms <<- rbind(muse.gm$simulated.zooms, data.frame(last=last.zoom, current=current.zoom, x=result[1], y=result[2]))
  
  paste("ZOOM(", last.zoom, "," , current.zoom , "," , result[1], ",", result[2],")", sep="")
}

muse.gm$simulate.interval <- function() {
  interval <- rlognormgpdcon(n=1, lnmean=lngpd.fit$lnmean, lnsd=lngpd.fit$lnsd, u=lngpd.fit$u, 
                             xi=lngpd.fit$xi, phiu=lngpd.fit$phiu)
  #   while (interval >  10 * 60) {
  #     interval <- sample(intervals, 1)
  #   }
  #interval <- rlnorm(1, 0.4983022, 0.5702684)
  muse.gm$total.interval <<- muse.gm$total.interval + as.integer(interval * 1000)
  muse.gm$simulated.intervals <<- c(muse.gm$simulated.intervals, as.integer(interval * 1000))
  as.integer(interval * 1000)
}

summary(rlognormgpdcon(n=10000, lnmean=lngpd.fit$lnmean, lnsd=lngpd.fit$lnsd, u=lngpd.fit$u, 
                       xi=lngpd.fit$xi, phiu=lngpd.fit$phiu))


muse.gm$choose.resolution <- function() {
  probs <- resolutions[,2]/(sum(resolutions[,2]))
  probs.sorted <- sort(probs)
  
  resolution <- ""
  U <- runif(1)    
  for (j in 1:length(probs.sorted)) {
    if (U < sum(probs.sorted[1:j])) {
      resolution <- as.character(resolutions[abs(j - 21),1])
      if (resolution == "") {
        print(paste("J = ", j, " e abs(j - 21) = ", abs(j - 21)))
      }
      break
    }
  }
  resolution
}

muse.gm$choose.initial.point <- function(zoom, reset_data=FALSE) {
  if (reset_data) {
    places <- as.data.frame(places.shape)
    places <- places[with(places, order(POP2000)),]
    probs.places <- places$POP2000/(sum(places$POP2000))
    probs.places.sorted <- sort(probs.places)
  }
  
  coordinate <- ""
  
  U <- runif(1)    
  for (j in 1:length(probs.places.sorted)) {
    if (U < sum(probs.places.sorted[1:j])) {
      city <- places[j,]
      coordinate <- paste(city$coords.x2, ",", city$coords.x1, ",", zoom, sep="")
      break
    }
  }
  #cat(paste("new google.maps.LatLng(", coordinate, "),", sep=""), file="/media/arquivos/Dropbox/mestrado/4-semestre/gogeo/resultados-testes/50-users-coordinates.csv", append=TRUE, sep="\n")
  coordinate
}


dim(muse.gm$simulated.durations)

cat(paste(muse.gm$simulated.durations[,1], collapse="\n"), file=paste(SIMULATED_DATA_PATH, "/muse-gm-durations.csv", sep=""), append=F, sep="\n")
cat(paste(muse.gm$simulated.durations[,2], collapse="\n"), file=paste(SIMULATED_DATA_PATH, "/muse-gm-number-of-actions.csv", sep=""), append=F, sep="\n")

#cat(paste(muse.gm$simulated.durations[,1], collapse="\n"), file="/media/arquivos/Dropbox/mestrado/4-semestre/dissertacao/svnlabora/teses/msc/vinicius/texto/defesa/figs/r/muse-gm-durations-10min.csv", append=F, sep="\n")
#cat(paste(muse.gm$simulated.durations[,2], collapse="\n"), file="/media/arquivos/Dropbox/mestrado/4-semestre/dissertacao/svnlabora/teses/msc/vinicius/texto/defesa/figs/r/muse-gm-number-of-actions-10min.csv", append=F, sep="\n")

path = "/media/arquivos/pcloud/mestrado/4-semestre/dissertacao/svnlabora/teses/msc/vinicius/texto/defesa/figs/resolutions/resolution-ww-monthly-201503-201505-bar.csv"
resolutions = read.table(path, sep=",", dec=".", header=TRUE)
resolutions = resolutions[-21,]
resolutions

file.path = paste(SIMULATED_DATA_PATH, "/actions-muse-gm.csv", sep="")

#plot(ecdf(muse.gm$simulated.intervals/1000), log="x", xlim=c(min(muse.gm$simulated.intervals)/1000, max(muse.gm$simulated.intervals)/1000))
#plot(ecdf(intervals), log="x", , xlim=c(min(muse.gm$simulated.intervals)/1000, max(muse.gm$simulated.intervals)/1000), col=2,add=T)

#cat("",file=file.path,append=FALSE)

op.dens <- op.dens.bkp
#op.dens <- op.dens.bkp10min

as.data.frame(op.dens)

muse.gm$simulate.muse.gm = function(number.of.sessions) {
  muse.gm$total.interval <<- 0
  
  # This for simulates the muse-gm's actions
  for (g in 1:number.of.sessions) {
    SIZE <- 1000
    
    freq.frame <- as.data.frame(op.dens)
    freq.frame
    
    lastOp <- ""
    lastZoom <- ""
    last.pan.dir <- ""
    last.zoom.dir <- ""
    
    #escolher primeiro passo
    (U <- runif(1))
    for (i in 1:length(operations.path$start.prob)) {
      probs.sorted <- sort(operations.path$start.prob)
      
      if (U < sum(probs.sorted[1:i])) {
        lastOp <- rownames(as.matrix(probs.sorted))[i]
        lastZoom <- lastOp
        break
      }
    }
    resolution <- muse.gm$choose.resolution()
    
    passos <- c(paste("SET_RESOLUTION(", resolution, ")", sep=""))
    
    initial.point <- muse.gm$choose.initial.point(15)
    
    start.interval <- muse.gm$simulate.interval()
    
    #muse.gm$total.interval <<- muse.gm$total.interval + start.interval
    
    passos <- c(passos, paste("START(", initial.point, ")+", start.interval, sep=""))
    
    for (i in 1:SIZE) {
      probs <- freq.frame[as.numeric(lastOp),]
      probs.sorted <- sort(probs)
      
      U <- runif(1)    
      for (j in 1:length(probs.sorted)) {
        if (U < sum(probs.sorted[1:j])) {
          path.synt <- c(path.synt, colnames(probs.sorted)[j])
          lastOp <- colnames(probs.sorted)[j]
          break
        }
      }
      if (lastOp == "start/end") {
        break
      } else {
        operation.sim <- muse.gm$simulate.operation(lastOp, lastZoom, last.pan.dir, last.zoom.dir)
        lastZoom <- lastOp
        passos <- c(passos, operation.sim)
      }
      #     This part forces a session with 10 minutes
      #     if (muse.gm$total.interval > 1000 * 60 * 10) {
      #       print(paste("Total interval:", muse.gm$total.interval / (1000 * 60), "min", "NUM_OP:", length(passos), sep=" "))
      #       muse.gm$total.interval <- 0
      #       break
      #     }
    }
    passos <- c(passos)
    #print(passos, quote = FALSE)
    #  if (length(passos) > 3) {
    cat(paste(passos, collapse=";"), file=file.path, append=TRUE, sep="\n")
    #  } else {
    #    g <- g - 1
    #  }
    
    muse.gm$simulated.number.of.operations <- c(muse.gm$simulated.number.of.operations, length(passos))
    muse.gm$simulated.durations <- rbind(muse.gm$simulated.durations, data.frame(duration=muse.gm$total.interval, num.operations=length(passos)))
    
    #print(muse.gm$total.interval)
    #print(paste("Total interval:", muse.gm$total.interval / (1000 * 60), "min", "NUM_OP:", length(passos), sep=" "))
    muse.gm$total.interval <- 0
    #print(max(muse.gm$simulated.number.of.operations))
    print(g)
  }
}

muse.gm$simulated.intervals <- c()
muse.gm$simulated.durations <- data.frame(duration=c(), num.operations=c())
muse.gm$simulated.zooms <- data.frame(last=c(), current=c(), x=c(), y=c())
muse.gm$simulated.zooms.array <- c()
muse.gm$simulated.zooms.jump <- c()
muse.gm$simulated.pans <- data.frame(x=c(), y=c())
muse.gm$simulated.number.of.operations <- c()
muse.gm$total.interval <- 0

muse.gm$simulate.muse.gm(10000)

# Jumps when the initial zoom is 15
jumps.in.4 = as.numeric(as.character(muse.gm$simulated.zooms[as.character(muse.gm$simulated.zooms[,1]) == 15,2])) - 15

head(jumps.in.4)

head(muse.gm$simulated.zooms[as.numeric(as.character(muse.gm$simulated.zooms[,1])) == 15,2])

plot(table(jumps.in.4)/length(jumps.in.4), type="h")

table(muse.gm$simulated.zooms.jump)/length(muse.gm$simulated.zooms.jump)

jumpInZoom.simulated.freq <- 1:32 - 1:32;jumpInZoom.simulated.freq

jumpInZoom.simulated.freq[30]
muse.gm$simulated.zooms.jump[muse.gm$simulated.zooms.jump == 16]
length(muse.gm$simulated.zooms.jump)
for (i in 1:length(muse.gm$simulated.zooms.jump)) {
  jumpSize <- muse.gm$simulated.zooms.jump[i] + 16
  if (jumpSize == 32) {
    print(jumpInZoom.simulated.freq[jumpSize])
  }
  jumpInZoom.simulated.freq[jumpSize] <- jumpInZoom.simulated.freq[jumpSize] + 1
}

jumpInZoom.simulated.freq

jumpInZoom.simulated.freq/sum(jumpInZoom.simulated.freq)

util$plot.eps("zoom-jump-size-mp", margin=c(3.6, 3.5, 1, 0.8))
plot(jumpInZoom.simulated.freq/sum(jumpInZoom.simulated.freq), type="h", log="y", lwd=13, col=c(2:16 - 1:15, 3:19 - 1:17), 
     xaxt="n", yaxt="n", xlab="Jump size", ylab="Frequency")
legend("topright", legend = c("Zoom Out", "Zoom In"),
       col = c(1, 2), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", cex=0.9)
axis(1, at=c(1, 5, 10, 15, 17, 21, 26, 31),labels=c(15, 10, 5, 1, 1, 5, 10, 15), col.axis="black", las=2)
axis(2, at=c(1e-4, 1e-3, 1e-2, 1e-1),labels=c("1e-04", "1e-03", "1e-02", "1e-01"), col.axis="black", las=3)
dev.off()

plot(density(muse.gm$simulated.intervals))
summary(muse.gm$simulated.number.of.operations)
summary(op.per.access)
quantile(muse.gm$simulated.number.of.operations, 0.8)
quantile(op.per.access, 0.8)
length(muse.gm$simulated.number.of.operations)

dim(muse.gm$simulated.durations)
summary(muse.gm$simulated.durations[,1]/1000)
summary(durations)

library(EnvStats)

#plot.eps("duration-model-simulated")
plot(ecdf(muse.gm$simulated.durations[,1]/1000), log="x", xlim=c(min(muse.gm$simulated.durations[,1]/1000), max(muse.gm$simulated.durations[,1]/1000)),
     ylab=expression("P(Session durations (s) " <= " x)"), xlab="Sessions durations (s)", main="", lw=3)
#dev.off()

plot(ecdf(muse.gm$simulated.durations[,1]/1000), log="x", xlim=c(min(muse.gm$simulated.durations[,1]/1000), max(muse.gm$simulated.durations[,1]/1000)))
lines(ecdf(durations[durations <= max(muse.gm$simulated.durations[,1]/1000)]), col=2)

#plot.eps("number-of-operations-model-simulated")
ecdfPlot(muse.gm$simulated.number.of.operations, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of actions" <= " x)"), 
         xlab="Number of actions", main="")
#dev.off()

#plot.eps("pan-size-model-simulated")
plot(density(muse.gm$simulated.pans$x), xlim=c(-1000, 1000), lwd=6, col=2, main="",
     xlab="Pan size (px)", ylab="Density", axes=F)
lines(density(muse.gm$simulated.pans$y), lty=2, lwd=6, col=4)

axis(side = 1, at=c(-1500, -1000, -500, 0, 500, 1000, 1500), labels=c(1500, 1000, 500, 0, 500, 1000, 1500))
axis(side = 2)
box()

arrows(0, 0, 0, 1, code = 2, col=1, lty=2, lwd=2)
arrows(-300, 0.002, -950, 0.002, length=0.25, angle = 30, code=2, col=2, lty=1, lwd=6)
arrows(-300, 0.0015, -950, 0.0015, length=0.25, angle = 30, code=2, col=4, lty=1, lwd=6)
arrows(300, 0.002, 950, 0.002, length=0.25, angle = 30, code = 2, col=2, lty=1,lwd=6)
arrows(300, 0.0015, 950, 0.0015, length=0.25, angle = 30, code = 2, col=4, lty=1,lwd=6)

text(cex=1, x=-550, y=0.0022, labels="Left", xpd=TRUE)
text(cex=1, x=-550, y=0.0017, labels="Up", xpd=TRUE)
text(cex=1, x=550, y=0.0022, labels="Right", xpd=TRUE)
text(cex=1, x=550, y=0.0017, labels="Down", xpd=TRUE)

legend("topright", legend = c("X", "Y"),
       col = c("red", "blue"), pch=c(NA,NA), lty=c(1, 2), lwd=c(4, 4),
       pt.cex=2, bty="n")
#dev.off()

plot(density(muse.gm$simulated.zooms$x), xlim=c(-500, 500))
lines(density(muse.gm$simulated.zooms$y), col=2)

op.dens

muse.gm$simulated.zooms.all <- c(muse.gm$simulated.zooms$last, muse.gm$simulated.zooms$current) 
muse.gm$simulated.zooms.all <- factor(muse.gm$simulated.zooms.all, levels=1:nlevels(muse.gm$simulated.zooms$last), labels=levels(muse.gm$simulated.zooms$last))

#sample.zoom <- sample(muse.gm$simulated.zooms.array, 1000)

#util$plot.eps("zoom-level-model-simulatation-2", margin=c(3, 3, 1, 0))
bp <- barplot(table(muse.gm$simulated.zooms.array) / length(muse.gm$simulated.zooms.array), ylim=c(0, 0.20), 
              names.arg="", xlab="Zoom level", ylab="Frequency",
              col=c("red"), mgp=c(2,0.5,.5))
text(cex=0.8, x=bp, y=-0.009, labels=levels(as.factor(muse.gm$simulated.zooms.array)), srt=90, xpd=TRUE)
#dev.off()

util$plot.eps("pan-sizes-pdf-comparison-simulated-x")
plot(density(movesInPixels[,1]), xlim=c(-1500, 1500), lwd=6, col=1, main="",
     xlab="Pan size (px)", ylab="Density", axes=F, ylim=c(0, 0.006))
lines(density(muse.gm$simulated.pans$x), lty=2, lwd=6, col=2)

axis(side = 1, at=c(-1500, -1000, -500, 0, 500, 1000, 1500), labels=c(1500, 1000, 500, 0, 500, 1000, 1500))
axis(side = 2)
box()

arrows(0, 0, 0, 1, code = 2, col=1, lty=2, lwd=2)
arrows(-400, 0.003, -1050, 0.003, length=0.25, angle = 30, code=2, col=1, lty=1, lwd=6)
arrows(400, 0.003, 1050, 0.003, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)

text(cex=1, x=-650, y=0.0033, labels="Left", xpd=TRUE)
text(cex=1, x=750, y=0.0033, labels="Right", xpd=TRUE)

par(cex=1.8)
legend("topright", legend = c("Data", "MUSeGen"),
       col = c("black", "red"), pch=c(NA,NA), lty=c(1, 2), lwd=c(4, 4),
       pt.cex=2, bty="n")
dev.off()

util$plot.eps("pan-sizes-pdf-comparison-simulated-y")
plot(density(movesInPixels[,2]), xlim=c(-1500, 1500), lwd=6, col=1, main="",
     xlab="Pan size (px)", ylab="Density", axes=F, ylim=c(0, 0.006))
lines(density(muse.gm$simulated.pans$y), lty=2, lwd=6, col=2)

axis(side = 1, at=c(-1500, -1000, -500, 0, 500, 1000, 1500), labels=c(1500, 1000, 500, 0, 500, 1000, 1500))
axis(side = 2)
box()

arrows(0, 0, 0, 1, code = 2, col=1, lty=2, lwd=2)
arrows(-400, 0.002, -1050, 0.002, length=0.25, angle = 30, code=2, col=1, lty=1, lwd=6)
arrows(400, 0.002, 1050, 0.002, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)

text(cex=1, x=-650, y=0.0024, labels="Upward", xpd=TRUE)
text(cex=1, x=750, y=0.0024, labels="Downward", xpd=TRUE)

par(cex=1.8)
legend("topright", legend = c("Data", "MUSeGen"),
       col = c("black", "red"), pch=c(NA,NA), lty=c(1, 2), lwd=c(4, 4),
       pt.cex=2, bty="n")
dev.off()

summary(muse.gm$simulated.zooms$current)

for (i in 1:1000) {
  muse.gm$choose.initial.point(15)
}

print(muse.gm$choose.initial.point(15))

probs.initial.points <- points.per.polygon[,2]/(sum(points.per.polygon[,2]))
probs.initial.points.sorted <- sort(probs.initial.points)

muse.gm$choose.initial.point2 <- function(zoom) {  
  tile <- ""
  U <- runif(1)    
  for (j in 1:length(probs.initial.points.sorted)) {
    if (U < sum(probs.initial.points.sorted[1:j])) {
      tile <- as.character(points.per.polygon[j,1])
      break
    }
  }
  tile.to.coord(tile, zoom)
}
