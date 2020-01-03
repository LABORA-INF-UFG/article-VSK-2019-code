source("./util/util.R")

util$pkgRequire("e1071")
util$pkgRequire("MASS")
util$pkgRequire("ADGofTest")
util$pkgRequire("fitdistrplus")
util$pkgRequire("markovchain")
util$pkgRequire("xtable")
util$pkgRequire("actuar")
util$pkgRequire("EnvStats")

path = paste(PCLOUD_DIR, "mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/PlaceOfZoom.csv", sep="/");path
place.of.zoom.readed = read.table(file=path, sep=",", dec=".", header=F)

head(place.of.zoom.readed, 10)
subset(place.of.zoom.readed, place.of.zoom.readed[,2] == max(place.of.zoom.readed[,2]))
quantile(abs(place.of.zoom.readed[,3]), 0.99)

subset(place.of.zoom.readed, abs(place.of.zoom.readed[,2]) > 11821.5)
quantile(subset(abs(place.of.zoom.readed[,2]), abs(place.of.zoom.readed[,5]) <= 15), 0.99)
length(subset(abs(place.of.zoom.readed[,2]), abs(place.of.zoom.readed[,5]) == 3))
subset(abs(place.of.zoom.readed[,2]), abs(place.of.zoom.readed[,5]) == 6)

x.place.zoom <- subset(place.of.zoom.readed, abs(place.of.zoom.readed[,2]) < 752.76
                       & abs(place.of.zoom.readed[,3]) < 752.76)

plot(density(subset(abs(x.place.zoom[,2]), abs(x.place.zoom[,5]) > 0)))
lines(density(subset(abs(x.place.zoom[,3]), abs(x.place.zoom[,5]) > 0)), col=2)

x.maior.que.zero <- subset(abs(x.place.zoom[,2]), abs(x.place.zoom[,2]) > 0)
y.maior.que.zero <- subset(abs(x.place.zoom[,3]), abs(x.place.zoom[,3]) > 0)

#dev.off()
plot(1:2)
util$plot.eps("zoom-geom-x")
fit.geo.x <- fitdistr(abs(x.maior.que.zero), "geometric")
print(fit.geo.x)

ks.test(abs(x.maior.que.zero), rgeom(length(x.maior.que.zero), fit.geo.x$estimate[1]))
chisq.test(abs(x.maior.que.zero), rgeom(length(x.maior.que.zero), fit.geo.x$estimate[1]))
chisq.test(rgeom(length(x.maior.que.zero), fit.geo.x$estimate[1]), rgeom(length(x.maior.que.zero), fit.geo.x$estimate[1]))

geometrica.x <- rgeom(length(x.maior.que.zero), fit.geo.x$estimate[1])

ecdfPlot(abs(x.maior.que.zero), ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("Prob(Movement size " <= " x)"), 
         xlab="Movement size (px)", main="")
ecdfPlot(geometrica.x, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("Prob(Movement size " <= " x)"), 
         xlab="Movement size (px)", main="", add=T)
legend("bottomright", legend = c("Observed", "Geometric"),
       col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
       lwd=c(4,4),bty="n", pt.cex=2)

# plot(ecdf(abs(x.maior.que.zero)), lw=4, main="", xlab="Tamanho do movimento (px)",
#      ylab=expression("Prob(Tamanho do movimento " <= " x)"))
# lines(ecdf(rgeom(2000, fit.geo.x$estimate[1])), col=2, lw=1)
# legend("bottomright", legend = c("Observado", "Geométrica"),
#        col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
#        lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

util$plot.eps("zoom-geom-y")
fit.geo.y <- fitdistr(abs(y.maior.que.zero), "geometric")
print(fit.geo.y)

geometrica.y <- rgeom(length(x.maior.que.zero), fit.geo.y$estimate[1])

ecdfPlot(abs(y.maior.que.zero), ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("Prob(Movement size " <= " x)"), 
         xlab="Movement size (px)", main="")
ecdfPlot(geometrica.y, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("Prob(Movement size " <= " x)"), 
         xlab="Movement size (px)", main="", add=T)
legend("bottomright", legend = c("Observed", "Geometric"),
       col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
       lwd=c(4,4),bty="n", pt.cex=2)

# plot(ecdf(abs(y.maior.que.zero)), lw=4, main="", xlab="Tamanho do movimento (px)",
#         ylab=expression("Prob(Tamanho do movimento " <= " x)"))
# lines(ecdf(rgeom(2000, fit.geo.y$estimate[1])), col=2, lw=1)
# legend("bottomright", legend = c("Observado", "Geométrica"),
#        col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
#        lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

place.of.zoom <- x.place.zoom

all.directions.z <- c()

count = 0
for (i in 1:max(place.of.zoom[,1])) {

  sub <- subset(place.of.zoom, place.of.zoom[,1] == i)
  if (dim(sub)[1] >= 2) {
    count <- count + 1
    
    for (j in 1:dim(sub)[1]) {
      if (sub[,2][j] > 0 & sub[,3][j] > 0) {
        all.directions.z <- c(all.directions.z, "D/PB")    
      } else if (sub[,2][j] > 0 & sub[,3][j] == 0) {
        all.directions.z <- c(all.directions.z, "D/0")
      } else if (sub[,2][j] > 0 & sub[,3][j] < 0) {
        all.directions.z <- c(all.directions.z, "D/PC")
      } else if (sub[,2][j] < 0 & sub[,3][j] > 0) {
        all.directions.z <- c(all.directions.z, "E/PB")
      } else if (sub[,2][j] < 0 & sub[,3][j] == 0) {
        all.directions.z <- c(all.directions.z, "E/0")
      } else if (sub[,2][j] < 0 & sub[,3][j] < 0) {
        all.directions.z <- c(all.directions.z, "E/PC")
      } else if (sub[,2][j] == 0 & sub[,3][j] > 0) {
        all.directions.z <- c(all.directions.z, "0/PB")
      } else if (sub[,2][j] == 0 & sub[,3][j] < 0) {
        all.directions.z <- c(all.directions.z, "0/PC")
      } else if (sub[,2][j] == 0 & sub[,3][j] == 0) {
        all.directions.z <- c(all.directions.z, "0/0")
      }
    }
    all.directions.z <- c(all.directions.z, "start/end")    
  }
}

all.directions.z.seq <- createSequenceMatrix(stringchar = all.directions.z)
all.directions.z.seq

all.directions.z.freq <- all.directions.z.seq[,-10]/apply(all.directions.z.seq[,-10], MARGIN=1, FUN=sum)
round(all.directions.z.freq, 3)
all.directions.z.freq[10,]

xtable(all.directions.z.freq, digits=3, caption="teste", label="teste")

#plot.eps("pan-direction-simulation")
#par(mfrow=c(3,3))

all.direction.synt.z <- c()

for (g in 1:1000) {
  SIZE <- 100
  
  freq.frame <- as.data.frame(all.directions.z.freq)
  freq.frame
  
  probs.matrix.z <- matrix(all.directions.z.seq[9,c(-9, -10)], nrow=1, ncol=8, dimnames=list(c(), colnames(all.directions.seq[,-9])))
  
  probs.init <- (probs.matrix.z/apply(probs.matrix.z, MARGIN=1, FUN=sum))
  round(probs.init,2)
  
  lastDirection.z <- ""
  
  (U <- runif(1))
  for (i in 1:length(probs.init)) {
    probs.sorted <- sort(as.data.frame(probs.init))
    probs.sorted
    
    if (U < sum(probs.sorted[1:i])) {
      lastDirection.z <- colnames(as.matrix(probs.sorted))[i]
      break
    }
  }
  
  for (i in 1:SIZE) {
    probs <- freq.frame[lastDirection.z,]
    probs.sorted <- sort(probs)
    
    U <- runif(1)    
    for (j in 1:length(probs.sorted)) {
      if (U < sum(probs.sorted[1:j])) {
        all.direction.synt.z <- c(all.direction.synt.z, colnames(probs.sorted)[j])
        lastDirection.z <- colnames(probs.sorted)[j]
        break
      }
    }
  }
  
  all.direction.synt.z <- c(all.direction.synt.z, "END")  
}

head(all.directions.z, 25)
tail(all.direction.synt.z, 20)

all.direction.synt.z.seq <- createSequenceMatrix(stringchar = all.direction.synt.z)
all.direction.synt.z.freq <- all.direction.synt.z.seq[-7,-7]/apply(all.direction.synt.z.seq[-7,-7], MARGIN=1, FUN=sum)
round(all.directions.z.freq, 2)
round(all.direction.synt.z.freq, 2)
sum(diag(all.direction.synt.z.freq))/8

xtable(all.directions.z.freq, digits=4)

###################################################################3

JUMP <- 1
md.place.z <- c()
quant.place.z <- c()
for (i in 5:21) {
  print(paste(i, quantile(abs(subset(x.place.zoom, x.place.zoom[,4] == i)[,2]), 0.75)), sep=" - ")
  quant.place.z <- c(quant.place.z, quantile(abs(subset(x.place.zoom, x.place.zoom[,4] == i & x.place.zoom[,5] == JUMP)[,3]), 0.9))
  md.place.z <- c(md.place.z, mean(abs(subset(x.place.zoom, x.place.zoom[,4] == i & x.place.zoom[,5] == JUMP)[,3])))
}

plot(quant.place.z, xlim=c(1,20))

zooms.to.count <- 5:21

plot.eps("zoom-pan-size-correlation-x")
(xlm <- lm(md.place.z~as.numeric(zooms.to.count)))
plot(md.place.z~as.numeric(zooms.to.count), ylim=c(0,300), xlab="Nível de zoom", ylab="Média do tamanho dos arrastes (px)")
abline(xlm, lty=2)
dev.off()
summary(xlm)
cor(zooms.to.count, quant.place.z, method="spearman")
(tau <- cor(zooms.to.count, quant.place.z, method="kendall"))
cor(zooms.to.count, mediaPorZ)

estimate.mean <- c()
estimate.sd <- c()

min.lim <- 4
max.lim <- 21

zoom.range <- min.lim:max.lim

jump.interval <- c(-8:-1,1:8)

intercept.estimates <- c()
tg.estimates <- c()

all.estimates.mean <- c()

#plot.eps("place-of-zooms", margin=c(3.6, 3.5, 1.5, 0.8))
#par(mfrow=c(2,2))
#par(cex=1.2)
for  (j in jump.interval) {
  if (j < 0) {
    zoom.range <- min.lim:max.lim
  } else {
    zoom.range <- min.lim:(max.lim - j)
  }
  
  zoos.validos <- c()
  for (i in zoom.range) {
    sub.jump <- subset(x.place.zoom, x.place.zoom[,4] == i & x.place.zoom[,5] == j
                       & x.place.zoom[,2] != 0)
    
    if (nrow(sub.jump) == 0) {
      next
    }
    zoos.validos <- c(zoos.validos, i)
    
    fit.geo <- fitdistr(abs(sub.jump[,2]), "geometric")
    
    estimate.mean <- c(estimate.mean, fit.geo$estimate[1])
    #estimate.sd <- c(estimate.sd, fit.geo$estimate[2])
    
    #plot(ecdf(abs(sub.jump[,3])), main=i, log="x", xlim=c(1, 10000000))
    #lines(ecdf(rgeom(2000, fit.geo$estimate[1])), col=2)
    #print(max(rgeom(2000, 3.86104237204425e-06)))
    #print(paste("Zoom=", i, ", Salto= ", j, ", estimate= ", fit.geo$estimate[1], sep=""))
  }
  
  y.z.out <- lm(log(estimate.mean)~as.numeric(zoos.validos))
  print(paste("Salto= ", j, sep=""))
  #print(y.z.out)
  intercept.estimates <- c(intercept.estimates, y.z.out$coefficients[1])
  tg.estimates <- c(tg.estimates, y.z.out$coefficients[2])
  
  print(estimate.mean)
  if (j < 0) {
    plot(log(estimate.mean)~as.numeric(zoos.validos), xlab="Nível de zoom", ylab="Log(p da geométrica)",
         main=paste("Z. out - tam. salto: ", abs(j), sep=""))
    abline(y.z.out, lty=2)
  } else {
    plot(log(estimate.mean)~as.numeric(zoos.validos), xlab="Nível de zoom", ylab="Log(p da geométrica)",
         main=paste("Z. in - tam. salto: ", abs(j), sep=""))
    abline(y.z.out, lty=2)
  }
  all.estimates.mean <- c(all.estimates.mean, estimate.mean)
  estimate.mean <- c()
}
#dev.off()

max(rgeom(20000, 1.334853e-03))

print(sort(all.estimates.mean))

plot(intercept.estimates)
plot(tg.estimates)

subset(x.place.zoom, x.place.zoom[,3] > 1500)

y.z.out <- lm(intercept.estimates~as.numeric(jump.interval))
y.z.out

#plot.eps("place-of-zoom-intercept")
plot(intercept.estimates~as.numeric(jump.interval), xlab="Tamanho do salto", ylab="Intercepto")
abline(y.z.out, lty=2)
#dev.off()

y.z.out <- lm(tg.estimates~as.numeric(jump.interval))
y.z.out

#plot.eps("place-of-zoom-tg")
plot(tg.estimates~as.numeric(jump.interval), xlab="Tamanho do salto", ylab="Coeficiente angular")
abline(y.z.out, lty=2)
#dev.off()
