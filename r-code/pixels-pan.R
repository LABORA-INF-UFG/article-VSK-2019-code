source("./util/util.R")

util$pkgRequire("e1071")
util$pkgRequire("MASS")
util$pkgRequire("ADGofTest")
util$pkgRequire("fitdistrplus")
util$pkgRequire("markovchain")
util$pkgRequire("xtable")
util$pkgRequire("dgof")

path = paste(PCLOUD_DIR, "/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/MovesInPixels.csv", sep="")
movesInPixels.readed = read.table(file=path, sep=",", dec=".", header=F)

head(movesInPixels.readed, 10)

tamanho = dim(movesInPixels.readed)[1]

movesInPixels <- subset(movesInPixels.readed, movesInPixels.readed[[3]] != "undefinedxundefined")

movesInPixels <- subset(movesInPixels, movesInPixels[[3]] != "24")

summary(movesInPixels[,c(1,2)])

summary(abs(movesInPixels[,1]))
length(movesInPixels[movesInPixels[,1] == 0,2])/length(movesInPixels[,2])
summary(abs(movesInPixels[,2]))

util$plot.eps("pan-sizes-pdf")
plot(density(movesInPixels[,1]), xlim=c(-1500, 1500), lwd=6, col="red", main="",
     xlab="Pan size (px)", ylab="Density", axes=F, ylim=c(0, 0.0045))
lines(density(movesInPixels[,2]), lty=2, lwd=6, col="blue")

axis(side = 1, at=c(-1500, -1000, -500, 0, 500, 1000, 1500), labels=c(1500, 1000, 500, 0, 500, 1000, 1500))
axis(side = 2)
box()

arrows(0, 0, 0, 1, code = 2, col=1, lty=2, lwd=2)
arrows(-400, 0.003, -1050, 0.003, length=0.25, angle = 30, code=2, col="red", lty=1, lwd=6)
arrows(400, 0.003, 1050, 0.003, length=0.25, angle = 30, code = 2, col="red", lty=1,lwd=6)

arrows(-400, 0.002, -1050, 0.002, length=0.25, angle = 30, code=2, col="blue", lty=1, lwd=6)
arrows(400, 0.002, 1050, 0.002, length=0.25, angle = 30, code = 2, col="blue", lty=1,lwd=6)

text(cex=1, x=-650, y=0.0033, labels="Left", xpd=TRUE)
text(cex=1, x=750, y=0.0033, labels="Right", xpd=TRUE)

text(cex=1, x=-650, y=0.0023, labels="Upward", xpd=TRUE)
text(cex=1, x=750, y=0.0023, labels="Downward", xpd=TRUE)

par(cex=1.8)
legend("topright", legend = c("X", "Y"),
       col = c("red", "blue"), pch=c(NA,NA), lty=c(1, 2), lwd=c(4, 4),
       pt.cex=2, bty="n")
dev.off()

#plot.eps("pan-sizes-pdf-comparison-simulated-x")
plot(density(movesInPixels[,1]), xlim=c(-1500, 1500), lwd=6, col=1, main="",
     xlab="Tamanho do arraste (px)", ylab="Densidade", axes=F, ylim=c(0, 0.006))
lines(density(muse.gm$simulated.pans$x), lty=2, lwd=6, col=2)

axis(side = 1, at=c(-1500, -1000, -500, 0, 500, 1000, 1500), labels=c(1500, 1000, 500, 0, 500, 1000, 1500))
axis(side = 2)
box()

arrows(0, 0, 0, 1, code = 2, col=1, lty=2, lwd=2)
arrows(-400, 0.003, -1050, 0.003, length=0.25, angle = 30, code=2, col=1, lty=1, lwd=6)
arrows(400, 0.003, 1050, 0.003, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)

text(cex=1, x=-650, y=0.0033, labels="Esquerda", xpd=TRUE)
text(cex=1, x=750, y=0.0033, labels="Direita", xpd=TRUE)

par(cex=1.8)
legend("topright", legend = c("Dados", "MUSe-GM"),
       col = c("black", "red"), pch=c(NA,NA), lty=c(1, 2), lwd=c(4, 4),
       pt.cex=2, bty="n")
#dev.off()

#plot.eps("pan-sizes-pdf-comparison-simulated-y")
plot(density(movesInPixels[,2]), xlim=c(-1500, 1500), lwd=6, col=1, main="",
     xlab="Tamanho do arraste (px)", ylab="Densidade", axes=F, ylim=c(0, 0.006))
lines(density(simulated.pans$y), lty=2, lwd=6, col=2)

axis(side = 1, at=c(-1500, -1000, -500, 0, 500, 1000, 1500), labels=c(1500, 1000, 500, 0, 500, 1000, 1500))
axis(side = 2)
box()

arrows(0, 0, 0, 1, code = 2, col=1, lty=2, lwd=2)
arrows(-400, 0.002, -1050, 0.002, length=0.25, angle = 30, code=2, col=1, lty=1, lwd=6)
arrows(400, 0.002, 1050, 0.002, length=0.25, angle = 30, code = 2, col=1, lty=1,lwd=6)

text(cex=1, x=-650, y=0.0024, labels="Para cima", xpd=TRUE)
text(cex=1, x=750, y=0.0024, labels="Para Baixo", xpd=TRUE)

par(cex=1.8)
legend("topright", legend = c("Dados", "MUSe-GM"),
       col = c("black", "red"), pch=c(NA,NA), lty=c(1, 2), lwd=c(4, 4),
       pt.cex=2, bty="n")
#dev.off()

plot(density(movesInPixels[,1]), xlim=c(-1500, 1500), col="red", lwd=4, lty=1, main="", xlab="Tamanho do arraste (pixels)", ylab="Densidade")
lines(density(movesInPixels[,2]), col="blue", lwd=4, lty=3)
legend("topright", legend=c("X-axis","Y-axis"), lty=c(1, 3), lwd=c(4, 4), col=c("red", "blue"), horiz=F)


plot(movesInPixels[,1][1:200], type="l", ylim=c(-500,500))
lines(movesInPixels[,2][1:200], col=2)

acf(movesInPixels[[2]], lag.max=300, log="y", ylim=c(0.01, 1))

all.pulse.x <- c()
all.pulse.y <- c()
all.direction.change <- c()

#par(mfrow=c(4,4))
count = 0
for (i in 1:max(movesInPixels[,6])) {
#   if (count %% 10 == 0) {
#      count <- count + 1
#      plot.eps(paste("arrastes/arraste", i, sep="-"))
#      par(mfrow=c(3,3))
#   }
  sub <- subset(movesInPixels, movesInPixels[,6] == i)
  if (dim(sub)[1] >= 2) {
    count <- count + 1

    pulse.x <- c()
    pulse.y <- c()
    direction.change <- c()
    
    for (j in 1:dim(sub)[1]) {
      if (sub[,1][j] > 0) {
        pulse.x <- c(pulse.x, 1)    
      } else if (sub[,1][j] == 0) {
        pulse.x <- c(pulse.x, 0)
      } else {
        pulse.x <- c(pulse.x, -1)
      }
      
      if (sub[,2][j] > 0) {
        pulse.y <- c(pulse.y, 1)    
      } else if (sub[,2][j] == 0) {
        pulse.y <- c(pulse.y, 0)
      } else {
        pulse.y <- c(pulse.y, -1)
      }
      
      if (length(pulse.x) > 1 & length(pulse.y) > 1)
      if (tail(pulse.x, 2)[2] == tail(pulse.x, 2)[1] & tail(pulse.y, 2)[2] == tail(pulse.y, 2)[1]) {
        direction.change <- c(direction.change, 0)
      } else if (tail(pulse.x, 2)[2] != tail(pulse.x, 2)[1] & tail(pulse.y, 2)[2] == tail(pulse.y, 2)[1]) {
        direction.change <- c(direction.change, 1)
      } else if (tail(pulse.x, 2)[2] == tail(pulse.x, 2)[1] & tail(pulse.y, 2)[2] != tail(pulse.y, 2)[1]) {
        direction.change <- c(direction.change, 1)
      } else {
        direction.change <- c(direction.change, 2)
      }
      
    }

    all.pulse.x <- c(all.pulse.x, pulse.x, 2)
    all.pulse.y <- c(all.pulse.y, pulse.y, 2)
    all.direction.change <- c(all.direction.change, direction.change, 3)
 
#     plot(pulse.x, ylim=c(-1,1), xlim=c(1,10), xlab="Arraste", ylab="", yaxt="n", type="n")
#     abline(v=seq(1,10,1), lty=3)
#     par(new=T)
#     plot(pulse.x, col="blue", lty=2, ylim=c(-1,1), xlim=c(1,10), xlab="", ylab="", yaxt="n", pch="X")
#     axis(2, at=c(-1, 0, 1), labels=c("E/PC", "0 px", "D/PB"), las=2)
#     legend(0.85, 0.8, legend=c("Eixo x","Eixo y"), pch=c("X", "O"), col=c("blue", "red"), horiz=F)
#     par(new=T)
#     plot(pulse.y, col="red", pch="O", ylim=c(-1,1), xlim=c(1,10), xlab="", ylab="", yaxt="n", xaxt="n")
  }

#    if (count %% 10 == 0) {
#      dev.off()
#    }
}

#plot(all.pulse.x, col="blue", lty=2, ylim=c(-1,1))
cor(all.pulse.x, all.pulse.y)

all.direction.change

direction.change.seq <- createSequenceMatrix(stringchar = all.direction.change)
direction.change.seq

freq.direction.change <- direction.change.seq[-4,-4]/apply(direction.change.seq[-4,-4], MARGIN=1, FUN=sum)
round(freq.direction.change, 2)

length(all.direction.change[all.direction.change == 0])/length(all.direction.change[all.direction.change != 3])

change.x <- length(all.direction.change[all.direction.change == 2 | all.direction.change == 4])/length(all.direction.change[all.direction.change != 5])
change.y <- length(all.direction.change[all.direction.change == 3 | all.direction.change == 4])/length(all.direction.change[all.direction.change != 5])
change.both <- length(all.direction.change[all.direction.change == 4])/length(all.direction.change[all.direction.change != 5])

change.x * change.y
change.both

length(all.pulse.x[all.pulse.x == -1 & all.pulse.y == 0])/length(all.pulse.x)

pulse.x.seq <- createSequenceMatrix(stringchar = all.pulse.x);pulse.x.seq
pulse.y.seq <- createSequenceMatrix(stringchar = all.pulse.y);pulse.y.seq

freq.pulse.x <- pulse.x.seq[-4,-4]/apply(pulse.x.seq[-4,-4], MARGIN=1, FUN=sum)
round(freq.pulse.x, 2)
freq.pulse.y <- pulse.y.seq[-4,-4]/apply(pulse.y.seq[-4,-4], MARGIN=1, FUN=sum)

#plot.eps("pan-direction-simulation")
par(mfrow=c(3,3))

all.synthetic.pulse.x <- c()
all.synthetic.pulse.y <- c()
all.direction.change <- c()

for (g in 1:100) {
  SIZE <- 100

  freq.frame <- as.data.frame(freq.pulse.x)
  freq.frame
  probs <- (pulse.x.seq/apply(pulse.x.seq, MARGIN=1, FUN=sum))[4,-4]
  probs
  
  (U <- runif(1))
  
  if (U <= sort(probs)[1]) {
    lastDirection <- paste(match(sort(probs)[1],probs) - 2)
  } else if (U <= sort(probs)[1] + sort(probs)[2]) {
    lastDirection <- paste(match(sort(probs)[2],probs) - 2)
  } else {
    lastDirection <- paste(match(sort(probs)[3],probs) - 2)
  }
  
  print(lastDirection)
  
  synthetic.pulse.x <-c()
  
  for (i in 1:SIZE) {
    probs <- freq.frame[lastDirection,]
    U <- runif(1)
    if (U <= sort(probs)[1]) {
      synthetic.pulse.x <- c(synthetic.pulse.x, match(sort(probs)[1],probs))
    } else if (U <= sort(probs)[1] + sort(probs)[2]) {
      synthetic.pulse.x <- c(synthetic.pulse.x, match(sort(probs)[2],probs))
    } else {
      synthetic.pulse.x <- c(synthetic.pulse.x, match(sort(probs)[3],probs))
    }
    index <- tail(synthetic.pulse.x, 1)
    lastDirection = paste(index - 2)    
  }
  
  freq.frame.y <- as.data.frame(freq.pulse.y)
  freq.frame.y
  
  probs <- (pulse.y.seq/apply(pulse.y.seq, MARGIN=1, FUN=sum))[4,-4]#prob inicial
  probs
  
  U <- runif(1)
  if (U <= sort(probs)[1]) {
    lastDirection <- paste(match(sort(probs)[1],probs) - 2)
  } else if (U <= sort(probs)[1] + sort(probs)[2]) {
    lastDirection <- paste(match(sort(probs)[2],probs) - 2)
  } else {
    lastDirection <- paste(match(sort(probs)[3],probs) - 2)
  }
  
  synthetic.pulse.y <-c()
  
  for (i in 1:SIZE) {
    probs <- freq.frame.y[lastDirection,]
    U <- runif(1)
    
    if (U <= sort(probs)[1]) {
      synthetic.pulse.y <- c(synthetic.pulse.y, match(sort(probs)[1],probs))
    } else if (U <= sort(probs)[1] + sort(probs)[2]) {
      synthetic.pulse.y <- c(synthetic.pulse.y, match(sort(probs)[2],probs))
    } else {
      synthetic.pulse.y <- c(synthetic.pulse.y, match(sort(probs)[3],probs))
    }
    
    index <- tail(synthetic.pulse.y, 1)
    lastDirection = paste(index - 2)
  }
  
  direction.change <- c()
  for (i in 1:length(synthetic.pulse.x)) {
    
    if (i > 1) {
      if (synthetic.pulse.x[i] == synthetic.pulse.x[i -1] & synthetic.pulse.y[i] == synthetic.pulse.y[i -1]) {
        direction.change <- c(direction.change, 0)
      } else if (synthetic.pulse.x[i] != synthetic.pulse.x[i -1] & synthetic.pulse.y[i] == synthetic.pulse.y[i -1]) {
        direction.change <- c(direction.change, 1)
      } else if (synthetic.pulse.x[i] == synthetic.pulse.x[i -1] & synthetic.pulse.y[i] != synthetic.pulse.y[i -1]) {
        direction.change <- c(direction.change, 1)
      } else {
        direction.change <- c(direction.change, 2)
      }
    }
  }
  
  all.synthetic.pulse.x <- c(all.synthetic.pulse.x, synthetic.pulse.x, 2)
  all.synthetic.pulse.y <- c(all.synthetic.pulse.y, synthetic.pulse.y, 2)
  all.direction.change <- c(all.direction.change, direction.change, 3)
  
#   plot(synthetic.pulse.x, ylim=c(-1,1), xlim=c(1,10), xlab="Arraste", ylab="", yaxt="n", type="n")
#   abline(v=seq(1,10,1), lty=3)
#   par(new=T)
#   plot(synthetic.pulse.x, col="blue", lty=2, ylim=c(-1,1), xlim=c(1,10), xlab="", ylab="", yaxt="n", pch="X")
#   axis(2, at=c(-1, 0, 1), labels=c("E/PC", "0 px", "D/PB"), las=2)
#   legend(0.85, 0.8, legend=c("Eixo x","Eixo y"), pch=c("X", "O"), col=c("blue", "red"), horiz=F)
#   par(new=T)
#   plot(synthetic.pulse.y, col="red", pch="O", ylim=c(-1,1), xlim=c(1,10), xlab="", ylab="", yaxt="n", xaxt="n")

}
#dev.off()

direction.change.seq <- createSequenceMatrix(stringchar = all.direction.change)
direction.change.seq

freq.direction.change <- direction.change.seq[-4,-4]/apply(direction.change.seq[-4,-4], MARGIN=1, FUN=sum)
round(freq.direction.change, 2)

length(all.direction.change[all.direction.change == 0])/length(all.direction.change[all.direction.change != 3])

round(freq.pulse.x, 2)
round(freq.pulse.y, 2)

pulse.x.seq.synt <- createSequenceMatrix(stringchar = all.synthetic.pulse.x);pulse.x.seq.synt
freq.pulse.x.seq.synt <- pulse.x.seq.synt[-4,-4]/apply(pulse.x.seq.synt[-4,-4], MARGIN=1, FUN=sum)
round(freq.pulse.x, 2)
round(freq.pulse.x.seq.synt, 2)

pulse.y.seq.synt <- createSequenceMatrix(stringchar = all.synthetic.pulse.y);pulse.y.seq.synt
freq.pulse.y.seq.synt <- pulse.y.seq.synt[-4,-4]/apply(pulse.y.seq.synt[-4,-4], MARGIN=1, FUN=sum)
round(freq.pulse.y, 2)
round(freq.pulse.y.seq.synt, 2)

#movesInPixels <- subset(movesInPixels, !(movesInPixels[[5]] %in% c("usernum.201",
#                  "usernum.214", "usernum.160")))

dim(movesInPixels)

hah <- subset(movesInPixels, movesInPixels[[1]] == 0 & movesInPixels[[2]] == 0)
#hah
dim(hah)[1] / dim(movesInPixels)[1]

statmod <- function(x) {
  z <- table(as.vector(x)); names(z)[z == max(z)]
}

n <- subset(movesInPixels, movesInPixels[[2]] != 0)[[2]]
statmod(n)

levels(as.factor(movesInPixels[[3]]))

#####################################SET X e Y##############################################

x <- subset(movesInPixels[,1], movesInPixels[,1] <= 1500 & movesInPixels[,2] <= 1500)
y <- subset(movesInPixels[,2], movesInPixels[,1] <= 1500 & movesInPixels[,2] <= 1500)
zoom <- subset(movesInPixels[,3], movesInPixels[,1] <= 1500 & movesInPixels[,2] <= 1500)
xResolution <- as.integer(subset(movesInPixels[,4], movesInPixels[,1] <= 1500 & movesInPixels[,2] <= 1500))
yResolution <- as.integer(subset(movesInPixels[,5], movesInPixels[,1] <= 1500 & movesInPixels[,2] <= 1500))

head(x[zoom == 4])
head(y)
head(zoom)

plot(density(abs(x)))
lines(density(abs(y)), col=2)

qqplot(abs(x[x < 2000]), abs(y[y < 2000]))
abline(0,1,col=2)

quantile(abs(x), 0.75)
quantile(abs(y), 0.75)

cor(abs(y), zoom, method="kendall")
cor(abs(y), yResolution, method="kendall")

cor(pulse.x, pulse.y, method="pearson")
cor(pulse.x, pulse.y, method="spearman")
cor(pulse.x, pulse.y, method="kendall")

mean(abs(x[xResolution < 1400]))

length(x[xResolution < 1400])/length(x)

mediaPorZ.x <- c()
mediaPorZ.y <- c()
quantPorZ <- c()
for (z in 1:21) {
  mediaPorZ.x <- c(mediaPorZ.x, mean(abs(x[zoom == (z)])))
  mediaPorZ.y <- c(mediaPorZ.y, mean(abs(y[zoom == (z)])))
  quantPorZ <- c(quantPorZ, quantile(abs(y[zoom == (z)]), 0.99))
}

length(y[zoom == 3])

summary(abs(x))
summary(abs(y))

mediaPorZ.y[1] = 0
mediaPorZ.x[1] = 0
mediaPorZ.y
mediaPorZ.x

par(mfrow = c(1,1))
#util$plot.eps("pans-mean-per-zoom")
plot(mediaPorZ.x, ylim=c(0, 300), xlim=c(3, 20), type="l", col=2, lwd=6, main="", xlab="Zoom level",
     ylab="Mean pan size (px)")
lines(mediaPorZ.y, col=4, lwd=6, lty=2)
legend("topleft", legend = c("X", "Y"),
       col = c("red", "blue"), pch=c(NA,NA), lty=c(1, 2), lwd=c(4, 4),
       pt.cex=2, bty="n")
#dev.off()

plot(quantPorZ, xlim=c(5,21))

zooms.to.count <- 5:21

mediaPorZ.x <- c()
mediaPorZ.y <- c()
quantPorZ <- c()
for (z in zooms.to.count) {
  mediaPorZ.x <- c(mediaPorZ.x, mean(abs(x[zoom == (z)])))
  mediaPorZ.y <- c(mediaPorZ.y, mean(abs(y[zoom == (z)])))
  quantPorZ <- c(quantPorZ, quantile(abs(y[zoom == (z)]), 0.99))
}


#util$plot.eps("zoom-pan-size-correlation-x")
(xlm <- lm(mediaPorZ.x~as.numeric(zooms.to.count)))
plot(mediaPorZ.x~as.numeric(zooms.to.count), ylim=c(0, 300), xlab="Zoom level", ylab="Mean size of pans (px)")
abline(xlm, lty=2)
#dev.off()
summary(xlm)
cor(zooms.to.count, mediaPorZ.x, method="spearman")
(tau <- cor(zooms.to.count, mediaPorZ.x, method="kendall"))
cor(zooms.to.count, mediaPorZ.x)^2

mediaPorZ <- c()
for (z in 4:20) {
  mediaPorZ <- c(mediaPorZ, mean(abs(y[zoom == (z + 1)])))
}

#util$plot.eps("zoom-pan-size-correlation-y")
(xlm <- lm(mediaPorZ.y~as.numeric(zooms.to.count)))
plot(mediaPorZ.y~as.numeric(zooms.to.count), ylim=c(0, 300), xlab="Zoom level", ylab="Mean size of pans (px)")
abline(xlm, lty=2)
#dev.off()
summary(xlm)
cor(zooms.to.count, mediaPorZ.y, method="spearman")
(tau <- cor(zooms.to.count, mediaPorZ.y, method="kendall"))
cor(zooms.to.count, mediaPorZ.y)

n <- length(mediaPorZ)
(z <- (3 * tau * sqrt(n*(n-1)))/sqrt(2*(2*n + 5)))

summary(xResolution)
head(xResolution, 3)

cor(abs(y), yResolution, method="spearman")
length(xResolution)
length(x)
plot(xResolution, abs(x))
plot(xResolution, y)
plot(yResolution, abs(y))
plot(yResolution, x)

plot(density(log(abs(y))))

max(abs(x))
max(abs(y))

########################################################
plot(density(x), col=2, lwd=2, xlim=c(-1500, 1500), xlab="Pixels", ylab="% of Total", main="")
lines(density(y), col=3, lwd=2)

(subset(movesInPixels, movesInPixels[[3]] == 2))

pansMean <- data.frame(x=c(1:21), y=c(1:21), qt=0)
pansMean[[2]][5]

QUANTILE <- 1

plot(ecdf(abs(x)), main=paste("Zoom", i), lwd=1, lty=1)
lines(ecdf(abs(y)), col="red", lwd=1, lty=1)

estimate <- c()
quants <- c()
zooms.to.count <- 15:16
#util$plot.eps.config.paper("pan-size-and-fitted-geom", margin=c(3.6, 3.5, 1.5, 0.8), width = 16, height = 4)
par(mfrow=c(1, 2), cex=1.05)

for (i in zooms.to.count) {
  sub <- subset(movesInPixels, movesInPixels[[3]] == i)
  x.sub <- abs(sub[[1]])  
  #print(paste("x: zoom ", i,sd(x.sub) / mean(x.sub)), sep="")
  pansMean[[1]][i]=mean(x.sub)
  y.sub <- abs(sub[[2]])
  plot(ecdf(y.sub), main=paste("Zoom", i), xlab="Pan size (px)", 
       ylab=expression("Prob(Pan size " <= " x)"), xlim=c(0, 2500))
  logn.p <- fitdistr(x.sub, "geometric");logn.p
  logn <- rgeom(8000, -0.0002397 * i + 0.0093273)
  print(paste("Zoom =", i, sep=" "))
  print(paste("var dos dados: ", sd(x.sub + 1)))
  print(paste("var da geom: ", sd(logn)))
  
  quants <- c(quants, quantile(x.sub, 0.5))
  
  # if (length(y.sub) > 1000) {
  #   print(ad.test(sample(y.sub, 500), pgeom, -0.0002038 * i + 0.0085857))
  # } else {
  #   print(ad.test(y.sub, pgeom, -0.0002038 * i + 0.0085857))
  # }
  
  print(chisq.test(x.sub, p=c(logn.p$estimate[[1]], 1-logn.p$estimate[[1]])))
  #print(paste(logn.p$estimate[[1]]))
  estimate <- c(estimate, logn.p$estimate[[1]])
  lines(ecdf(logn), col="red")
  #print(paste("y: zoom ", i,sd(y.sub) / mean(y.sub)), sep="")
  pansMean[[2]][i]=mean(y.sub)
  pansMean[[3]][i]=length(x.sub) / length(x)
}
dev.off()

util$pkgRequire("KSgeneral")

print.chisq = function(zooms.to.count, axis=1, rounds=30) {
  results = data.frame(z=c(), chisq.test.result=c())
  for (i in 1:rounds) {
    for (z in zooms.to.count) {
      sub <- subset(movesInPixels, movesInPixels[[3]] == z)
      sub <- abs(sub[[axis]])  
      
      values = as.numeric(names(table(sub)))
      sub.table = table(sub)
      
      if (axis == 1) {
        prob = -0.0001376 * z + 0.0079955
        probs.geom <- dgeom(values, p=prob)
        gen.geom = rgeom(1000, prob = prob)
      } else {
        prob = -0.0002038 * z + 0.0085857
        probs.geom <- dgeom(values, p=prob)
        gen.geom = rgeom(1000, prob = prob)
      }
      
      print(paste("z = ", z, sep=""))
      
      ecdfPlot(sub, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Interval " <= " x)"), 
               xlab="Interval (s)", main=z)
      ecdfPlot(gen.geom, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, add=T)
      
      #gen.geom
      test.result = dgof::ks.test(sub, ecdf(gen.geom), simulate = T, B=10000)
      #test.result = KSgeneral::disc_ks_test(sub, ecdf(gen.geom), exact = T)
      print(test.result)
      results = rbind(results, data.frame(z, test.result$p.value))
    }
  }
  results
}

chisq.results = print.chisq(5:21, 2, 30)
chisq.results[chisq.results == 18,2]
mean(chisq.results[chisq.results == 9+4,2])
ks.resuts = lapply(5:21, function(x) mean(chisq.results[chisq.results == x,2]))
for (l in ks.resuts) {
  print(l)
}


ks.test(rep(1, 3), ecdf(1:3), simulate=TRUE, B=10000)
?ks.test
length(x[zoom == 6])/length(x)

table(rgeom(1000, p=-0.0001376 * 17 + 0.0079955))

dgof::ks.test(rgeom(10000, 0.01), ecdf(rgeom(10000, 0.01)), exact = T)
KSgeneral::disc_ks_test(rgeom(10000, 0.01), ecdf(rgeom(10000, 0.01)), exact = T)

util$pkgRequire("EnvStats")

xpto = 17

estimate[xpto - 5]

0.0094689 -0.0002092*xpto

estimate
par(mfrow=c(1,1))

par(mfrow=c(1, 1))
util$plot.eps("pans-geom-estimation-x")
(estimate.lm = lm(estimate~as.numeric(zooms.to.count)))
pl <- plot(estimate~as.numeric(zooms.to.count), xlab="Zoom level", ylab="Estimation of p")
abline(estimate.lm, lty=2)
dev.off()
summary(estimate.lm)
estimate.lm
cor(zooms.to.count, estimate, method="spearman")
(tau <- cor(zooms.to.count, estimate, method="kendall"))
cor(zooms.to.count, estimate)


quants.lm = lm(quants~as.numeric(zooms.to.count))
plot(quants~as.numeric(zooms.to.count), xlab="Nível de zoom", ylab="Média do tamanho dos arrastes (px)", ylim=c(0, max(quants) + 10))
abline(quants.lm, lty=2)
summary(quants.lm)
cor(zooms.to.count, quants, method="spearman")
(tau <- cor(zooms.to.count, quants, method="kendall"))
cor(zooms.to.count, quants)

estimate <- c()
quants <- c()
zooms.to.count <- 17:17
#plot.eps("pans-geom-y-17", margin=c(3.6, 3.5, 1.5, 0.8))
par(mfrow=c(1, 2), cex=1.1)

for (i in zooms.to.count) {
  sub <- subset(movesInPixels, movesInPixels[[3]] == i)
  x.sub <- abs(sub[[1]])  
  #print(paste("x: zoom ", i,sd(x.sub) / mean(x.sub)), sep="")
  pansMean[[1]][i]=mean(x.sub)
  y.sub <- abs(sub[[2]])
  plot(ecdf(y.sub), main=paste("Zoom", i), xlab="Tam. do arraste (px)", ylab="% do total", xlim=c(0, 2500))
  #logn <- rgeom(10000, (-0.0001376 * i + 0.0079955))
  logn <- rgeom(1000, (-0.0002038 * i + 0.0085857))
  print(paste("Zoom =", i, sep=" "))
  print(paste("sd dos dados: ", sd(y.sub + 1)))
  print(paste("sd da geom: ", sd(logn)))
  
  quants <- c(quants, quantile(y.sub, 0.5))
  
  if (length(x.sub) > 1000) {
    #print(ks.test(sample(y.sub, 500), "pgeom", logn.p$estimate[[1]]))
  } else {
    #print(ks.test(y.sub, "pgeom", logn.p$estimate[[1]]))
  }
  #print(ad.test(y.sub, pexp, logn.p$estimate[[1]]))
  #print(paste(logn.p$estimate[[1]]))
  estimate <- c(estimate, logn.p$estimate[[1]])
  lines(ecdf(logn), col="red")
  #print(paste("y: zoom ", i,sd(y.sub) / mean(y.sub)), sep="")
  pansMean[[2]][i]=mean(y.sub)
  pansMean[[3]][i]=length(x.sub) / length(x)
}
#dev.off()
