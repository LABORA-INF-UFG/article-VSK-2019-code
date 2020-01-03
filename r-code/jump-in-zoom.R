library(MASS)
QUANTILE=0.97

path = paste(PCLOUD_DIR, "/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/JumpInZoom.csv", sep="")
jumpInZoom = scan(path, sep=",", dec=".")

head(jumpInZoom)

hist(jumpInZoom, freq=FALSE)

quantile(abs(jumpInZoom[jumpInZoom < 0]), 0.9)

intervalJump <- c(-15:15)

jumpInZoom.freq <- 1:31 - 1:31

for (i in 1:length(jumpInZoom)) {
  jumpSize <- jumpInZoom[i] + 16
  jumpInZoom.freq[jumpSize] <- jumpInZoom.freq[jumpSize] + 1
}

summary(jumpInZoom)

jumpInZoom.freq
length(3:17 - 1:15)
util$plot.eps("zooms-jump-barplot")
plot(jumpInZoom.freq/sum(jumpInZoom.freq), type="h", log="y", lwd=13, col=c(2:16 - 1:15, 3:18 - 1:16), xaxt="n", yaxt="n", 
     xlab="Jump size", ylab="Frequency")
legend("topright", legend = c("Zoom Out", "Zoom In"),
       col = c(1, 2), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", cex=0.9)
axis(1, at=c(1, 5, 10, 15, 17, 21, 26, 31),labels=c(15, 10, 5, 1, 1, 5, 10, 15), col.axis="black", las=2)
axis(2, at=c(1e-4, 1e-3, 1e-2, 1e-1),labels=c("1e-4", "1e-3", "1e-2", "1e-1"), col.axis="black", las=3)
dev.off()

# plot.eps("zooms-jump-barplot", margin=c(0,0,0,0), oma=c(1.5,2,1,1), pin=c(9, 4), width=11, heigth=6)
# bp <- barplot(jumpInZoom.freq/sum(jumpInZoom.freq), ylim=c(0, 0.40), xlab="Tamanho do salto", ylab="% do total",
#               col=c("red"), las=2, cex.names=0.9)
# axis(1, at=c(0.5, 6.5, 12.75, 17.5, 19.9, 24.8, 30.75, 36.75),labels=c(-15, -10, -5, -1, 1, 5, 10, 15), col.axis="black", las=2)
# dev.off()

path = paste(PCLOUD_DIR, "/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/JumpInZoomByZoom.csv", sep="")
jumpInZoom = read.table(path, sep=",", dec=".")

head(jumpInZoom)

num.access <- as.vector(jumpInZoom[[1]])
zooms <- as.vector(jumpInZoom[[2]])
jumps <- as.vector(jumpInZoom[[3]])
user.names <- as.vector(jumpInZoom[[4]])

length(as.factor(user.names[zooms == 10 & jumps==-1]))
length(levels(as.factor(user.names[zooms == 10 & jumps==-1])))

par(mfcol=c(1,1))
plot(density(jumps[zooms == 10 & user.names=="usernum.235"]))

jumps[zooms == 10 & num.access==2]

jumps.filtered <- c()

for (i in 1:max(num.access)) {
  if (length(jumps[num.access==i & zooms==10]) > 0) {
    print(jumps[num.access==i])
  }
  jump.in.access <- jumps[num.access==i]
  for (j in 1:length(jump.in.access)) {
    for (z in 1:21) {
      jumps.per.zoom <- jumps[num.access==i & zooms=z]
    }
  }
}

# count <- 1
# filter.acesses.byzoom <- function(usersArg, zoom, result=data.frame()) {
#   #result <- data.frame(users=c(), times=c())
#   print(class(usersArg[count]))
#   for (z in zoomsAccessed) {
#     if (z == zoom) {
#       result <- data.frame(users=c(result$users, usersArg[count]),
#                            times=c(result$times, zoomsAccessedTimes[count]),
#                            stringsAsFactors=FALSE)
#     }
#     count <- count + 1
#   }
#   result
# }
# 
# black.list <- c()
# 
# for (z in 1:21) {
#   by.zoom <- filter.acesses.byzoom(user.names, z)
#   quant <- quantile(by.zoom[[2]], QUANTILE)
#   indexes <- which(by.zoom[[2]] > quant)
#   print(indexes)
#   for (index in indexes) {
#     black.list <- c(black.list, by.zoom[[index, 1]])
#   }
# }
# 
# black.list


util$plot.pdf("zooms-jump-scatter")
plot(zooms, jumps,
     xlab="Zoom level", ylab="Jump size", pch=19, col=rgb(0,100,0,50,maxColorValue=255), yaxt="n")
axis(2, at=c(-15, -10, -5, 0, 5, 10, 15), labels=c("15", "10", "5", "0", "5", "10", "15"), col.axis="black", las=0)
arrows(-1, 0, 22, 0, length=0.25, code = 0, col=1, lty=2, lwd=2)
arrows(21, 0, 21, 15, length=0.25, code = 2, col=2, lty=1, lwd=6)
text(cex=1.3, x=17, y=10, labels="Zoom In", srt=0, xpd=TRUE)
arrows(0, 0, 0, -15, length=0.25, code = 2, col=1, lty=1, lwd=6)
text(cex=1.3, x=5, y=-7, labels="Zoom Out", srt=0, xpd=TRUE)
dev.off()


scatter.smooth(zooms, jumps, xlab="Zoom Levels", ylab="Jump")

for (i in 0:20) {
  print(1 - i*(1/20))
}

l.max <- 21
lambda.arg <- 3
omega.arg <- 0.1

zoomIn <- function(l, lambda) {
  1 - (l/l.max)^lambda
}

zoomOut <- function(l, w, lambda) {
  w * (l/l.max)^lambda
}

zoomOut(15, 0.4, .5)
zoomIn(10, 1.1)

head(subset(jumpInZoom, jumpInZoom[[3]] > 0))

lambda.arg = 0.5
please <- c()

contador <- 0

for (v in 2:21) {
  numVals <- 1000
  vals <- c()
  lambda.arg = lambda.arg + 0.15
  for (i in 1:numVals) {
    x1 <- runif(1, 0, 1)
    if (x1 >= zoomIn(v, lambda.arg)) {
      vals <- c(vals, max((-(rgeom(1, 0.5) + 1)), 0 - v))
    } else {
      vals <- c(vals, min((rgeom(1, 0.5) + 1), abs(v - l.max)))
    }
  }
  
  #length(vals[vals < 0])/length(vals)
  if ((v - 2) %% 4 == 0) {
    plot.eps(paste("jumps-by-zoom", v, v + 4, sep="-"), margin=c(3.6, 3.5, 3, 0.8))
    par(mfrow=c(1,1))
    par(mfcol=c(4,2))
    par(cex=0.8)
  }
  jumps <- subset(jumpInZoom, jumpInZoom[[2]] == v)
  
  min.lim <- min(vals)
  max.lim <- max(vals)
  
  if (min(jumps[[3]]) < min.lim) {
    min.lim <- min(jumps[[3]])  
  }
  if (max(jumps[[3]]) > max.lim) {
    max.lim <- max(jumps[[3]])  
  }
  
  plot(table(vals) / numVals, main=paste("Zoom", v, sep=" "), new=T, type="h", xlab="Tamanho do salto", ylab="Frequência",
       xlim=c(min.lim, max.lim))
  
  
  please <- c(please, (dim(subset(jumps, jumps[[3]] < 0))[1]/dim(jumps)[1]))
  plot(table(jumps[[3]]) / length(jumps[[3]]), col=2, type="h", main=paste("Zoom", v, sep=" "),
       xlab="Tamanho do salto", ylab="Frequência", xlim=c(min.lim, max.lim))
  
  contador <- contador + 1
  if (contador %% 4 == 0) {
    dev.off()
  }
}


warnings()
head(please)
par(mfrow=c(1,1))
plot(please)
cor(please, 1:21, method="spearman")
lm(please~as.numeric(1:21))

i = 19
jumpsIn17 <- subset(jumpInZoom, jumpInZoom[[2]] == (i))
plot(table(jumpsIn17[[3]]) / length(jumpsIn17[[3]]), type="h", main=paste("Zoom", i, sep=" "))



library(hexbin)

# plot.eps("zooms-jump-scatter")
# bin<-hexbin(zooms, jumps, xbins=30)
# plot(bin, main="Hexagonal Binning")
# dev.off()

xpto <- c()

dev.off()

for (i in 2:21) {
  jumpsIn17 <- jumpInZoom
  jumpsIn17 <- subset(jumpsIn17, jumpsIn17[[2]] == i)
  
  #print(paste("Zoom", i, sep=" "))
  #print((dim(subset(jumpsIn17, jumpsIn17[[3]] < 0))[1] + 1)/ (dim(subset(jumpsIn17, jumpsIn17[[3]] > 0))[1] + 1))
  
  #xpto <- c(xpto, (dim(subset(jumpsIn17, jumpsIn17[[3]] < 0))[1] + 1) / (dim(subset(jumpsIn17, jumpsIn17[[3]] > 0))[1] + 1))
  plot(table(abs(jumpsIn17[[3]])) / length(jumpsIn17[[3]]), type="h", main=paste("Zoom", i, sep=" "))
  #plot(density(subset(jumpsIn17, jumpsIn17[[3]] < 0)[[3]]), main=paste("Zoom", i, "< 0", sep=" "))
  #plot(density(subset(jumpsIn17, jumpsIn17[[3]] > 0)[[3]]), main=paste("Zoom", i, "> 0", sep=" "))
}

par(mfrow=c(1,1))
plot(xpto, type="l", xlim=c(1,19), ylim=c(0, 4))

jumpsIn17 <- subset(jumpInZoom, jumpInZoom[[2]] == 17)

min <- min(jumpsIn17[[3]])
max <- max(jumpsIn17[[3]])

jumpsIn17[[3]] <- jumpsIn17[[3]] + abs(min)

plot(density(jumpsIn17[[3]]), main=paste("Zoom", i, sep=" "))

max
mean(jumpsIn17[[3]])

wei <- fitdistr(jumpsIn17[[3]], "weibull")
wei
plot(density(rweibull(1000, 20, 21)))

#####################################################################

l.min <- 2
l.max <- 21

zl.special <- 17

e <- exp(1)

jumpSize <- function(l, zl, lambda) {
  if (lambda >= zl.special) {
    (zl/lambda)*(l/lambda)^(zl-1)*e^(-(l/lambda)^zl)
  } else {
    (zl/lambda)*(l/lambda)^(zl-1)*e^(-(l/(lambda))^zl)
  }
}

library(ghyp)

#lambda e alpha.bar = shape parameters
#mu = location parameter
#sigma = scale parameter
#gamma = skewness parameter

zzl <- 11

plot(density(rghyp(10000, ghyp(lambda=(zzl - 15), alpha.bar=(21 - zzl), sigma=1, gamma=((15)-zzl), mu=zzl))),
     main=paste("Zoom: ", zzl, sep=" "))

for (i in l.min:l.max) {  
  numVals <- 10000
  dvals <- rghyp(numVals, ghyp(lambda=0, alpha.bar=1, sigma=1, gamma=((15)-i), mu=i))
  plot(density(dvals), main=paste("Zoom: ", i, sep=" "))
}

data <- jumpsIn17[[3]] + max(abs(jumpsIn17[[3]]))

data <- sample(data, 200)

hy.fit <- fit.ghypuv(data)

attributes(hy.fit)

plot(density(rghyp(10000, ghyp(lambda=1, sigma=1, gamma=0, mu=21))),
     main=paste("Zoom: ", 17, sep=" "))
