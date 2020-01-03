util$pkgRequire("e1071")

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/MovementsSize.csv"
movesInPixels.readed = read.table(file=path, sep=",", dec=".", header=F)

head(movesInPixels.readed, 3)

tamanho = dim(movesInPixels.readed)[1]

movesInPixels <- subset(movesInPixels.readed, !is.na(movesInPixels.readed[[3]]))

head(movesInPixels, 3)
#movesInPixels <- subset(movesInPixels, movesInPixels[[3]] != "24")

#movesInPixels <- subset(movesInPixels, !(movesInPixels[[5]] %in% c("usernum.201",
#                  "usernum.214", "usernum.160")))

dim(movesInPixels)[1] / dim(movesInPixels.readed)[1]

hah <- subset(movesInPixels, movesInPixels[[1]] == 0)
#hah
dim(hah)[1] / dim(movesInPixels)[1]

statmod <- function(x) {
  z <- table(as.vector(x)); names(z)[z == max(z)]
}

n <- subset(movesInPixels, movesInPixels[[1]])[[1]]
statmod(movesInPixels[[2]])

levels(as.factor(movesInPixels[[3]]))

x <- movesInPixels[[1]]
y <- movesInPixels[[2]]
zoom <- movesInPixels[[3]]

plot(density(abs(x)))
lines(density(abs(y)), col=2)

mediaPorZ.mov <- c()
for (z in 2:20) {
  mediaPorZ.mov <- c(mediaPorZ.mov, mean(abs(y[zoom == (z + 1)])))
}

xlm <- lm(mediaPorZ.mov~as.numeric(3:21))
plot(mediaPorZ.mov~as.numeric(3:21), ylim=c(0, 300), xlab="Nível de zoom", ylab="Média do tamanho dos arrastes (px)")
abline(xlm, lty=2)
xlm
mediaPorZ.mov
cor(abs(x), abs(y), method="kendall")

qqplot(abs(x), abs(y))
quantile(abs(x), 0.9)
quantile(abs(y), 0.9)
abline(0,1, col=2)

val.x <- -3.1

(val.x + 3) / (val.x - 1)

val.x * 2 - 3
val.x + 4
val.x * 3 - 2

for (i in 1:length(x)) {
  if (x[i] == y[i]) {
    print(paste(x[i], zoom[i], sep="-"))
  }
}

mean(abs(x))
mean(abs(y))

##Achar a relação entre o tamanho do arraste em y com o tamanho da tela.

xx <- subset(x, x <= (max(y) + 30))
xx <- subset(xx, xx >= (min(y) - 30))

x <- xx
length(xx) / length(x)
length(y[y == 0]) / length(y)

mean(abs(x))
mean(abs(y))

########################################################
plot(density(x), col=2, lwd=2, xlim=c(-1500, 1500), xlab="Pixels", ylab="% of Total", main="")
lines(density(y), col=3, lwd=2)

(subset(movesInPixels, movesInPixels[[3]] == 2))

pansMean <- data.frame(x=c(1:21), y=c(1:21), qt=0)
pansMean[[2]][5]

QUANTILE <- 1

plot(ecdf(abs(x)), main=paste("Zoom", i), lwd=1, lty=1)
lines(ecdf(abs(y)), col="red", lwd=1, lty=1)

for (i in 2:21) {
  sub <- subset(movesInPixels, movesInPixels[[3]] == i)
  x.sub <- abs(sub[[1]])  
  x.sub <- x.sub[x.sub < quantile(x.sub,QUANTILE)]
  print(paste("x: zoom ", i,sd(x.sub) / mean(x.sub)), sep="")
  pansMean[[1]][i]=mean(x.sub)
  y.sub <- abs(sub[[2]])
  plot(density(x.sub), ylim=c(0, 0.007), main=paste("Zoom", i))
  lines(density(y.sub), col="red")
  y.sub <- y.sub[y.sub < quantile(y.sub,QUANTILE)]
  print(paste("y: zoom ", i,sd(y.sub) / mean(y.sub)), sep="")
  pansMean[[2]][i]=mean(y.sub)
  pansMean[[3]][i]=length(x.sub) / length(x)
}

pansMean

plot(1, type="n", xlim=c(4, 21), ylim=c(min(pansMean[[2]]), max(pansMean[[2]])))
lines(pansMean[[1]])
lines(pansMean[[2]], col="red")

########################################################

yy <- sample(y, 4372)

plot(x, y, 
     xlab="x (pixels)", ylab="y (pixels)", pch=19, col=rgb(0,100,0,50,maxColorValue=255),
     )

library(hexbin)

bin<-hexbin(x, yy, xbins=30)
plot(bin)

plot(density(x))
plot(density(y))

summary(x)
summary(y)

lap <- rlaplace(n=length(y),location=50, scale=180)

laplace.test(y)

plot(density(lap))
ks.test(y, lap)
qqplot(y, lap)
abline(0, 1, col="red")

############################################
nLL <- function(location, scale) -sum(dlaplace(y, location, scale, log = TRUE))
fit0 <- mle(nLL, start = list(location=1, scale=800), nobs = NROW(y))
stopifnot(nobs(fit0) == length(y))
(coef <- coef(fit0))
lap <- rlaplace(n=length(y),location=coef[1][1], scale=coef[2])
plot(density(lap))
ks.test(y, lap)
qqplot(y, lap)
abline(0, 1, col="red")
############################################

parameters = fitdistr(x[x > 0], "laplace")$estimate
parameters
poi <- rweibull(n=length(x[x > 0]), parameters[1], parameters[2])
plot(density(poi))
ks.test(x[x > 0], poi)

parameters = fitdistr(y[y > 0], "exponential")$estimate
parameters
poi <- rexp(n=length(y[y > 0]), parameters[1])
plot(density(poi))
ks.test(y, poi)

library(e1071)

kurtosis(x)
kurtosis(y)

print(parameters)

hx <- rnorm(mean=-4.565257, sd=235.400309, n=tamanho)

hist(hx)

qqnorm(xvalues); qqline(xvalues, col=2)
qqplot(xvalues, hx); abline(0, 1)

seno <- function (g) {
  sin((g / 180) * pi)
}

cosseno <- function (g) {
  cos((g / 180) * pi)
}

tang <- function(g) {
  tan((g / 180) * pi)
}

seno(30)
cosseno(60)
tang(270)
sqrt(3)
2 / sqrt(2)
sqrt(2)
