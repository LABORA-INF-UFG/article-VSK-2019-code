source('./util/util.R')
util$pkgRequire('e1071')
util$pkgRequire('MASS')
util$pkgRequire('ADGofTest')

filename = "ZoomLevelFrequency.csv"
zooms.freq.readed = util$loadData(filename, separator=",")

(ind<-sort(zooms.freq.readed[[1]], index.return=TRUE))
sort(ind$ix)

zooms.freq <- zooms.freq.readed[[2]]

zooms.ord <- c()
for (i in ind$ix) {
  zooms.ord <- c(zooms.ord, zooms.freq[i])
}
zooms.ord

zoom.frequency.data <- data.frame(zoom.levels=1:21, frequency=zooms.ord)
head(zoom.frequency.data, 25)
util$writeToFile(zoom.frequency.data, "zoom-frequency-data.dat")

plot.eps("zooms-frequency", margin=c(3.6, 3.5, 1, 0.1))
bp <- barplot(zooms.ord/sum(zooms.ord), ylim=c(0, 0.20), names.arg=ind$x, xlab="Nível de zoom", ylab="Frequência",
              col=c("red"), las=2, cex.names=0.9)
dev.off()


filename = "ZoomLevelPerAccessFrequency-bkp.csv"
zooms.freq.readed = util$loadData(filename, separator=" ")

head(zooms.freq.readed, 30)
dim(zooms.freq.readed)
zooms.freq.readed[2,]

zooms.freq.per.access <- zooms.freq.readed

for (a in 1:dim(zooms.freq.readed)[1]) {
  if (sum(zooms.freq.readed[a,])) {
    zooms.freq.per.access[a,] = zooms.freq.readed[a,] - zooms.freq.readed[a,]
  }
}

head(zooms.freq.per.access, 3)

total <- 0
for (a in 1:dim(zooms.freq.readed)[1]) {
  if (sum(zooms.freq.readed[a,]) > 2) {
    zooms.freq.per.access[a,] = zooms.freq.readed[a,]/sum(zooms.freq.readed[a,])
    total <- total + 1
  }
}

head(zooms.freq.per.access, 3)

zooms.freq.per.access[is.na(zooms.freq.per.access)]

zooms.freq <- c()

for (z in 1:21) {
  soma <- 0
  for (a in 1:dim(zooms.freq.readed)[1]) {
    soma <- soma + zooms.freq.per.access[a,z]
  }
  print(soma)
  zooms.freq <- c(zooms.freq, soma / total)
}

zooms.freq[4]

util$plot.eps("zooms-frequency2", margin=c(3.6, 3.5, 1, 0.1))
bp <- barplot(zooms.freq, ylim=c(0, 0.20), names.arg=ind$x, xlab="Zoom level", ylab="Frenquency",
              col=c("red"), las=3, cex.names=0.9)
dev.off()
sum(zooms.freq)

