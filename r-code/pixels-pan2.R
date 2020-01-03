source("./util/util.R")

util$pkgRequire("e1071")
util$pkgRequire("MASS")
util$pkgRequire("ADGofTest")
util$pkgRequire("fitdistrplus")
util$pkgRequire("markovchain")
util$pkgRequire("xtable")

path = paste(PCLOUD_DIR, "mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/MovesInPixels.csv", sep="/")
movesInPixels.readed = read.table(file=path, sep=",", dec=".", header=F)

head(movesInPixels.readed, 10)

tamanho = dim(movesInPixels.readed)[1]

movesInPixels <- subset(movesInPixels.readed, movesInPixels.readed[[3]] != "undefinedxundefined")

movesInPixels <- subset(movesInPixels, movesInPixels[[3]] != "24")

plot(movesInPixels[,1][1:200], type="l", ylim=c(-500,500))
lines(movesInPixels[,2][1:200], col=2)

acf(movesInPixels[[2]], lag.max=300, log="y", ylim=c(0.01, 1))

all.directions <- c()

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
    
    for (j in 1:dim(sub)[1]) {
      if (sub[,1][j] > 0 & sub[,2][j] > 0) {
        all.directions <- c(all.directions, "D/PB")    
      } else if (sub[,1][j] > 0 & sub[,2][j] == 0) {
        all.directions <- c(all.directions, "D/0")
      } else if (sub[,1][j] > 0 & sub[,2][j] < 0) {
        all.directions <- c(all.directions, "D/PC")
      } else if (sub[,1][j] < 0 & sub[,2][j] > 0) {
        all.directions <- c(all.directions, "E/PB")
      } else if (sub[,1][j] < 0 & sub[,2][j] == 0) {
        all.directions <- c(all.directions, "E/0")
      } else if (sub[,1][j] < 0 & sub[,2][j] < 0) {
        all.directions <- c(all.directions, "E/PC")
      } else if (sub[,1][j] == 0 & sub[,2][j] > 0) {
        all.directions <- c(all.directions, "0/PB")
      } else if (sub[,1][j] == 0 & sub[,2][j] < 0) {
        all.directions <- c(all.directions, "0/PC")
      } else {
        print("estranho")
      }
      
    }
    
    all.directions <- c(all.directions, "start")
    
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

all.directions.seq <- createSequenceMatrix(stringchar = all.directions)
all.directions.seq

all.directions.freq <- all.directions.seq/apply(all.directions.seq, MARGIN=1, FUN=sum);all.directions.freq
all.directions.freq <- (all.directions.seq[,-9]/apply(all.directions.seq[,-9], MARGIN=1, FUN=sum))
round(all.directions.freq, 4)
sum(all.directions.freq)


all.directions.latex <- (all.directions.seq[,-9]/apply(all.directions.seq[,-9], MARGIN=1, FUN=sum))

round(all.directions.latex, 4)
sum(all.directions.latex)
all.directions.latex

xtable(all.directions.latex, digits=3)

#plot.eps("pan-direction-simulation")
#par(mfrow=c(3,3))

all.direction.synt <- c()

for (g in 1:1000) {
  SIZE <- 100
  
  freq.frame <- as.data.frame(all.directions.freq)
  freq.frame
  probs.init <- (all.directions.seq/apply(all.directions.seq, MARGIN=1, FUN=sum))[7,-7]
  round(probs.init,2)
  
  lastDirection <- ""
  
  (U <- runif(1))
  for (i in 1:length(probs.init)) {
    probs.sorted <- sort(probs.init)
    
    if (U < sum(probs.sorted[1:i])) {
      lastDirection <- rownames(as.matrix(probs.sorted))[i]
      break
    }
  }
  
  for (i in 1:SIZE) {
    probs <- freq.frame[lastDirection,]
    probs.sorted <- sort(probs)
    
    U <- runif(1)    
    for (j in 1:length(probs.sorted)) {
      if (U < sum(probs.sorted[1:j])) {
        all.direction.synt <- c(all.direction.synt, colnames(probs.sorted)[j])
        lastDirection <- colnames(probs.sorted)[j]
        break
      }
    }
  }
  
  all.direction.synt <- c(all.direction.synt, "END")  
}

head(all.directions, 25)
tail(all.direction.synt, 20)

all.direction.synt.seq <- createSequenceMatrix(stringchar = all.direction.synt)
all.direction.synt.freq <- all.direction.synt.seq[-7,-7]/apply(all.direction.synt.seq[-7,-7], MARGIN=1, FUN=sum)
round(all.directions.freq, 2)
round(all.direction.synt.freq, 2)
sum(diag(all.direction.synt.freq))/8

xtable(all.directions.freq, digits=4)

0.0025 + 0.0100 + 0.0017 + 0.2076 + 0.1528 + 0.0050 + 0.1703 + 0.4501
0.0074 + 0.0067 + 0.0030 + 0.1743 + 0.1921 + 0.0022 + 0.4629 + 0.1514
0.0952 + 0.0952 + 0.0952 + 0.0952 + 0.0952 + 0.1429 + 0.1429 + 0.2382
0.0035 + 0.0095 + 0.0052 + 0.1482 + 0.4454 + 0.0017 + 0.2166 + 0.1699
0.0058 + 0.0025 + 0.0017 + 0.4332 + 0.1428 + 0.0042 + 0.1828 + 0.2270
0.1176 + 0.0588 + 0.0588 + 0.1765 + 0.2353 + 0.0000 + 0.1765 + 0.1765
0.1930 + 0.1930 + 0.0000 + 0.0877 + 0.1228 + 0.0175 + 0.1579 + 0.2281
0.2800 + 0.1400 + 0.0400 + 0.1400 + 0.0800 + 0.0200 + 0.1600 + 0.1400
