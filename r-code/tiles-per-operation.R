library(EnvStats)

path = "/media/arquivos/Dropbox/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/TilesPerPanWithCacheAndPrefetching.csv"
tiles.per.pan.file = read.table(file=path, sep=",", dec=".")
head(tiles.per.pan.file)

####################################################PAN - NO CACHE################################################################
plot.eps.config.paper("tiles-per-pan-with-no-cache", margin=c(3.5,4,.5,0), paper="special", width=11, height=4)
par(cex=1.6)
tiles.per.pan <- (table(tiles.per.pan.file[,2])/dim(tiles.per.pan.file)[1])

round(tiles.per.pan, 2)

bp <- barplot(tiles.per.pan,
              xlab="Número de tiles", ylab="",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.pan)/10, 0.5),
              width=1, xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(tiles.per.pan), lty=1, las=2)
mtext(side=2, text = "Frequência", line=3, cex=1.6)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 5e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "5e-1"), lty=1, las=1)
dev.off()


####################################################PAN - NO CACHE################################################################

plot.eps.config.paper("tiles-per-pan-with-cache", margin=c(3.5,3.5,1.5,0), paper="special", width=11, height=4)
par(cex=1.5)
tiles.per.pan <- (table(tiles.per.pan.file[,1])/dim(tiles.per.pan.file)[1])

sum(tiles.per.pan[26:length(tiles.per.pan)])

bp <- barplot(tiles.per.pan,
              xlab="Número de tiles", ylab="Frequência",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.pan)/10, 0.5),
              width=1, xaxt="n")
par(cex=1.4)
axis(1, at=bp, labels=rownames(tiles.per.pan), las=2)
dev.off()

####################################################PAN - CDF################################################################
plot.eps("tiles-per-pan-cdf")
par(cex=2)
tiles.per.pan <- (table(tiles.per.pan.file[,2])/dim(tiles.per.pan.file)[1])

ecdfPlot(tiles.per.pan.file[,1], ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Número de tiles" <= " x)"), 
         xlab="Número de tiles", main="", xlim=c(min(tiles.per.pan.file[,1]), 66))
ecdfPlot(tiles.per.pan.file[,2], ecdf.lty=3, ecdf.lw=6, ecdf.col=2, main="", add=T)
legend("bottomright", legend = c("Com cache", "Sem cache"),
       col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()


####################################################ZOOM###########################################################################

path = "/media/arquivos/Dropbox/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/TilesPerZoomWithAndWithoutCache.csv"
tiles.per.zoom.file = read.table(file=path, sep=",", dec=".")
head(tiles.per.zoom.file)

####################################################ZOOM - NO CACHE################################################################
plot.eps.config.paper("tiles-per-zoom-with-no-cache", margin=c(3.5,4,.5,0), paper="special", width=11, height=4)
par(cex=1.6)
tiles.per.zoom <- (table(tiles.per.zoom.file[,2])/dim(tiles.per.zoom.file)[1])

bp <- barplot(tiles.per.zoom,
              xlab="Número de tiles", ylab="",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.zoom)/10, 0.5),
              width=1, xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(tiles.per.zoom), lty=1, las=2)
mtext(side=2, text = "Frequência", line=3, cex=1.6)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 5e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "5e-1"), lty=1, las=1)
dev.off()


####################################################ZOOM - NO CACHE################################################################

plot.eps.config.paper("tiles-per-pan-with-cache", margin=c(3.5,3.5,1.5,0), paper="special", width=11, height=4)
par(cex=1.5)
tiles.per.zoom <- (table(tiles.per.zoom.file[,1])/dim(tiles.per.zoom.file)[1])

sum(tiles.per.zoom[26:length(tiles.per.zoom)])

bp <- barplot(tiles.per.zoom,
              xlab="Número de tiles", ylab="Frequência",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.zoom)/10, 0.5),
              width=1, xaxt="n")
par(cex=1.4)
axis(1, at=bp, labels=rownames(tiles.per.zoom), las=2)
dev.off()

####################################################ZOOM - CDF################################################################
plot.eps("tiles-per-zoom-cdf")
par(cex=2)

ecdfPlot(tiles.per.zoom.file[,1], ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Número de tiles" <= " x)"), 
         xlab="Número de tiles", main="", xlim=c(min(tiles.per.pan.file[,1]), 66))
ecdfPlot(tiles.per.zoom.file[,2], ecdf.lty=3, ecdf.lw=6, ecdf.col=2, main="", add=T)
legend("bottomright", legend = c("Com cache", "Sem cache"),
       col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()
