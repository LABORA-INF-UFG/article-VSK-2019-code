util$pkgRequire("EnvStats")

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerSession.csv"
kb.per.session.tiles = read.table(file=path, sep=",", dec=".", header=T)
head(kb.per.session.tiles)
summary(kb.per.session.tiles[,1]/1000000)
quantile(kb.per.session.tiles[,1]/1000000, 0.99)

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerSessionContentLength.csv"
kb.per.session.contentLength = read.table(file=path, sep=",", dec=".", header=T)
head(kb.per.session.contentLength)
summary(kb.per.session.contentLength[,1]/1000000)
quantile(kb.per.session.contentLength[,1]/1000000, 0.99)

util$plot.eps("kb-per-session-tiles-vs-all")
ecdfPlot(kb.per.session.contentLength[,1]/1000, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Size " <= " x)"), 
         xlab="Size (KB)", main="", log="x")
ecdfPlot(kb.per.session.tiles[,1]/1000, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Size " <= " x)"), 
         xlab="Size (KB)", main="", add=T)
legend("bottomright", legend = c("AR", "Tiles"),
        col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
        lwd=c(6,6),bty="n", pt.cex=2)
dev.off()

##########################################################################################################################

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerPan.csv"
kb.per.pan = read.table(file=path, sep=",", dec=".")
summary(kb.per.pan[,1]/1000)
quantile(kb.per.pan[,1]/1000, 0.99)

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerPanWithCache.csv"
kb.per.pan.cache = read.table(file=path, sep=",", dec=".")
summary(kb.per.pan.cache[,1]/1000)

util$plot.eps("kb-per-pan")
ecdfPlot(kb.per.pan[,1]/1000, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Size " <= " x)"), 
         xlab="Size (KB)", main="")
# ecdfPlot(kb.per.pan.cache[,1] * 8, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Tamanho " <= " x)"), 
#          xlab="Tamanho (bits)", main="", add=T)
# legend("bottomright", legend = c("Sem Cache", "Com Cache"),
#        col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
#        lwd=c(6,6),bty="n", pt.cex=2)
dev.off()

######################################################################################################################

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerZoom.csv"
kb.per.zoom = read.table(file=path, sep=",", dec=".")
head(kb.per.zoom)
summary(kb.per.zoom[,1]/1000)
quantile(kb.per.zoom[,1]/1000, 0.99)

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerZoomWithCache.csv"
kb.per.zoom.cache = read.table(file=path, sep=",", dec=".")
summary(kb.per.zoom.cache[,1])

util$plot.eps("kb-per-zoom")
ecdfPlot(kb.per.zoom[,1]/1000, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Size " <= " x)"), 
         xlab="Size (KB)", main="")
#ecdfPlot(kb.per.zoom.cache[,1] * 8, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Tamanho " <= " x)"), 
#         xlab="Tamanho (bits)", main="", add=T)
# legend("bottomright", legend = c("Sem Cache", "Com Cache"),
#        col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
#        lwd=c(6,6),bty="n", pt.cex=2)
dev.off()


############################################################################################################
path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerSessionVsNumOperations.csv"
kb.per.session.vs.num.op = read.table(file=path, sep=",", dec=".")
head(kb.per.session.vs.num.op)
summary(kb.per.session.vs.num.op[,1]/1000)
quantile(kb.per.session.vs.num.op[,1]/1000, 0.99)

util$plot.eps("kb-per-session-vs-num-operations")
ecdfPlot(kb.per.session.vs.num.op[,1]/1000, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Mean amount of KB KB " <= " x)"), 
         xlab="Mean amount of KB", main="")
dev.off()


path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/KbPerSessionVsSessionDuration.csv"
kb.vs.session.duration = read.table(file=path, sep=",", dec=".")
head(kb.vs.session.duration)
summary((kb.vs.session.duration[,1]*8)/1000)
quantile((kb.vs.session.duration[,1]*8)/1000, 0.99)

util$plot.eps("kb-per-session-vs-session-duration")
ecdfPlot((kb.vs.session.duration[,1]*8)/1000, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Kbps " <= " x)"), 
         xlab="Kbps", main="")
dev.off()


###########################################################################################################################
util$plot.eps.config.paper("tiles-per-pan", margin=c(3.5,4,.5,0), paper="special", width=11, height=5.3)
par(cex=1.6)
tiles.per.pan <- (table(kb.per.pan[,2])/length(kb.per.pan[,2]))

bp <- barplot(tiles.per.pan,
              xlab="Number of tiles", ylab="",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.pan)/10, 0.5),
              width=1, xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(tiles.per.pan), lty=1, las=2)
mtext(side=2, text = "Frequency", line=3, cex=1.6)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 5e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "5e-1"), lty=1, las=1)
dev.off()

plot.eps("tiles-per-pan-with-cache", margin=c(3.5,3.5,1.5,0), paper="special", width=11, height=4)
par(cex=1.5)
tiles.per.pan <- (table(kb.per.pan.cache[,2])/length(kb.per.pan.cache[,2]))

sum(tiles.per.pan[26:length(tiles.per.pan)])

bp <- barplot(tiles.per.pan,
              xlab="Número de tiles", ylab="Frequência",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.pan)/10, 0.5),
              width=1, xaxt="n")
par(cex=1.4)
axis(1, at=bp, labels=rownames(tiles.per.pan), las=2)
dev.off()

########################ZOOM

util$plot.eps.config.paper("tiles-per-zoom", margin=c(3.5,4,0.5,0), paper="special", width=6, height=3)
par(cex=1.2)
tiles.per.zoom <- (table(kb.per.zoom[,2])/length(kb.per.zoom[,2]))[1:21];tiles.per.zoom

bp <- barplot(tiles.per.zoom,
              xlab="Number of tiles", ylab="",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.zoom)/10, 0.5),
              width=1, yaxt="n", xaxt="n")
axis(1, at=bp, labels=rownames(tiles.per.zoom), lty=1, las=2)
mtext(side=2, text = "Frequency", line=3, cex=1.2)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 5e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "5e-1"), lty=1, las=1)
dev.off()

plot.eps("tiles-per-zoom-with-cache", margin=c(3.5,3.5,1.5,0), paper="special", width=11, height=4)
tiles.per.zoom <- (table(kb.per.zoom.cache[,2])/length(kb.per.zoom.cache[,2]))

sum(tiles.per.pan[26:length(tiles.per.zoom)])

bp <- barplot(tiles.per.zoom,
              xlab="Número de tiles", ylab="Frequência",
              beside=T, col=c(2), log="y", ylim=c(min(tiles.per.zoom)/10, 0.5), xpd=F, axis.lty=1,
              xaxt="n")
par(cex=1.4)
axis(1, at=bp, labels=rownames(tiles.per.zoom), las=2)
dev.off()

