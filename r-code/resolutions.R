source("./util/util.R")

util$pkgRequire("e1071")
util$pkgRequire("fitdistrplus")
util$pkgRequire("MASS")
util$pkgRequire("VGAM")

path = "/media/arquivos/pcloud/mestrado/4-semestre/dissertacao/svnlabora/teses/msc/vinicius/texto/defesa/figs/resolutions/resolution-BR-monthly-201503-201505-bar.csv"
resolutions = read.table(path, sep=",", dec=".", header=TRUE)

plot.eps("resolutions-br", margin=c(5, 3.5, 0.5, 0.1))
resolutions.freq <- resolutions[[2]]/100
bp <- barplot(resolutions.freq, names.arg=resolutions[[1]], xlab="", ylab="Frequência",
              col=c("red"), las=2, cex.names=0.9, log="y", ylim=c(min(resolutions.freq), 0.5))
dev.off()

path = "/media/arquivos/pcloud/mestrado/4-semestre/dissertacao/svnlabora/teses/msc/vinicius/texto/defesa/figs/resolutions/resolution-ww-monthly-201503-201505-bar.csv"
resolutions = read.table(path, sep=",", dec=".", header=TRUE)

#plot.eps("resolutions-ww", margin=c(3.5, 5.5, 0, 0.1))
resolutions.freq <- resolutions[[2]]/100
bp <- barplot(rev(resolutions.freq), names.arg=rev(resolutions[[1]]), ylab="", xlab="Frequência",
              col=c("red"), las=2, cex.names=0.9, log="x", xlim=c(min(resolutions.freq), 0.5),
              horiz=T, axes=F)
axis(1, at=c(0.0042, 0.01, 0.05, 0.1, 0.3), labels=c("4e-03", "1e-02", "5e-02", "1e-01", "3e-01"))
#dev.off()

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/Resolutions.csv"
resolutions.collected = read.table(path, sep=",", dec=".", header=TRUE)

strsplit(as.character(resolutions.collected[2,1]), "x")[[1]][1]
resolutions.table <- table(resolutions.collected)
plot(resolutions.table)
resolutions.array <- c()

for (i in 1:nrow(resolutions.collected)) {
  resolutions.array <- c(resolutions.array, as.character(resolutions.collected[i,1]))
}
resolutions.array <- resolutions.array[!is.na(resolutions.array)]
top10 <- sort(table(resolutions.array), decreasing=T)[1:10]

plot(top10/sum(top10), xaxt="n")
axis(1, at=1:20, labels=rownames(sort(table(resolutions.array), decreasing=T)[1:20]))
top10.freq <- top10/sum(top10)

rev(top10.freq)
top10.freq

util$plot.eps("resolutions-collected", margin=c(3.5, 5.1, 0, 0.25))
bp <- barplot(rev(top10.freq), names.arg=rev(rownames(top10.freq)), ylab="", xlab="Frequency",
              col=c("red"), las=2, cex.names=0.9, xlim=c(0, max(top10.freq) + 0.05),
              horiz=T, xaxt="n", yaxt="n")
axis(2, at=bp, labels=rev(rownames(top10.freq)), las=2)
axis(1, at=c(0, 5e-2, 1e-1, 0.15, 0.2, 0.25, 0.3), labels=c("0", "0.05", "0.10", "0.15", "0.20", 
                                                                                        "0.25", "0.30"))
dev.off()

resolutions

