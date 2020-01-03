library(EnvStats)

tiles.per.action.model.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/model/cache/pan-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")
length(tiles.per.action.model[tiles.per.action.model > 20])/length(tiles.per.action.model)

tiles.per.action.model <- tiles.per.action.model[tiles.per.action.model <= 20]

plot.eps("tiles-per-pan-model-simulated", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(0, 1),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=2, cex.names=0.9)

dev.off()

tiles.per.action.model.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/model/pan-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")
length(tiles.per.action.model[tiles.per.action.model > 20])/length(tiles.per.action.model)

tiles.per.action.model <- tiles.per.action.model[tiles.per.action.model <= 21]

util$plot.eps("tiles-per-pan-model-simulated-nocache", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion,
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=0, cex.names=0.9,
              ylim=c(min(proportion)/10, 1), xaxt="n", yaxt="n", log="y")
axis(1, at=bp, labels=rownames(proportion), col.axis="black", las=2)
axis(2, at=c(1e-4, 1e-3, 1e-2, 1e-1, 1e0), labels=c("1e-04", "1e-03", "1e-02", "1e-01", "1.0"), col.axis="black", las=0)
dev.off()


##################################################ZOOM - MODEL###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/cache/zoom-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n");tiles.per.action.model

tiles.per.action.model <- tiles.per.action.model#[tiles.per.action.model <= 40]

util$plot.eps("tiles-per-zoom-model-simulated", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model);proportion
bp <- barplot(proportion, ylim=c(0, 0.25), names.arg="",
              xlab="Number of tiles", ylab="Frequency", col=c("red"), cex.names=0.9)

text(cex=0.9, x=bp[c(1, 11, 21, 31, 41, 48)], y=-0.015, labels=levels(as.factor(tiles.per.action.model))[c(1, 11, 21, 31, 41, 48)], srt=90, xpd=TRUE)
dev.off()

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/zoom-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

resolutions

tiles.per.action.model <- tiles.per.action.model

util$plot.eps("tiles-per-zoom-model-simulated-nocache", margin=c(3.6, 3.5, 1.3, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion/10), 1), names.arg=levels(tiles.per.action.model),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=0, cex.names=0.9,
              log="y", xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(proportion), col.axis="black", las=2)
axis(2, at=c(1e-4, 1e-3, 1e-2, 1e-1, 1e0), labels=c("1e-04", "1e-03", "1e-02", "1e-01", "1.0"), col.axis="black", las=0)
dev.off()

##################################################SESSION - MODEL###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/cache/session-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-session-model-simulated")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
dev.off()

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/session-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model
mean(tiles.per.action.model)

#plot.eps("tiles-per-session-model-simulated-nocache")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
#dev.off()

##################################################PAN - GUAN###############################################################

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/cache/pan-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model[tiles.per.action.model <= 32]

table(tiles.per.action.model)/length(tiles.per.action.model)

plot.eps("tiles-per-pan-guan-simulated", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion)/10, 1), names.arg=levels(tiles.per.action.model),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=2, cex.names=0.9,
              log="y")
dev.off()

tiles.per.action.model.path = paste(PCLOUD_DIR, "/mestrado/4-semestre/scala-scripts/tiles/guan/pan-guan.log", sep="")
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")
head(tiles.per.action.model)

tiles.per.action.model <- tiles.per.action.model[tiles.per.action.model <= 32]

util$plot.eps("tiles-per-pan-guan-simulated-nocache", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion)/10, 1),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=0, cex.names=0.9,
              log="y", xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(proportion), col.axis="black", las=2)
axis(2, at=c(1e-4, 1e-3, 1e-2, 1e-1, 1e0), labels=c("1e-04", "1e-03", "1e-02", "1e-01", "1.0"), col.axis="black", las=0)
dev.off()


##################################################ZOOM - GUAN###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/cache/zoom-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-zoom-guan-simulated", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion)/10, 1),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=2, cex.names=0.9,
              log="y")
dev.off()

tiles.per.action.model.path = paste(PCLOUD_DIR, "/mestrado/4-semestre/scala-scripts/tiles/guan/zoom-guan.log", sep="")
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-zoom-guan-simulated-nocache", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion/10), 1),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=0, cex.names=0.9,
              log="y", xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(proportion), col.axis="black", las=2)
axis(2, at=c(1e-3, 1e-2, 1e-1, 1e0), labels=c("1e-03", "1e-02", "1e-01", "1.0"), col.axis="black", las=0)
dev.off()

##################################################SESSION - GUAN###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/cache/session-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-session-guan-simulated")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
dev.off()

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/session-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

#plot.eps("tiles-per-session-guan-simulated-nocache")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
#dev.off()

################################################################################################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/tiles-per-session-model.log"
tiles.per.session.model = scan(tiles.per.action.model.path, sep="\n")
tiles.per.action.guan.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/tiles-per-session-guan.log"
tiles.per.session.guan = scan(tiles.per.action.guan.path, sep="\n")

summary(tiles.per.session.model)
summary(tiles.per.session.guan)

#plot.eps("tiles-per-session-model-help-10-minutes")
ecdfPlot(tiles.per.session.model, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
ecdfPlot(tiles.per.session.guan, ecdf.lty=3, ecdf.lw=6, ecdf.col=4, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="", add=T)
legend("bottomright", legend = c("MP", "HELP"),
       col = c("red", "blue"), pch=c(NA,NA), lty = c(2, 3),
       lwd=c(3,3),bty="n", pt.cex=2)
#dev.off()








######################################################GRÁFICOS MAIORES############################################################
library(EnvStats)

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/cache/pan-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")
length(tiles.per.action.model[tiles.per.action.model > 20])/length(tiles.per.action.model)

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-pan-model-simulated", margin=c(3.6, 4, 0, 0), paper="special", width=13, height=6)
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion)/10, max(proportion) * 2), log="y",
              xlab="Number of tiles", ylab="", col=c("red"), cex.names=0.9,
              yaxt="n", xaxt="n")
axis(1, at=bp, labels=rownames(proportion), lty=1, las=2)
mtext(side=2, text = "Frequency", line = 3, cex=2)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 4e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "4e-1"), lty=1, las=1)
dev.off()

tiles.per.action.model.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/model/pan-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")
length(tiles.per.action.model[tiles.per.action.model > 20])/length(tiles.per.action.model)

tiles.per.action.model <- tiles.per.action.model[tiles.per.action.model <= 21]

util$plot.eps.config.paper("tiles-per-pan-model-simulated-nocache", margin=c(3.6, 4, 1.1, 0), paper="special", width=8, height=6)
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion,
              xlab="Number of tiles", ylab="", col=c("red"), las=0, cex.names=0.9,
              log="y", ylim=c(min(proportion)/10, 1), xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(proportion), lty=1, las=2)
mtext(side=2, text = "Frequency", line = 3, cex=2)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "1.0"), lty=1, las=0)
dev.off()


##################################################ZOOM - MODEL###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/cache/zoom-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model#[tiles.per.action.model <= 40]

plot.eps("tiles-per-zoom-model-simulated", margin=c(3.6, 3, 0, 0), paper="special", width=15, height=6)
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion)/10, max(proportion) * 3), log="y",
              xlab="Number of tiles", ylab="", col=c("red"), las=0, cex.names=0.9,
              xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(proportion), lty=1, las=2)
mtext(side=2, text = "Frequency", line=2, cex=2)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 4e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "4e-1"), lty=1, las=1, line=-1)
dev.off()

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/zoom-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

resolutions

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-zoom-model-simulated-nocache", margin=c(3.6, 3.5, 1.3, 0.1), paper="special", width=15, height=6)
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(min(proportion/10), 1), names.arg=levels(tiles.per.action.model),
              xlab="Number of tiles", ylab="", col=c("red"), las=0, cex.names=0.9,
              log="y", xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(proportion), lty=1, las=2)
mtext(side=2, text = "Frequency", line=2, cex=2)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 5e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "5e-1"), lty=1, las=1, line=-1)
dev.off()

##################################################SESSION - MODEL###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/cache/session-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-session-model-simulated")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
dev.off()

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/session-model.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model
mean(tiles.per.action.model)

plot.eps("tiles-per-session-model-simulated-nocache")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
dev.off()

##################################################PAN - GUAN###############################################################

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/cache/pan-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

table(tiles.per.action.model)/length(tiles.per.action.model)

plot.eps("tiles-per-pan-guan-simulated", margin=c(3.6, 3.5, 1, 0.1), paper="special", width=15, height=6)
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(0, 1), names.arg=levels(tiles.per.action.model),
              xlab="Number of tiles", ylab="", col=c("red"), las=2, cex.names=0.9,
              xaxt="n", yaxt="n")
axis(1, at=bp, labels=rownames(proportion), lty=1, las=2)
mtext(side=2, text = "Frequency", line=2, cex=2)
axis(2, at=c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 5e-1), labels=c("1e-6", "1e-5", "1e-4","1e-3", "1e-2", "1e-1", "5e-1"), lty=1, las=1, line=-1)
dev.off()

tiles.per.action.model.path = "/media/arquivos/pcloud/mestrado/4-semestre/scala-scripts/tiles/guan/pan-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model[tiles.per.action.model <= 32]

util$plot.eps("tiles-per-pan-guan-simulated-nocache", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(0, 1),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=2, cex.names=0.9)
dev.off()


##################################################ZOOM - GUAN###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/cache/zoom-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-zoom-guan-simulated", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(0, 1),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=2, cex.names=0.9)
dev.off()

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/zoom-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-zoom-guan-simulated-nocache", margin=c(3.6, 3.5, 1, 0.1))
proportion <- table(tiles.per.action.model)/length(tiles.per.action.model)
bp <- barplot(proportion, ylim=c(0, 1),
              xlab="Number of tiles", ylab="Frequency", col=c("red"), las=2, cex.names=0.9)
dev.off()

##################################################SESSION - GUAN###############################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/cache/session-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-session-guan-simulated")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
dev.off()

tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/session-guan.log"
tiles.per.action.model = scan(tiles.per.action.model.path, sep="\n")

tiles.per.action.model <- tiles.per.action.model

plot.eps("tiles-per-session-guan-simulated-nocache")
ecdfPlot(tiles.per.action.model, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
dev.off()

################################################################################################################################
tiles.per.action.model.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/model/session-model.log"
tiles.per.session.model = scan(tiles.per.action.model.path, sep="\n")
tiles.per.action.guan.path = "/home/vinicius/pCloudDrive/sync/mestrado/4-semestre/scala-scripts/tiles/guan/session-guan.log"
tiles.per.session.guan = scan(tiles.per.action.guan.path, sep="\n")

summary(tiles.per.session.model)
summary(tiles.per.session.guan)

plot.eps("tiles-per-session-model-help-10-minutes")
ecdfPlot(tiles.per.session.model, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="")
ecdfPlot(tiles.per.session.guan, ecdf.lty=3, ecdf.lw=6, ecdf.col=4, ylab=expression("P(Number of tiles" <= " x)"), 
         xlab="Number of tiles", main="", add=T)
par(cex=1.6)
legend("bottomright", legend = c("MUSe-GM", "HELP"),
       col = c("red", "blue"), pch=c(NA,NA), lty = c(2, 3),
       lwd=c(4,4),bty="n", pt.cex=2)
dev.off()

