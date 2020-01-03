util$pkgRequire("EnvStats")
mytest = new.env()  
mytest$data = read.csv('/home/vinicius/peval/simulation-results/musegen-original/intervals.csv', sep=",", header = T)
head(mytest$data[,2])

mytest$intervals = mytest$data[,2]/1000
#mytest$intervals = mytest$intervals[mytest$intervals < 4*60*60]
mytest$intervals = sample(mytest$intervals, 2000)
summary(mytest$intervals)

ecdfPlot(intervals, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Interval " <= " x)"), 
         xlab="Interval (s)", main="", log="x", xlim=c(min(model.par.log), max(model.par.log)))
ecdfPlot(model.par.log, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, add=T)
ecdfPlot(mytest$intervals, ecdf.lty=3, ecdf.lw=6, ecdf.col=3, add=T)

summary(mytest$intervals)
summary(intervals)
mytest$intervals[mytest$intervals < 0.5]
intervals[intervals < 0.5]

#######################################DURATION###################################################

mytest$musegen.durations = read.csv('/home/vinicius/peval/simulation-results/musegen/durations.csv', sep=",", header = T)
head(mytest$musegen.durations)
mytest$musegen.durations = mytest$musegen.durations[,2]/1000
mytest$musegen.durations = sample(mytest$musegen.durations, 10000)

mytest$help.durations = read.csv('/home/vinicius/peval/simulation-results/help/durations.csv', sep=",", header = T)
head(mytest$help.durations[,2])
mytest$help.durations = mytest$help.durations[,2]/1000
mytest$help.durations = sample(mytest$help.durations, 10000)

mytest$custom.help.durations = read.csv('/home/vinicius/peval/simulation-results/custom-help/durations.csv', sep=",", header = T)
head(mytest$custom.help.durations[,2])
mytest$custom.help.durations = mytest$custom.help.durations[,2]/1000
mytest$custom.help.durations = sample(mytest$custom.help.durations, 10000)

mytest$intervals = mytest$intervals[mytest$intervals < 4*60*60]
mytest$intervals = sample(mytest$intervals, 2000)
summary(mytest$intervals)

summary(mytest$musegen.durations)
quantile(mytest$musegen.durations, 0.05)
quantile(durations[durations <= 60*60 * 24], 0.05)
summary(durations[durations <= 60*60 * 24])

util$plot.eps("duration-comparisson-musegen-help-custom-help")
ecdfPlot(mytest$musegen.durations, ecdf.lty=1, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Session Duration " <= " x)"), 
         xlab="Session Duration (s)", main="", log="x")
ecdfPlot(mytest$help.durations, ecdf.lty=2, ecdf.lw=6, ecdf.col=4, add=T)
ecdfPlot(mytest$custom.help.durations, ecdf.lty=4, ecdf.lw=6, ecdf.col=3, add=T)
legend("bottomright", legend=c("MUSeGen", "HELP", "Custom HELP"), col=c(2,4,3), lty=c(1,2,4), lwd=c(2, 2, 2), pch=c(NA,NA,NA), bty="n")
dev.off()

#######################################NUMBER OF OPERATIONS###################################################

mytest$musegen.num.operations = read.csv('/home/vinicius/peval/simulation-results/musegen/num_operations.csv', sep=",", header = T)
head(mytest$musegen.num.operations)
mytest$musegen.num.operations = mytest$musegen.num.operations[,2]
mytest$musegen.num.operations = sample(mytest$musegen.num.operations, 10000)

mytest$help.num.operations = read.csv('/home/vinicius/peval/simulation-results/help/num_operations.csv', sep=",", header = T)
head(mytest$help.num.operations[,2])
mytest$help.num.operations = mytest$help.num.operations[,2]
mytest$help.num.operations = sample(mytest$help.num.operations, 10000)

mytest$custom.help.num.operations = read.csv('/home/vinicius/peval/simulation-results/custom-help/num_operations.csv', sep=",", header = T)
head(mytest$custom.help.num.operations[,2])
mytest$custom.help.num.operations = mytest$custom.help.num.operations[,2]
mytest$custom.help.num.operations = sample(mytest$custom.help.num.operations, 10000)

#mytest$intervals = mytest$intervals[mytest$intervals < 4*60*60]
mytest$intervals = sample(mytest$intervals, 2000)
summary(mytest$intervals)

util$plot.eps("number-of-operations-comparisson-musegen-help-custom-help")
ecdfPlot(mytest$musegen.num.operations, ecdf.lty=1, ecdf.lw=6, ecdf.col=2, ylab=expression("P(Number of operations " <= " x)"), 
         xlab="Number of operations", main="")
ecdfPlot(mytest$help.num.operations, ecdf.lty=2, ecdf.lw=6, ecdf.col=4, add=T)
ecdfPlot(mytest$custom.help.num.operations, ecdf.lty=4, ecdf.lw=6, ecdf.col=3, add=T)
legend("bottomright", legend=c("MUSeGen", "HELP", "Custom HELP"), col=c(2,4,3), lty=c(1,2,4), lwd=c(2, 2, 2), pch=c(NA,NA,NA), bty="n")
dev.off()


###################################################ZOOM FREQUENCY#######################################
mytest$help.zoom.freq = read.csv('/home/vinicius/peval/simulation-results/help2/zoom_freq.csv', sep=",", header = T)
head(mytest$help.zoom.freq)

util$plot.eps("zoom-frequency-help", margin=c(3, 3, 1, 0))
bp <- barplot(mytest$help.zoom.freq[2:22,3], ylim=c(0, 0.5), 
              names.arg="", xlab="Zoom level", ylab="Frenquency",
              col=c("red"), mgp=c(2,0.5,.5))
text(cex=0.8, x=bp, y=-0.015, labels=mytest$help.zoom.freq[2:22,2], srt=-90, xpd=TRUE)
dev.off()

mytest$custom.help.zoom.freq = read.csv('/home/vinicius/peval/simulation-results/custom-help2/zoom_freq.csv', sep=",", header = T)
head(mytest$custom.help.zoom.freq)

util$plot.eps("zoom-frequency-custom-help", margin=c(3, 3, 1, 0))
bp <- barplot(mytest$custom.help.zoom.freq[2:22,3], ylim=c(0, 0.25), 
              names.arg="", xlab="Zoom level", ylab="Frenquency",
              col=c("red"), mgp=c(2,0.5,.5))
text(cex=0.8, x=bp, y=-0.015, labels=mytest$custom.help.zoom.freq[2:22,2], srt=-90, xpd=TRUE)
dev.off()
