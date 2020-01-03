source("./util/util.R")

util$pkgRequire("e1071")
util$pkgRequire("fitdistrplus")
util$pkgRequire("MASS")
util$pkgRequire("VGAM")
util$pkgRequire("ADGofTest")
util$pkgRequire("EnvStats")

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/SessionDurations.csv"
durations = scan(file=path)

# SUMMARY OF SESSION DURATIONS
summary(durations)

(length(durations[durations > 24 * 60 * 60])/length(durations)) * 100
(q99 <- quantile(durations, 0.99))/60
q99 / (60 * 60)
q99
durations[durations > q99]
q01 <- quantile(durations, 0.01)
durations[durations <= q01]

quantile(durations, 0.01)
min(durations)

y = durations[durations <= 60*60 * 24]

# SUMMARY OF SESSION DURATIONS
options(digits=10)
summary(durations)

# SUMMARY OF SESSION DURATIONS
summary(y)

#plot.eps("session-hist")
hist(y[y < 1000], breaks="FD", main="", xlab="Duração das Sessões (s)", ylab="Frequência", col="red", freq=FALSE)
#dev.off()

#plot.eps("session-dens")
plot(density(y), main="", xlab="Duração das Sessões (s)", ylab="Densidade", type="n")
lines(density(y), col="red", lw=3)
#dev.off()

(parametersLogNormal = fitdistr(y, "lognormal"))
#parametersWei = fitdistr(y, "weibull")
#parametersExponencial = fitdistr(y, "exponential")


#print(parametersWei)
#print(parametersExponencial)

meanlog = parametersLogNormal$estimate[1]
sdlog = parametersLogNormal$estimate[2]

#rate = parametersExponencial$estimate[1]
#wei.shape <- parametersWei$estimate[1]
#wei.scale <- parametersWei$estimate[2]
meanlog
sdlog

lognormal = rlnorm(length(y), meanlog, sdlog)
#wei <- rweibull(size, shape=wei.shape, scale=wei.scale)
ks.test(y, lognormal)

util$plot.eps("sessions-lognorm-cdf")
ecdfPlot(y, log="x", ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("Prob(Session duration " <= " x)"), 
         xlab="Duration (s)", main="", xlim=c(min(lognormal), max(y)))
ecdfPlot(lognormal, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("Prob(Session duration " <= " x)"), 
         xlab="Duration (s)", main="", add=T)
legend("bottomright", legend = c("Observed", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
       lwd=c(4,4),bty="n", pt.cex=2)
dev.off()

plot(density(lognormal))
plot(density(y))

plot.eps("sessions-lognorm-pdf")
plot(density(y), main="", xlab="Duração da sessão (s)", ylab=expression("Prob(Duração da sessão " <= " x)"), type="n")
lines(density(y), col="black", lw=4)
lines(density(lognormal), col="red", lw=4)
legend("topright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

plot.eps("sessions-lognorm-cdf2", margin=c(3.6, 3.6, 0.5, 1.1))
plot(ecdf(y),ylim=c(0,1),lw=4, log="x", xlim=c(min(y), max(lognormal)), main="", ylab=expression("Prob(Duração da sessão " <= " x)"), xlab="Duração da Sessão (s)")
lines(ecdf(lognormal),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

#qqnorm(y);  qqline(y, col = 2)
plot.eps("sessions-lognorm-qq")
qqplot(y, lognormal, xlab="Duração das Sessões (s)", ylab="Lognormal")
abline(0,1, col="red")
dev.off()

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/SessionDurations1Min.csv"
durations1Min = read.table(path, sep=",", dec=".", header=FALSE)
head(durations1Min)

durations.1min <- durations1Min[[1]]/1000
num.op.1min <- durations1Min[[2]]

y = durations.1min
total.durations <- durations
min(total.durations)
min(y)

util$pkgRequire("EnvStats")
util$pkgRequire("rgdal")
util$pkgRequire("rgeos")
util$pkgRequire("spsurvey")

setEPS()
postscript(paste("/media/arquivos/Dropbox/mestrado/4-semestre/dissertacao/svnlabora/jidm15/vinicius/figs/", "all-sessions-vs-1min", ".eps", sep=""),
           width=10.51181103, height=7.82283464)
par(mar=c(3.6, 3.5, 0.5, 0.8))
par(mgp = c(2.5, 1, 0))
par(cex=3.1)
ecdfPlot(total.durations, log="x", ecdf.lty=2, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Session duration" <= " x)"), 
         xlab="Seconds", main="")
lines(ecdf(y), lty=1, lw=6, col=2)
legend("bottomright", legend = c("Engaged Users", "All Sessions"),
                    col = c("red", "black"), pch=c(NA,NA), lty = c(1, 2),
                    lwd=c(4,4),bty="n", pt.cex=2)
dev.off()

length(subset(durations.1min, durations.1min < 10))/length(subset(durations.1min, num.op.1min < 3))

plot(density(subset(durations.1min, num.op.1min < 3)))
length(subset(durations.1min, durations.1min < 5))

parametersLogNormal = fitdistr(y, "lognormal")
#parametersWei = fitdistr(y, "weibull")
#parametersExponencial = fitdistr(y, "exponential")

print(parametersLogNormal)

#print(parametersWei)
#print(parametersExponencial)

meanlog = parametersLogNormal$estimate[1]
sdlog = parametersLogNormal$estimate[2]

#rate = parametersExponencial$estimate[1]
#wei.shape <- parametersWei$estimate[1]
#wei.scale <- parametersWei$estimate[2]

lognormal = rlnorm(5000, meanlog, sdlog)
#wei <- rweibull(size, shape=wei.shape, scale=wei.scale)
plot(density(lognormal))
plot(density(y))

summary(lognormal)

ks.test(y, lognormal)

plot.eps("sessions-lognorm-pdf")
plot(density(y), main="", xlab="Duração da sessão (s)", ylab=expression("Prob(Duração da sessão " <= " x)"), type="n")
lines(density(y), col="black", lw=4)
lines(density(lognormal), col="red", lw=4)
legend("topright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

plot.eps("sessions-lognorm-cdf", margin=c(3.6, 3.6, 0.5, 1.1))
plot(ecdf(y),ylim=c(0,1),lw=4, log="x", xlim=c(min(y), max(lognormal)), main="", ylab=expression("Prob(Duração da sessão " <= " x)"), xlab="Duração da Sessão (s)")
lines(ecdf(lognormal),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()
