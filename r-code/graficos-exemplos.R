source("/media/arquivos/Dropbox/mestrado/4-semestre/r-scripts/util.R")

library(e1071)
library(fitdistrplus)
library(MASS)
library(VGAM)
library(EnvStats)

size <- 5000

y <- rlnorm(size, 4, 0.5)

plot.eps("lognorm-hist")
hist(y[y < 1000], breaks="FD", main="", xlab="Duração das Sessões (s)", ylab="Frequência", col="red", freq=FALSE)
dev.off()

plot.eps("example-dens")
plot(density(y), main="", xlab="Valores", ylab="Probabilidade", type="n")
lines(density(y), col="black", lw=3)
dev.off()

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

lognormal = rlnorm(size, meanlog, sdlog)
#wei <- rweibull(size, shape=wei.shape, scale=wei.scale)
#plot(density(lognormal))
#plot(density(y))

plot.eps("example-compare-pdf")
plot(density(y), main="", xlab="Valores", ylab="Probabilidade", type="n")
lines(density(y), col="black", lw=4)
lines(density(lognormal), col="red", lw=4, lty=2)
legend("topright", legend = c("Artificiais", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 2),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

#exponential = rexp(size, rate=rate)

#plot.ecdf(y); par(new=TRUE); plot.ecdf(exponential, col="red")
#plot.ecdf(y); par(new=TRUE); plot.ecdf(wei, col="red")

plot.eps("example-compare-cdf")
ecdfPlot(y, ecdf.lty=1, ecdf.lw=6, log="x", ecdf.col=1, ylab=expression("Prob(Valores " <= " x)"), 
         xlab="Valores", main="")
ecdfPlot(lognormal, ecdf.lty=2, ecdf.lw=6, log="x", ecdf.col=2, ylab=expression("Prob(Valores " <= " x)"), 
         xlab="Valores", main="", add=T)
legend("bottomright", legend = c("Artificiais", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 2),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

#qqnorm(y);  qqline(y, col = 2)
plot.eps("example-qq")
qqplot(y, lognormal, xlab="Valores", ylab="Lognormal")
abline(0,1, col="red")
dev.off()

library(ADGofTest)

ks.test(y, "plnorm", meanlog, sdlog)
lognormal = rlnorm(size, meanlog, sdlog)
ks.test(y, lognormal)
ad.test(y, plnorm, meanlog, sdlog)

#########################PARETO#########################################

source('/media/arquivos/Dropbox/mestrado/4-semestre/r-scripts/pareto.R')
library(MASS)
library(VGAM)

intervals <- rlnorm(5000, 4, 2)
#intervals <- rpareto(5000, 1, 1.5)

pareto.params <- pareto.fit(intervals, threshold=min(intervals))
pareto.params

#plot.eucdf.loglog(x=intervals, type="l")
x.pareto <- rpareto(n=1000, 1, 1.5)
#plot.eucdf.loglog(x=x.pareto, type="l", col="red")
ks.test(intervals, x.pareto)

qqplot(intervals, x.pareto, log="xy")
abline(0, 1, col="red")

library(poweRlaw)

pl_m <- conpl$new(intervals)

est_pl = estimate_xmin(pl_m)
est_pl
pl_m$setXmin(est_pl)

plot.eps("example-llcd-lognormal")
plot(pl_m, ylab=expression("Prob(Valores " >= " x)"), xlab="Valores")
lines(pl_m, col=2, lw=5)
dev.off()

pl_m <- conpl$new(x.pareto)

est_pl = estimate_xmin(pl_m)
est_pl
pl_m$setXmin(est_pl)

plot.eps("example-llcd-pareto", margin=c(3.6, 3.5, 0.5, 1.2))
plot(pl_m, ylab=expression("Prob(Valores " >= " x)"), xlab="Valores")
lines(pl_m, col=2, lw=4)
dev.off()

