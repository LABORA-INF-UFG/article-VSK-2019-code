source('./pareto.R')
source('./util/util.R')
util$pkgRequire('MASS')
util$pkgRequire('VGAM')
util$pkgRequire('ghyp')
util$pkgRequire('ADGofTest')
util$pkgRequire('actuar')
util$pkgRequire("ineq")
util$pkgRequire("poweRlaw")
util$pkgRequire("evmix") # apt-get install libgsl-dev
util$pkgRequire("EnvStats")

filename = "Intervals.csv"
intervals.readed = util$loadData(filename, separator="\\")

dim(intervals.readed)
head(intervals.readed,3)
summary(intervals.readed)

operations <- subset(intervals.readed, 
                     (intervals.readed[[2]] %in% c("pan", "zoom", "search", "route")))

dim(operations)[1] / dim(intervals.readed)[1]

head(operations[,1])

think.time <- as.numeric(operations[,1])
think.time <- think.time[!is.na(think.time)]
think.time <- think.time/1000
quantile(think.time, 0.99)

mean(think.time)
median(think.time)

intervals <- think.time[think.time >= 0.4 & think.time <= 4 * 60 * 60]
length(intervals)
summary(intervals)

pareto.params <- pareto.fit(intervals, threshold=min(intervals))
pareto.params
guan$think.time.pareto.estimated.from.our.data = pareto.params

hy.fit <- fit.ghypuv(intervals, opt.pars = c(lambda = T, alpha.bar = T, mu = T,
                                             sigma = T, gamma = T))

gh <- rghyp(length(intervals), ghyp(lambda=-0.5577193, alpha.bar=0.3030547, mu=0.4058565, sigma=0.8102850, gamma=5.5453787))

length(gh[gh < 100])/length(gh)
length(intervals[intervals < 100])/length(intervals)
hist(gh[gh < 100])
hist(intervals[intervals < 100])

?rghyp

#Write data to a file
interval.data <- data.frame(intervals=intervals, general.pareto=gh)
head(interval.data)
util$writeToFile(interval.data, "interval-data.dat")

#plot.eps("intervals-gh")
plot(ecdf(intervals),ylim=c(0,1),lw=4, log="x", xlim=c(min(intervals), max(gh)), main="", ylab=expression("Prob(Intervalo " <= " x)"), xlab="Intervalo (s)")
lines(ecdf(gh),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "HG"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
#dev.off()

ks.test(gh, intervals)

summary(gh)
summary(intervals)

mean(think.time)
mean(gh[gh > min(think.time)])

summary(think.time)
summary(gh[gh > min(think.time)])

length(think.time[think.time < 1])/length(think.time)

intervals <- think.time[think.time >= 0.3]
intervals <- intervals

min(intervals)
max(intervals)

length(intervals)

ineq(intervals,type="Gini")
plot(Lc(intervals),col="darkred",lwd=2)

#plot.eucdf.loglog(intervals, type="l")

smint <- intervals

pareto.params <- pareto.fit(smint, threshold=min(smint))
pareto.params
# 
x.pareto <- rpareto(n=length(smint), pareto.params$xmin, pareto.params$exponent)
ks.test(smint, x.pareto)

plot(ecdf(intervals[intervals > 50]),ylim=c(0,1),lw=4, log="x", xlim=c(min(intervals), max(gh)), main="", ylab="Frequência", xlab="Intervalos (s)")
lines(ecdf(x.pareto[x.pareto > 50]),lw=4,verticals=T, col="red")

log.parrs <- fitdistr(smint, "weibull")

ad.test(smint, dweibull, log.parrs$estimate[1], log.parrs$estimate[2])
ks.test(smint, "dweibull", log.parrs$estimate[1], log.parrs$estimate[2])

lnorm.int <- rweibull(length(smint), log.parrs$estimate[1], log.parrs$estimate[2])

plot(ecdf(smint),ylim=c(0,1),lw=4, log="x", xlim=c(min(intervals), max(intervals)), main="", ylab="% do Total", xlab="Duração das Sessões (s)")
lines(ecdf(x.pareto),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)

# 
# plot(ecdf(intervals[intervals > pareto.params$xmin]),ylim=c(0,1),lw=4, log="x", xlim=c(min(intervals[intervals > pareto.params$xmin]), max(x.pareto)), main="", ylab="% do Total", xlab="Duração das Sessões (s)")
# lines(ecdf(x.pareto),lw=4,verticals=T, col="red")
# legend("bottomright", legend = c("Observado", "Pareto"),
#        col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
#        lwd=c(3,3),bty="n", pt.cex=2)
# 
# qqplot(intervals, x.pareto, log="xy")
# abline(0, 1, col="red")

smint <- sample(intervals, 5000)

pl_m <- conpl$new(think.time[think.time >= 0.4])

est_pl = estimate_xmin(pl_m)
est_pl
pl_m$setXmin(est_pl$xmin)
pl_m$pars = est_pl$pars

#bootstrap(pl_m, no_of_sims=2, threads=2, xmins = 10)

plot(density(intervals[intervals >= est_pl$xmin]), type="n")
lines(density(intervals[intervals >= est_pl$xmin]))
lines(density(rpareto(n=length(intervals), est_pl$xmin, est_pl$pars)), col=2)

summary(intervals[intervals >= est_pl$xmin])
summary(rpareto(n=length(intervals), est_pl$xmin, est_pl$pars))

qqplot(intervals[intervals >= est_pl$xmin], rpareto(n=length(intervals[intervals >= est_pl$xmin]), est_pl$xmin, est_pl$pars), log="xy")
abline(0,1, col=2)

x.pareto <- rpareto(n=length(smint[smint > pl_m$xmin]), pl_m$xmin, pl_m$pars)

plot(ecdf(smint[smint > pl_m$xmin]),ylim=c(0,1),lw=4, log="x", xlim=c(min(intervals), max(gh)), main="", ylab="Frequência", xlab="Intervalos (s)")
lines(ecdf(x.pareto),lw=4,verticals=T, col="red")

length(intervals[intervals >= est_pl$xmin])/length(intervals)
fitdistr(intervals[intervals < est_pl$xmin], "lognormal")

plot.eps("llcd-intervals")
plot(pl_m, xlab="Intervalo (s)", ylab=expression("Prob(Intervalo " >= " x)"))
lines(pl_m, col=2, lw=3)
dev.off()

m_bl_ln = conlnorm$new(smint[smint < pl_m$xmin])
est = estimate_xmin(m_bl_ln)
est
m_bl_ln$setXmin(est)

m_bl_ln

x.logn <- rlnorm(n=length(smint[smint < pl_m$xmin]), m_bl_ln$pars[1], m_bl_ln$pars[2])

plot(ecdf(smint[smint <= pl_m$xmin]),ylim=c(0,1),lw=4, log="x", xlim=c(min(intervals), pl_m$xmin), main="", ylab="Frequência", xlab="Intervalos (s)")
lines(ecdf(x.logn),lw=4,verticals=T, col="red")

summary(x.logn)
summary(smint[smint <= pl_m$xmin])
pl_m$xmin

pl_m <- conpl$new(think.time[think.time > 0.4 & think.time < max(think.time)])
par()
est_pl = estimate_xmin(pl_m)
est_pl
pl_m$setXmin(est_pl$xmin)
pl_m$pars = est_pl$pars

plot.eps("llcd-intervals")
plot(pl_m, xlab="Intervalo (s)", ylab=expression("Prob(Intervalo " >= " x)"), xaxt="n")
lines(pl_m, col=2, lw=3)
axis(1, at=c(0, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6), labels=c("0", "1","10", "100", "1e3", "1e4", "1e5", "1e6"))
dev.off()

10000/(60^2)

intervals <- think.time[think.time >= 0.4 & think.time <= 4 * 60 * 60]

sort(intervals, decreasing=T)[1]
sort(think.time, decreasing=T)[6] / (60 * 60)

(lngpd.fit <- flognormgpdcon(intervals))
lngpd.fit
model.par.log <- rlognormgpdcon(n=length(intervals), lnmean=lngpd.fit$lnmean, lnsd=lngpd.fit$lnsd, u=lngpd.fit$u, 
                             xi=lngpd.fit$xi)
model.par.log
quantile(intervals, 0.5)
#ks.test(intervals, "dlognormgpdcon", lnmean=lngpd.fit$lnmean, lnsd=lngpd.fit$lnsd, u=lngpd.fit$u, 
#        xi=lngpd.fit$xi, phiu=lngpd.fit$phiu)

#plot(sort(think.time) , 1-ecdf(think.time)(sort(think.time)), log="xy")

#mix.par.log <- c(rlnorm((1-0.09936001) * 25000, 0.9760651, 0.9031854), rparetoI((0.09936001) * 25000, 13.59291, 1.108232))

ks.test(intervals, model.par.log)

28760.000/(60 * 60)

summary(rparetoI(length(intervals) * 100, 1, 1.5))
summary(intervals)
summary(model.par.log)

quantile(intervals, 0.99)
quantile(model.par.log, 0.99)

ad.test(intervals, dlognormgpdcon, lnmean=lngpd.fit$lnmean, lnsd=lngpd.fit$lnsd, u=lngpd.fit$u, 
        xi=lngpd.fit$xi)

library(EnvStats)

util$plot.eps("intervals-logn-gpd")
ecdfPlot(intervals, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("P(Interval " <= " x)"), 
         xlab="Interval (s)", main="", log="x", xlim=c(min(model.par.log), max(model.par.log)))
ecdfPlot(model.par.log, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, add=T)
legend("bottomright", legend = c("Observed", "Lognormal-GPD"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 2),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

plot(m_bl_ln)
lines(m_bl_ln, col=3, lwd=2)

x.pareto <- rpareto(n=length(intervals), est_pl$xmin, est_pl$pars)
#plot.eucdf.loglog(x=x.pareto, type="l", col="red")
ks.test(intervals[intervals >= est_pl$xmin], x.pareto)
max(x.pareto) / (24*60 * 60)

qqplot(intervals[intervals > est_pl$xmin], x.pareto)
abline(0, 1, col="red")

plot(ecdf(intervals[intervals > est_pl$xmin]),ylim=c(0,1),lw=4, log="x", xlim=c(min(intervals[intervals > est_pl$xmin]), max(x.pareto)), main="", ylab="% do Total", xlab="Duração das Sessões (s)")
lines(ecdf(x.pareto),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Pareto"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)

#bs_p = bootstrap_p(pl_m)
#bs_p

#ln_m = conlnorm$new(intervals)
#(est_ln = estimate_xmin(ln_m))
#ln_m$setXmin(est_ln)
#lines(ln_m, col=2)

###########################BODY###################################
body <- intervals[intervals < est_pl$xmin]
#body <- body[body > 0.300]
length(body)
body <- sample(body, 2000)

length(body[body > 5.5]) / length(body)

#plot(ecdf(body))
plot(density(body))

summary(body)
length(body)

library(nortest)

lognormal.fit <- fitdistr(body, "lognormal")
lognormal.params <- lognormal.fit$estimate
lognormal <- rlnorm(length(body), meanlog=lognormal.params[1],
                           sdlog=lognormal.params[2])

#plot(ecdf(lognormal))
plot(density(lognormal))

ks.test(body, lognormal)

qqplot(body, lognormal, log="xy", xlab="Intervals", ylab="Lognormal")
abline(0,1, col="red")

wei.fit <- fitdistr(body, "weibull")
wei.params <- wei.fit$estimate
wei.params
wei.fitted <- rweibull(length(body), wei.params[1],
                           wei.params[2])

summary(wei.fitted)
#plot(density(wei.fitted))
#plot(ecdf(wei.fitted))

ks.test(body, wei.fitted)

qqplot(body, wei.fitted, log="xy", xlab="Intervals", ylab="Weibull")
abline(0,1, col="red")

normal.fit <- fitdistr(body, "gamma")
normal.params <- normal.fit$estimate
normal.params
normal <- rgamma(length(body), normal.params[1], normal.params[2])

summary(normal)

ks.test(body, normal)

qqplot(body, normal, log="xy", xlab="Intervals", ylab="Gamma")
abline(0,1, col="red")

p <- 0.615

dist <- p * rexp(10000, 0.030) + (1 - p) * rexp(10000, 0.148)
plot(density(dist))

plot(density(body))

qqplot(body, dist, log="xy", xlab="Intervals", ylab="Lognormal")
abline(0,1, col="red")