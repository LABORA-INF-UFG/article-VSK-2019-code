source("./util/util.R")

util$pkgRequire("e1071")
util$pkgRequire("fitdistrplus")
util$pkgRequire("MASS")
util$pkgRequire("VGAM")

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/SessionInterarrival.csv"
interarrival = scan(file=path)

summary(interarrival)
(length(interarrival[interarrival > 24 * 60 * 60])/length(interarrival)) * 100
(q99 <- quantile(interarrival, 0.99))
q99 / (60 * 60 * 24)
interarrival[interarrival > q99]
q01 <- quantile(interarrival, 0.01)
interarrival[interarrival <= q01]

# path = "/media/arquivos/Dropbox/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/SessionDurations1Min.csv"
# durations1Min = scan(path)
# size=length(durations1Min)
# print(size)
y = interarrival[interarrival > 0]
(size <- length(y))

plot.eps("session-interarrival-hist")
hist(y, breaks="FD", main="", xlab="Duração das Sessões (s)", ylab="Frequência", col="red", freq=FALSE)
dev.off()

plot.eps("session-interarrival-dens")
plot(density(y), main="", xlab="Duração das Sessões (s)", ylab="Densidade", type="n")
lines(density(y), col="red", lw=3)
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

lognormal = rlnorm(1000, meanlog, sdlog)
#wei <- rweibull(size, shape=wei.shape, scale=wei.scale)
plot(density(lognormal))
plot(density(y))

plot.eps("sessions-interarrival-lognorm-pdf")
plot(density(lognormal), main="", xlab="Duração das Sessões (s)", ylab="Densidade", type="n")
lines(density(y), col="black", lw=4)
lines(density(lognormal), col="red", lw=4)
legend("topright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

plot.eps("sessions-interarrival-lognorm-cdf")
plot(ecdf(y),ylim=c(0,1),lw=4, log="x", xlim=c(min(y), max(lognormal)), main="", ylab="% do Total", xlab="Duração das Sessões (s)")
lines(ecdf(lognormal),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

#qqnorm(y);  qqline(y, col = 2)
plot.eps("sessions-interarrival-lognorm-qq")
qqplot(y, lognormal, xlab="Duração das Sessões (s)", ylab="Lognormal")
abline(0,1, col="red")
dev.off()

util$pkgRequire("ADGofTest")

ks.test(y, "plnorm", meanlog, sdlog)
ad.test(y, plnorm, meanlog, sdlog)

weibull.session.pars = fitdistr(y, "weibull")

weibull.session = rweibull(1000, weibull.session.pars$estimate[1], weibull.session.pars$estimate[2])

ks.test(y, "pweibull", weibull.session.pars$estimate[1], weibull.session.pars$estimate[2])
ad.test(y, pweibull, weibull.session.pars$estimate[1], weibull.session.pars$estimate[2])

plot(ecdf(y),ylim=c(0,1),lw=4, log="x", xlim=c(min(y), max(lognormal)), main="", ylab="% do Total", xlab="Duração das Sessões (s)")
lines(ecdf(weibull.session),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Weibull"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)

(cauchy.session.pars = fitdistr(y, "cauchy"))

cauchy.session = rcauchy(1000, cauchy.session.pars$estimate[1], cauchy.session.pars$estimate[2])

ks.test(y, "pcauchy", cauchy.session.pars$estimate[1], cauchy.session.pars$estimate[2])
ad.test(y, pcauchy, cauchy.session.pars$estimate[1], cauchy.session.pars$estimate[2])

plot(ecdf(y),ylim=c(0,1),lw=4, log="x", xlim=c(min(y), max(lognormal)), main="", ylab="% do Total", xlab="Duração das Sessões (s)")
lines(ecdf(cauchy.session),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Cauchy"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)

(gamma.session.pars = fitdistr(y, "gamma", list(shape = 1, rate = 0.1), lower = 0.01))

gamma.session = rgamma(1000, gamma.session.pars$estimate[1], gamma.session.pars$estimate[2])

ks.test(y, "pgamma", gamma.session.pars$estimate[1], gamma.session.pars$estimate[2])
ad.test(y, pgamma, gamma.session.pars$estimate[1], gamma.session.pars$estimate[2])

(chisq.session.pars = fitdistr(y, "chi-squared"), list(df = 1)))

chisq.session = rcauchy(1000, chisq.session.pars$estimate[1], chisq.session.pars$estimate[2])

ks.test(y, "pchisq", chisq.session.pars$estimate[1], chisq.session.pars$estimate[2])
ad.test(y, pchisq, chisq.session.pars$estimate[1], chisq.session.pars$estimate[2])
