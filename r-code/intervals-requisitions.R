source('/media/arquivos/Dropbox/mestrado/4-semestre/r-scripts/pareto.R')
library(MASS)
library(VGAM)

path = "/media/arquivos/Dropbox/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/IntervalsRequisitions.csv"
intervals.readed = read.table(path, sep=",", dec=".", header=FALSE)
dim(intervals.readed)

head(intervals.readed, 3)
#2902 + 3152 + 1918 + 1938

operations <- subset(intervals.readed, 
                      (intervals.readed[[3]] %in% c("usernum.169")))

think.time <- as.numeric(intervals.readed[[1]])
think.time <- think.time[!is.na(think.time)]

(x <- subset(intervals.readed, intervals.readed[[1]]  < 10))
dim(x)

think.time <- think.time[think.time != 0]

summary(think.time)

q99 <- quantile(think.time, 0.99) 
q99

q01 <- quantile(think.time, 0.1)
q01
intervals <- think.time#[think.time >= 1000]
intervals <- intervals#[intervals < q99]

size = length(intervals)
#intervals <- intervals[(intervals >= 200 & intervals <= (86400000 / 24))]
#intervals <- intervals[intervals >= (86400000 / 24)]

length(intervals)
summary(intervals)

g1000 <- think.time[think.time >= 1000]

length(g1000) / length(think.time)

# pareto.MLE <- function(X)
# {
#   n <- length(X)
#   m <- min(X)
#   a <- n/sum(log(X)-log(m))
#   return( c(m,a) ) 
# }
# 
# # example. 
# set.seed(1001)
# #hist(intervals)
# #plot(ecdf(intervals))
# 
# pareto.params <- pareto.MLE(intervals)
# pareto.params

#z = rpareto(length(intervals), pareto.params[1], pareto.params[2])
#plot(density(z))
#plot(ecdf(z))

#qqplot(x=z, y=intervals)
#abline(0, 1)

#summary(z)
#summary(intervals)

#fitdistr(intervals, "lognormal")
#z <- rpareto(2000, pareto.params[1], pareto.params[2])

#lognormal pars = 3.387420 2.777956, xmin = 1079

#plot(ecdf(intervals))

max(intervals)

library(ineq)

ineq(intervals,type="Gini")
plot(Lc(intervals),col="darkred",lwd=2)

plot.eccdf <- function(x, log) {
  f <- ecdf(x)
  
  plot(sort(x), 1-f(sort(x)), type="s", lwd=1, log=log)
}

plot.eccdf(intervals, log="xy")

pareto.params <- pareto.fit(intervals, threshold=min(intervals))
pareto.params

plot.eucdf.loglog(x=intervals, type="l")
x.pareto <- rpareto(n=5000, pareto.params$xmin, pareto.params$exponent)
plot.eucdf.loglog(x=x.pareto, type="l", col="red")

library(poweRlaw)

pl_m <- conpl$new(intervals)

est_pl = estimate_xmin(pl_m)
est_pl
pl_m$setXmin(est_pl)

plot(pl_m)
lines(pl_m, col=2)

x.pareto <- rpareto(n=length(intervals), est_pl$xmin, est_pl$pars)
#plot.eucdf.loglog(x=x.pareto, type="l", col="red")
ks.test(intervals[intervals >= est_pl$xmin], x.pareto)

#bs_p = bootstrap_p(pl_m)
#bs_p
#plot(bs_p)

####################################################################

body <- intervals[intervals <= est_pl$xmin]
body <- body[body > 0.200]
length(body)
body <- sample(body, 5000)

#plot(ecdf(body))
plot(density(body))

length(body[body <= 4000]) / length(body)

kmeans(body, 2)

summary(body)
length(body)

library(nortest)

lognormal.fit <- fitdistr(body, "lognormal")
lognormal.params <- lognormal.fit$estimate
lognormal <- rlnorm(length(body), meanlog=lognormal.params[1],
                    sdlog=lognormal.params[2])

#plot(ecdf(lognormal))

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

qqplot(body, wei.fitted, log="xy", xlab="Intervals", ylab="Lognormal")
abline(0,1, col="red")

normal.fit <- fitdistr(body, "gamma")
normal.params <- normal.fit$estimate
normal.params
normal <- rgamma(length(body), normal.params[1], normal.params[2])

summary(normal)
#plot(density(wei.fitted))
#plot(ecdf(wei.fitted))

ks.test(body, normal)

qqplot(body, normal, log="xy", xlab="Intervals", ylab="Lognormal")
abline(0,1, col="red")

p <- 0.615

dist <- p * rexp(10000, 0.030) + (1 - p) * rexp(10000, 0.148)
plot(density(dist))

plot(density(body))

qqplot(body, dist, log="xy", xlab="Intervals", ylab="Lognormal")
abline(0,1, col="red")

