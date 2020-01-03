util$pkgRequire("ADGofTest")
util$pkgRequire("EnvStats")
util$pkgRequire("fitdistrplus")

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/NumRequisitionsPerUser.csv"
acessos = read.table(path, sep=",", dec=".", header=FALSE)
acessos
head(acessos,3)

nums <- acessos[[2]]
users <- as.vector(acessos$user)
quantile(nums, 0.01)
acessos

filtered.users <- list()
count <- 1

user.filter <- function(users, nums, filter) {
  for (num in nums) {
    if (num > filter) {
      user <- list()
      user[[users[count]]] <- num
      filtered.users <- c(filtered.users, user)
    }
    count <- count + 1
  }
  filtered.users
}

q75 <- quantile(nums, .75)

filtered.users <- user.filter(users, nums, q75)

#length(nums[nums > 1000])
#nums[nums > 1000]
data.users <- data.frame(users=users, nums=nums)
plot(density(nums))

kmeans(nums, 6)

path = "/home/vinicius/pCloudDrive/sync/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/NumOperationsPerSession.csv"
acessos = read.table(path, sep=",", dec=".")

head(acessos,3)

summary(acessos)
class(acessos)

op.per.access <- acessos[[1]]
users <- as.vector(acessos[[1]])

op.per.access <- op.per.access[op.per.access > 0]
summary(op.per.access)

parametersWei = fitdistr(op.per.access, "weibull")

guan$session.length.estimated.from.our.data = parametersWei;guan$session.length.estimated.from.our.data

parametersWei

size <- length(op.per.access)

wei <- rweibull(10000, shape=parametersWei$estimate[1], scale=parametersWei$estimate[2])
summary(wei + 1)
quantile(op.per.access, 0.98)
quantile(wei, 0.98)

ks.test(op.per.access, wei)
ks.test(op.per.access, "pweibull", parametersWei$estimate[1], parametersWei$estimate[2])
ad.test(op.per.access, pweibull, parametersWei$estimate[1], parametersWei$estimate[2])

qqplot(op.per.access, wei)
abline(0, 1, col="red")

plot(ecdf(op.per.access),ylim=c(0,1),lw=4, log="x", xlim=c(min(op.per.access), max(op.per.access)), main="", ylab="% do Total", xlab="Nº de ações por sessão")
lines(ecdf(wei),lw=4,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Weibull"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)

log.pars = fitdistr(op.per.access, "lognormal")
log.pars

(size <- length(op.per.access))

logn <- rlnorm(10000, log.pars$estimate[1], log.pars$estimate[2])

ks.test(op.per.access, logn)
qqplot(op.per.access, logn)
abline(0, 1, col="red")
ks.test(op.per.access, "plnorm", log.pars$estimate[1], log.pars$estimate[2])
ad.test(op.per.access, plnorm, log.pars$estimate[1], log.pars$estimate[2])

summary(op.per.access)
summary(wei + 1)
summary(logn)

util$plot.eps("sessions-length-cdf")
ecdfPlot(op.per.access, ecdf.lty=1, ecdf.lw=6, ecdf.col=1, ylab=expression("Prob(Number of actions " <= " x)"), 
         xlab="Number of actions", main="", xlim=c(min(wei), max(op.per.access)))
ecdfPlot(wei, ecdf.lty=2, ecdf.lw=6, ecdf.col=2, ylab=expression("Prob(Number of actions " <= " x)"), 
         xlab="Number of actions", main="", add=T)
legend("bottomright", legend = c("Observed", "Weibull"),
       col = c("black","red"), pch=c(NA,NA), lty=c(1, 2),
       lwd=c(4,4),bty="n", pt.cex=2)
dev.off()

summary(op.per.access)

plot.eps("sessions-length-cdf", margin=c(3.6, 3.6, 0.5, 0.4))
ecdfPlot(op.per.access,ylim=c(0,1), log="x", xlim=c(min(op.per.access), max(op.per.access)), main="", 
     ylab=expression("Prob(Nº de ações " <= " x)"), xlab="Nº de ações")
ecdfPlot(logn,verticals=T, col="red")
legend("bottomright", legend = c("Observado", "Lognormal"),
       col = c("black","red"), pch=c(NA,NA), lty = c(1, 1),
       lwd=c(3,3),bty="n", pt.cex=2)
dev.off()

qqplot(op.per.access, logn)
abline(0, 1, col="red")

matrix.data <- data.frame(users=acessos[[1]], num=acessos[[3]])

########################################################
mydata <- na.omit(op.per.access) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata, 3) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit)

library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

library(fpc)
plotcluster(mydata, fit$cluster)

########################################################



quantile(op.per.access, .95)

summary(op.per.access)
plot(density(op.per.access))
plot(ecdf(op.per.access))
boxplot(op.per.access)
hist(op.per.access)

x.poi <- rpois(n=200, lambda=2.5)

qqplot(x.poi, op.per.access, main="QQ-Plot distr. Weibull")
abline(0, 1)

library(fBasics)
skewness(op.per.access)
kurtosis(op.per.access)

library(MASS)
fitdistr(op.per.access, "gamma")

plot(ecdf(rgamma(n=20000, shape=0.898751233, rate=0.045449653)))

x.gamma <- rgamma(n=length(op.per.access), shape=0.898751233, rate=0.045449653)

qqplot(x.gamma, op.per.access)
abline(0, 1)
summary(x.gamma)
summary(op.per.access)

plot(ecdf(op.per.access))
plot(ecdf(x.gamma), add=TRUE, col="red")

ks.test(op.per.access[1:200],"pgamma",shape=0.898751233, rate=0.045449653)

library(vcd)## loading vcd package
gf<-goodfit(op.per.access,type= "poisson",method= "MinChisq")
summary(gf)

library(fitdistrplus)
fitdist(op.per.access, distr="gamma")

x <- matrix(data=op.per.access[1:1300], nrow=1300)

f <- apply(x, 2,  fitdist, "gamma")

for(i in 1:13)
  plot(f[[i]])



library(animation)
saveGIF({for(i in 1:13) plot(f[[i]])})

apply((sapply(f, "[[", "estimate")),1, summary)

ExpectedMean <- function(mu, sig) exp(mu+ sig^2/2)
## Expected std
ExpectedStd <- function(mu, sig) sqrt((exp(sig^2)-1)*exp(2*mu + sig^2))

summary(apply(sapply(f, "[[", "estimate"), 2, function(x) ExpectedMean(x[1], x[2])))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6529  0.6665  0.6747  0.6748  0.6819  0.7087 
summary(apply(sapply(f, "[[", "estimate"), 2, function(x) ExpectedStd(x[1], x[2])))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.06604 0.07880 0.08212 0.08316 0.08794 0.10170 

## Let's look at the goodness of fit statistics to get an
## idea how much variance we can expect there:
gof.ln <- lapply(f, gofstat)
gof.test <- lapply(gof.ln, function(x) data.frame(x[c("chisqpvalue", "cvm", "ad", "ks")]))
apply(do.call("rbind", gof.test), 2, summary)
