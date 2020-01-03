library(mixtools)

wait = faithful$waiting
mixmdl = normalmixEM(wait)
plot(mixmdl,which=2)
lines(density(wait), lty=2, lwd=2)

x <- 0:10

mixexplik <- function(p,lambda1,lambda2) { 
  log(p*dexp(x,lambda1) + (1-p)*dexp(x,lambda2)) 
} 

library(stats4)

mle(mixexplik, start=list(p=0.615, lambda1=0.03, lambda2=0.148), method = "L-BFGS-B")
