util$pkgRequire("ggplot2")
util$pkgRequire("stats4")

path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/TilePopularity.csv"
tilepop = read.table(file=path, sep=" ", dec=".")

head(tilepop, 3)
dim(tilepop)
summary(tilepop)
tiles.count <- as.numeric(tilepop[[2]])
sum(tiles.count)

summary(tilepop[,2])

plot(ecdf(tiles.count))

vec <- 1:nrow(tilepop)
summary(vec)

tilesMod <- list(nums=vec, qtd=sort(tiles.count, decreasing = T))
summary(as.data.frame(tilesMod))

Zipf_plot(as.data.frame(tilesMod))

rf <- data.frame(f=sort(tilepop[,2], decreasing = T), r=vec)

summary(rf)

quantile(rf$r, 0.999)

#plot.eps("tile-zipf")
plot(log(rf$r),log(rf$f), xlab="Ranking", ylab="Qtd. Acessos")
lm(log(rf$f)~log(rf$r))
lines(x=log(rf$r),y=9.017--1.414*log(rf$r), col=2, lw=3)
#dev.off()

######################

hits <- vector(length=max(tilepop[,2]));length(hits)

head(tilepop[,2])

for (hit in tilepop[,2]) {
  if (hit > 1000) {
    print(hit)
  }
  hits[hit] = hits[hit]+1
}

vecs <- c()

last.hit <- 0

for (hit in 1:length(hits)) {
  if (hits[hit] != 0) {
    last.hit <- last.hit + 1
    vecs <- c(vecs, hit)
  }
}

max(tilepop[,2])
summary(hits)
summary(vecs)

hits <- hits[hits != 0]

summary(hits)

rf <- data.frame(f=hits, r=vecs)

quantile(rf$r, 0.999)
summary(rf$r)

util$plot.eps("tiles-popularity")
plot(rf$r,rf$f, ylab="Number of accesses", xlab="Number of tiles with x accesses", log="xy", type="p")
#abline(lm(log10(rf$f)~log10(rf$r)), col=2)
lm(log10(rf$f)~log10(rf$r))
#lines(x=log(rf$r),y=9.020-1.342*log(rf$r), col=2, lw=3)
dev.off()

length(rf$f)

fr <- rf$f

p <- fr/sum(fr)

lzipf <- function(s,N) -s*log(1:N)-log(sum(1/(1:N)^s))

opt.f <- function(s) sum((log(p)-lzipf(s,length(p)))^2)

opt <- optimize(opt.f,c(0.5,10))
opt

ll <- function(s) sum(fr*(s*log(1:length(fr))+log(sum(1/(1:length(fr))^s))))

fit <- mle(ll,start=list(s=1))

summary(fit)

s.sq <- opt$minimum
s.ll <- coef(fit);s.ll

max(rf$r)

util$plot.eps("tiles-popularity-zipf")
plot(rf$r,p,log="xy", ylab="Frequency", xlab="Index")
#lines(1:length(fr),exp(lzipf(s.sq,length(fr))),col=2, lwd=4)
lines(1:length(fr),exp(lzipf(s.ll,length(fr))),col=2, lwd=4)
dev.off()


