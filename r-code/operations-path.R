source('./util/util.R')
util$pkgRequire("markovchain")
util$pkgRequire("xtable")


operations.path = new.env()
# This part of the code creates the table with the probabilities of change of each zoom level. This table is used in simulations of the dissertation and
# it includes just zoom in/out and pan operations (besides start/end)
operations.path$createOperationsPathWithZoomAndPan = function() {
  path = paste(PCLOUD_DIR, "mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/OperationsPath.csv", sep="/")
  operations = read.table(path, sep=",", dec=".", header=FALSE)
  head(operations)
  colnames(operations) = c("session_id", "zoom_level", "operation")
  tail(operations[,3])
  
  operacoes <- subset(operations, operations[,3] != "preview"
                      & operations[,3] != "start")
  
  head(operacoes)
  
  op.matrix <- matrix(nrow=23, ncol=23, dimnames=list(c(paste(1:21), "search", "route"), c(1:21, "search", "route")))
  (op.matrix <- apply(op.matrix, c(1, 2), function(x) 0)) # create a matrix with zeros
  
  op.seq <- c()
  
  for (i in 1:max(operacoes[,1])) {
    sub <- subset(operacoes, operacoes[,1] == i)
    if (nrow(sub) <= 2) {
      next
    }
    
    op.seq <- c(op.seq, "start/end")
    for (j in 1:nrow(sub)) {
      if (sub[j,3] %in% c("pan", "zoom_in", "zoom_out", "search", "route")) {
        op.seq <- c(op.seq, as.character(sub[j,2]))
      } else #if (sub[j,3] %in% c("search", "route"))# 
      {
        #op.seq <- c(op.seq, as.character(sub[j,3]))
      }
    }
    op.seq <- c(op.seq, "start/end")
    
  }
  
  (op.freq <- createSequenceMatrix(stringchar = op.seq))
  
  op.freq
  
  (op.dens <- op.freq[-c(1),-c(1)]/apply(op.freq[-c(1),-c(1)], MARGIN=c(1), FUN=function(x) sum(x)))
  op.dens[22,22] = 0
  
  op.dens[22,] = op.dens[22,] / sum(op.dens[22,])
  #round(op.dens, 3)
  op.dens
}

op.dens = operations.path$createOperationsPathWithZoomAndPan();op.dens
op.dens = op.dens[c(1:20, 22),c(1:20, 22)];op.dens
sum(op.dens)
apply(op.dens, MARGIN=c(1), FUN=function(x) sum(x))
op.dens = op.dens/apply(op.dens, MARGIN=c(1), FUN=function(x) sum(x))
sum(op.dens)
round(op.dens, 3)
op.dens
operations.path$start.prob <- op.dens[nrow(op.dens),]
operations.path$start.prob

####################################SEM START_END################################################################
# op.freq
# op.dens <- op.freq[-c(1, 23),-c(1, 23)]/apply(op.freq[-c(1, 23),-c(1, 23)], MARGIN=c(1), FUN=function(x) sum(x))
# op.dens[22,22] = 0
# op.dens[22,] = op.dens[22,] / sum(op.dens[22,])
# round(op.dens, 5)
#################################################################################################################

xtable(op.dens[,1:12], digits=2)

operations.path$start.prob <- op.dens[nrow(op.dens),]
operations.path$start.prob

path.synt <- c()

for (g in 1:1000) {
  SIZE <- 1000
  
  freq.frame <- as.data.frame(op.dens)
  freq.frame
  
  lastOp <- ""
  
  (U <- runif(1))
  for (i in 1:length(operations.path$start.prob)) {
    probs.sorted <- sort(operations.path$start.prob)
    
    if (U < sum(probs.sorted[1:i])) {
      lastOp <- rownames(as.matrix(probs.sorted))[i]
      break
    }
  }
  
  for (i in 1:SIZE) {
    probs <- freq.frame[lastOp,]
    probs.sorted <- sort(probs)
    
    U <- runif(1)    
    for (j in 1:length(probs.sorted)) {
      if (U < sum(probs.sorted[1:j])) {
        path.synt <- c(path.synt, colnames(probs.sorted)[j])
        lastOp <- colnames(probs.sorted)[j]
        break
      }
    }
    if (lastOp == "start/end") {
      break
    }
  }
}
length(path.synt[path.synt == "19"])/length(path.synt[path.synt != "start/end"])

num.ops <- c()
count <- 0
caminho <- c()
for (i in 1:length(path.synt)) {
  
  if (path.synt[i] != "start/end") {
    count = count + 1
    caminho <- c(caminho, path.synt[i])
  } else {
    #print(caminho)
    caminho <- c()
    num.ops <- c(num.ops, count)
    count = 0
  }
}

library(MASS)
geo.fit <- fitdistr(num.ops, "geometric")

max(num.ops)
max(norm.sim)
norm.sim <- rgeom(length(num.ops), geo.fit$estimate[1])
plot(ecdf(num.ops))
lines(ecdf(norm.sim), col=2)

plot(density(num.ops), ylim=c(0, 0.03))
lines(density(norm.sim), col=2)

niveis.zoom <- path.synt[path.synt != "search" & path.synt != "route" & path.synt != "start/end"]
niveis.zoom <- as.numeric(niveis.zoom)

plot(table(niveis.zoom), type="h")

#ks.test(num.ops, "rlnorm", 2.98278955, 1,27339640)



#########################################################################################################
# The code bellow is used to create the tables B.1, B.2, B.3 and B.4 of the dissertation. It includes route and search operations
path = "/media/arquivos/pcloud/mestrado/3-semestre/dissertacao/frameworkdeeventos/gerador-graficos/data/OperationsPath.csv"
operations = read.table(path, sep=",", dec=".", header=FALSE)
head(operations)
tail(operations[,3])

operacoes <- subset(operations, operations[,3] != "preview"
                    & operations[,3] != "start" & operations[,3] != "automatic" & operations[,3] != "place")

head(operacoes)

op.matrix <- matrix(nrow=23, ncol=23, dimnames=list(c(paste(1:21), "search", "route"), c(1:21, "search", "route")))
(op.matrix <- apply(op.matrix, c(1, 2), function(x) 0))

op.seq <- c()

for (i in 1:max(operacoes[,1])) {
  sub <- subset(operacoes, operacoes[,1] == i)
  if (nrow(sub) <= 2) {
    next
  }
  
  op.seq <- c(op.seq, "start/end")
  for (j in 1:nrow(sub)) {
    if (sub[j,3] %in% c("pan", "zoom_in", "zoom_out")) {
      op.seq <- c(op.seq, as.character(sub[j,2]))
    } else if (sub[j,3] %in% c("search", "route"))# 
    {
      op.seq <- c(op.seq, as.character(sub[j,3]))
    }
  }
  op.seq <- c(op.seq, "start/end")
  
}

(op.freq <- createSequenceMatrix(stringchar = op.seq))

op.freq

(op.dens <- op.freq/apply(op.freq, MARGIN=c(1), FUN=function(x) sum(x)))
op.dens[24,24] = 0

op.dens[24,] = op.dens[24,] / sum(op.dens[24,])
round(op.dens, 5)

xtable(op.dens[,19:24], digits=6)
