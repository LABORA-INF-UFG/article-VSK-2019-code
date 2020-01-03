source("/media/arquivos/pcloud/mestrado/4-semestre/r-scripts/util.R")

octave.path = "/media/arquivos/pcloud/mestrado/4-semestre/dissertacao/svnlabora/teses/msc/vinicius/texto/defesa/figs/octave/data"

random.path = "/media/arquivos/pcloud/mestrado/4-semestre/gogeo/resultados-testes2/random"
model.path = "/media/arquivos/pcloud/mestrado/4-semestre/gogeo/resultados-testes2/model"
guan.path = "/media/arquivos/pcloud/mestrado/4-semestre/gogeo/resultados-testes2/guan"

analyze.tests(paste(model.path, "/80-users.log", sep=""), "80-model")
analyze.tests(paste(random.path, "/90-users.log", sep=""), "80-random")
analyze.tests(paste(guan.path, "/80-users.log", sep=""), "80-help")

test.results = read.data(paste(random.path, "/80-users.log", sep=""))
#plot.eps("response-times-90-random")
plot(get.response.mean.times(test.results), type="l", log="y", main="", xlab="Tempo (s)", 
     ylab="Tempo médio de resposta (ms)", lwd=3)
axis(1, at=seq(0, 100, 25), labels=seq(350, 450, 25))
#dev.off()

test.results.random = read.data(paste(random.path, "/80-users.log", sep=""))
test.results.guan = read.data(paste(guan.path, "/80-users.log", sep=""))
test.results.model = read.data(paste(model.path, "/80-users.log", sep=""))

plot(get.response.mean.times(test.results.random), type="l", log="y", main="Random", xlab="Tempo (s)", 
     ylab="Tempo médio de resposta (ms)", lwd=3)
plot(get.response.mean.times(test.results.guan), type="l", log="y", main="Guan", xlab="Tempo (s)", 
     ylab="Tempo médio de resposta (ms)", lwd=3)
plot(get.response.mean.times(test.results.model), type="l", log="y", main="MP", xlab="Tempo (s)", 
     ylab="Tempo médio de resposta (ms)", lwd=3)

plot(get.resquests.per.sec(test.results.random), type="l", log="y", main="Random", xlab="Tempo (s)", 
     ylab="Req/seg", lwd=3)
plot(get.resquests.per.sec(test.results.guan), type="l", log="y", main="Guan", xlab="Tempo (s)", 
     ylab="Req/seg", lwd=3)
plot(get.resquests.per.sec(test.results.model), type="l", log="y", main="MP", xlab="Tempo (s)", 
     ylab="Req/seg", lwd=3)

mean(get.resquests.per.sec(test.results.random))
mean(get.resquests.per.sec(test.results.guan))
mean(get.resquests.per.sec(test.results.model))

plot(get.throughput(test.results.random), type="l", log="y", main="Random", xlab="Tempo (s)", 
     ylab="Throughput", lwd=3)
plot(get.throughput(test.results.guan), type="l", log="y", main="Guan", xlab="Tempo (s)", 
     ylab="Throughput", lwd=3)
plot(get.throughput(test.results.model), type="l", log="y", main="MP", xlab="Tempo (s)", 
     ylab="Throughput", lwd=3)

mean(get.throughput(test.results.model))
mean(get.throughput(test.results.model))
mean(get.throughput(test.results.model))

analyze.tests <- function(results.path, text) {
  print("------------------------------------------------------")
  print(paste("TESTE: ", text, sep=""))
  test.results = read.data(results.path)
  head(test.results[,8], 3)
  
  results.ok <- test.results[test.results[,10] == "OK",]
  results.ko <- test.results[test.results[,10] == "KO",]
  
  print(paste("FREQUÊNCIA DE ERROS:", round(nrow(results.ko) / nrow(test.results) * 100, 2)))
  
  response.times <- get.response.mean.times(test.results)
  
  acf(response.times, main=paste("Tempo de resposta:", text))
  req.size <- length(response.times)
  print("TEMPO DE RESPOSTA:")
  print(summary(response.times))
  plot(response.times#[floor(req.size/2):floor(req.size/2 + 2000)]#
       , type="l", main=text, xlab="Requisição", ylab="Tempo (ms)")
  
  test.duration <- get.test.duration(test.results)
  print(paste("DURAÇÃO DO TESTE: ", test.duration, sep=""))
  
  req.per.sec <- get.resquests.per.sec(test.results)
  
  cat(paste(req.per.sec), file=paste(octave.path, "/", text, ".csv", sep=""), append=TRUE, sep="\n")
  
  print("REQUISIÇÕES/SEG")
  print(summary(req.per.sec))
  plot(density(req.per.sec), main=text, xlab="Requisições/segundo", ylab="Probabilidade")
  plot(req.per.sec, type="l", main=text, xlab="Tempo (s)", ylab="Número de Requisições")
  acf(req.per.sec, main=paste("Requisições/seg:", text), lag.max=600)
}

get.throughput <- function(data) {
  start.times <- data[,7]
  end.times <- data[,9]
  response.sizes <- data[,12]
  
  start.time.init.zero <- as.numeric(start.times - min(start.times))
  end.time.init.zero <- as.numeric(end.times - min(start.times))
  
  throughput.per.sec <- c()
  throughput.list <- c()
  seconds <- 1
  
  for (i in 1:length(end.time.init.zero)) {
    throughput <- as.numeric(response.sizes[i]) / as.numeric(end.time.init.zero[i] - start.time.init.zero[i])
    throughput.list <- c(throughput.list, throughput)
    if (start.time.init.zero[i] >= seconds * 1000) {
      throughput.per.sec = c(throughput.per.sec, mean(throughput.list))
      throughput.list <- c()
      seconds <- seconds + 1
      print(seconds)
    }
  }  
  throughput.per.sec
}

get.response.mean.times <- function(data) {
  start.times <- data[,7]
  end.times <- data[,9]
  
  start.time.init.zero <- as.numeric(start.times - min(start.times))
  end.time.init.zero <- as.numeric(end.times - min(start.times))
  
  response.times.per.sec <- c()
  num.req <- 0
  response.times <- 0
  seconds <- 1
  for (i in 1:length(end.time.init.zero)) {
    num.req <- num.req + 1
    response.times <- response.times + (end.time.init.zero[i] - start.time.init.zero[i])
    
    if (start.time.init.zero[i] >= seconds * 1000) {
      response.times.per.sec = c(response.times.per.sec, response.times/num.req)
      response.times = 0
      num.req = 0
      seconds <- seconds + 1
    }
  }  
  response.times.per.sec
}

get.response.times <- function(data) {
  start.times <- data[,6]
  end.times <- data[,9]
  
  response.times <- end.times - start.times
  
  response.times
}

get.test.duration <- function(data) {
  start.time <- min(data()[,6])
  end.time <- max(data[,9])
  
  test.duration <- end.time - start.time
  test.duration
}

get.resquests.per.sec = function(data) {
  start.times <- data[,6]
  end.times <- data[,9]
  
  start.time.init.zero <- start.times - min(start.times)
  head(start.time.init.zero)
  
  req.per.sec <- c()
  num.req <- 0
  seconds <- 1
  for (i in 1:length(start.time.init.zero)) {
    num.req <- num.req + 1
    if (start.time.init.zero[i] >= seconds * 1000) {
      req.per.sec = c(req.per.sec, num.req)
      num.req = 0
      seconds <- seconds + 1
    }
  }
  req.per.sec
}

read.data <- function(filePath) {
  data <- read.table(file=filePath, sep="\t", dec=".", header=F)
  data <- subset(data, data[,12] != "http://localhost:80")
  data
}

#########################################################################################################################

response.times.all <- data.frame(mean=c(), sd=c(), number.of.clients=c(), type=c())

requests.per.sec.all <- data.frame(mean=c(), sd=c(), number.of.clients=c(), type=c())

QUANTILE <- 1

for (i in seq(10, 130, 10)) {  
  if (i <= 90) {
    random.data <- read.data(paste(random.path, "/", i, "-users.log", sep=""))
    response.time <- get.response.times(random.data)
    response.time <- response.time[response.time <= quantile(response.time, QUANTILE)]
    response.times.all <- rbind(response.times.all, data.frame(mean=response.time, number.of.clients=i, type="A"))
    
    requests.per.sec <- get.resquests.per.sec(random.data)
    requests.per.sec <- requests.per.sec[requests.per.sec <= quantile(requests.per.sec, QUANTILE)]
    requests.per.sec.all <- rbind(requests.per.sec.all, data.frame(mean=requests.per.sec, number.of.clients=i, type="A"))
  }
  
  model.data <- read.data(paste(model.path, "/", i, "-users.log", sep=""))
  response.time <- get.response.times(model.data)
  response.time <- response.time[response.time <= quantile(response.time, QUANTILE)]
  response.times.all <- rbind(response.times.all, data.frame(mean=response.time, number.of.clients=i, type="MP"))
  
  requests.per.sec <- get.resquests.per.sec(model.data)
  requests.per.sec <- requests.per.sec[requests.per.sec <= quantile(requests.per.sec, QUANTILE)]
  requests.per.sec.all <- rbind(requests.per.sec.all, data.frame(mean=requests.per.sec, number.of.clients=i, type="MP"))
  
  if (i <= 100) {
    guan.data <- read.data(paste(guan.path, "/", i, "-users.log", sep=""))
    response.time <- get.response.times(guan.data)
    response.time <- response.time[response.time <= quantile(response.time, QUANTILE)]
    response.times.all <- rbind(response.times.all, data.frame(mean=response.time, number.of.clients=i, type="MG"))
    
    requests.per.sec <- get.resquests.per.sec(guan.data)
    requests.per.sec <- requests.per.sec[requests.per.sec <= quantile(requests.per.sec, QUANTILE)]
    requests.per.sec.all <- rbind(requests.per.sec.all, data.frame(mean=requests.per.sec, number.of.clients=i, type="MG"))
  }
  print(i/10)
}

library(ggplot2)
library(grid)
library(scales)

response.times.all2 <- response.times.all#[response.times.all[,3] == "A" & response.times.all[,2] == 90,]
response.times.all2 <- response.times.all[response.times.all[,3] != "MG",]
head(response.times.all2)
sd(response.times.all2$mean)

dodge <- position_dodge(width=5)

sumSE <- util$summarySE(response.times.all2, measurevar="mean", groupvars=c("number.of.clients", "type"))
#sumSE

optns <- theme_bw() + theme (
  legend.position = c(0.2,0.9),
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.key.size = unit(2.5, "lines"),
  legend.key = element_blank(),
  text = element_text(size=24)
)

util$plot.eps("response-times-ci")
ggplot(sumSE, aes(x=number.of.clients, y=mean, colour=type), log="y") + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci, lty=type), lwd=1, width=8.5) +
  geom_line(aes(lty=type)) + 
  geom_point(aes(shape=type, fill=type), size=3) +
  xlab("Number of clients") +
  ylab("Response time (ms)") +
  scale_y_log10(breaks=c(1,10,50,100,200)) +
  scale_x_discrete(breaks=seq(10,130,20)) +
  optns
dev.off()

plot.eps("response-times-sd")
ggplot(sumSE, aes(x=number.of.clients, y=mean, colour=type)) + 
  geom_errorbar(aes(ymin=lower, ymax=mean+sd, lty=type), position=dodge, lwd=1, width=8.5) +
  geom_line(aes(lty=type)) + 
  geom_point(aes(shape=type, fill=type), size=3) +
  xlab("Número de clientes") +
  ylab("Tempo de resposta (ms)") +
  scale_y_log10(breaks=c(1,10,50,100,200, 500, 1000, 1500, 2000)) +
  scale_x_discrete(breaks=seq(10,130,20)) +
  optns
dev.off()

head(requests.per.sec.all)
requests.per.sec.all2 <- requests.per.sec.all#[requests.per.sec.all[,3] != "R" | requests.per.sec.all[,2] != 90,]
requests.per.sec.all2 <- requests.per.sec.all[requests.per.sec.all[,3] != "MG",]

sumSE <- util$summarySE(requests.per.sec.all2, measurevar="mean", groupvars=c("number.of.clients", "type"))
#sumSE

optns <- theme_bw() + theme (
  legend.position = c(0.82,0.2),
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.key.size = unit(2.5, "lines"),
  legend.key = element_blank(),
  text = element_text(size=24)
)
util$plot.eps("requests-per-sec-ci")
ggplot(sumSE, aes(x=number.of.clients, y=mean, colour=type), log="y") + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci, lty=type), lwd=1, width=8.5) +
  geom_line(aes(lty=type)) + 
  geom_point(aes(shape=type, fill=type), size=3) +
  xlab("Number of clients") +
  ylab("Requests per second") +
  scale_y_log10(breaks=c(1,10, 25, 50, 100,200)) +
  optns
dev.off()

plot.eps("requests-per-sec-sd")
ggplot(sumSE, aes(x=number.of.clients, y=mean, colour=type), log="y") + 
  geom_errorbar(aes(ymin=lower, ymax=mean+sd, lty=type), lwd=1, position=dodge, width=8.5) +
  geom_line(aes(lty=type)) + 
  geom_point(aes(shape=type, fill=type), size=3) +
  xlab("Número de clientes") +
  ylab("Requisições por segundo") +
  scale_x_discrete(breaks=seq(10,130,20)) +
  optns
dev.off()

head(requests.per.sec.all[requests.per.sec.all$type == "A",])

plot(requests.per.sec.all[requests.per.sec.all$type == "MG" & requests.per.sec.all$number.of.clients == 40,1], type="l",
    xlab="Tempo (s)", ylab="Requisições/segundo")

acf(requests.per.sec.all[requests.per.sec.all$type == "MG" & requests.per.sec.all$number.of.clients == 200,1], lag.max=600)
acf(response.times.all[response.times.all$type == "MG" & response.times.all$number.of.clients == 40,1], lag.max=600)
 