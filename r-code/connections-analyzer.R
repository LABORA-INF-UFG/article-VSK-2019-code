source("/media/arquivos/pcloud/mestrado/4-semestre/r-scripts/util.R")

random.path.conn = "/media/arquivos/pcloud/mestrado/4-semestre/gogeo/resultados-testes2/random/connections"
model.path.conn = "/media/arquivos/pcloud/mestrado/4-semestre/gogeo/resultados-testes2/model/connections"
guan.path.conn = "/media/arquivos/pcloud/mestrado/4-semestre/gogeo/resultados-testes2/guan/connections"

#########################################################################################################################

connections.all <- data.frame(mean=c(), sd=c(), number.of.clients=c(), type=c())

for (i in seq(10, 130, 10)) {  
  if (i <= 90) {
    connections <- read.table(file=paste(random.path.conn, "/", i, "-connections.log", sep=""), sep="", dec=".", header=F)
    connections <- connections[connections != 0]
    connections.all <- rbind(connections.all, data.frame(mean=connections, number.of.clients=i, type="A"))
  }
  
  connections <- read.table(file=paste(model.path.conn, "/", i, "-connections.log", sep=""), sep="", dec=".", header=F)
  connections <- connections[connections != 0]
  connections.all <- rbind(connections.all, data.frame(mean=connections, number.of.clients=i, type="MP"))
  
  if (i <= 100) {
    connections <- read.table(file=paste(guan.path.conn, "/", i, "-connections.log", sep=""), sep="", dec=".", header=F)
    connections <- connections[connections != 0]
    connections.all <- rbind(connections.all, data.frame(mean=connections, number.of.clients=i, type="MG"))
  }
  print(i/10)
}

library(ggplot2)
library(grid)

head(connections.all)
connections.all2 = connections.all[connections.all[,3] != "MG",]
head(connections.all2)

sumSE <- util$summarySE(connections.all2, measurevar="mean", groupvars=c("number.of.clients", "type"))
#sumSE

dodge <- position_dodge(width=1)

optns <- theme_bw() + theme (
  legend.position = c(0.2,0.9),
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.key.size = unit(2.5, "lines"),
  legend.key = element_blank(),
  text = element_text(size=24)
)

util$plot.eps("connections-mean-sd")
ggplot(sumSE, aes(x=number.of.clients, y=mean, colour=type), log="y") + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, lty=type), position=dodge, lwd=1, width=8.5) +
  geom_line(aes(lty=type)) + 
  geom_point(aes(shape=type, fill=type), size=3) +
  xlab("Número de clientes") +
  ylab("Número de conexões") +
  scale_x_discrete(breaks=seq(10,130,20)) +
  optns
dev.off()

util$plot.eps("connections-mean-ci")
  ggplot(sumSE, aes(x=number.of.clients, y=mean, colour=type), log="y") + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci, lty=type), position=dodge, lwd=1, width=8.5) +
  geom_line(aes(lty=type)) + 
  geom_point(aes(shape=type, fill=type), size=3) +
  xlab("Number of clients") +
  ylab("Number of connections") +
  scale_x_discrete(breaks=seq(10,130,20)) +
  optns
dev.off()

head(connections.all)

acf(connections.all[connections.all$type == "MP" & connections.all$number.of.clients == 50, 1], lag.max=600)

plot(connections.all[connections.all$type == "MG" & connections.all$number.of.clients == 80, 1][seq(1, 1200, 2)], type="l",
     xlab="Tempo (s)", ylab="Conexões/segundo")

acf(requests.per.sec.all[requests.per.sec.all$type == "MG" & requests.per.sec.all$number.of.clients == 200,1], lag.max=600)
acf(response.times.all[response.times.all$type == "MG" & response.times.all$number.of.clients == 40,1], lag.max=600)
