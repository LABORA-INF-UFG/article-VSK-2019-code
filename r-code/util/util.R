source('./util/constants.R')

#Create a new environment for this utility functions
util = new.env()

#Require a package and install it in case it is not installed yet.
#If the package does not exists, the function informs the user.
util$pkgRequire <- function(package.name) {
  if (!require(package.name,character.only = TRUE))
  {
    install.packages(package.name,dep=TRUE)
    if(!require(package.name,character.only = TRUE)) stop("Package not found")
  }
}

util$loadData <- function(filename, separator=",", dec=".", hasHeader=FALSE) {
  path = paste(DATA_COLLECTED_PATH, "/", filename, sep="")
  table = read.table(path, sep=separator, dec=".", header=hasHeader)
  table
}

util$plot.eps = function(filename, margin=c(3.6, 3.5, 0.5, 0.8), mgp=c(2.5, 1, 0)) {
  setEPS()
  postscript(paste(PLOT_PATH, filename, ".eps", sep=""))
  par(mar=margin)
  par(mgp=mgp)
  par(cex=2)
}

util$plot.eps.config.paper = function(filename, margin=c(3.6, 3.5, 0.5, 0.8), mgp=c(2.5, 1, 0), paper="default", width=0, height=0) {
  setEPS()
  postscript(paste(PLOT_PATH, filename, ".eps", sep=""), paper=paper, width=width, height=height)
  par(mar=margin)
  par(mgp=mgp)
  par(cex=2)
}

util$plot.pdf = function(filename, margin=c(3.6, 3.5, 0.5, 0.8)) {
  pdf(paste(PLOT_PATH, filename, ".pdf", sep="")) 
  par(mar=margin)
  par(mgp = c(2.5, 1, 0))
  par(cex=2)
}

util$summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  type = c()
  
  for (i in 1:nrow(datac)) {
    if (datac$type[i] == "A") {
      type = c(type, "MA")
    } else if (datac$type[i] == "MP") {
      type = c(type, "MUSe-GM")
    } else {
      type = c(type, "HELP")
    }
  }
  
  datac$type = type
  
  lower <- c()
  
  for (i in 1:nrow(datac)) {
    if (datac$mean[i] - datac$sd[i] < 0) {
      lower = c(lower, 0)
    } else {
      lower = c(lower, datac$mean[i] - datac$sd[i])
    }
  }
  
  datac$lower = lower
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

util$writeToFile <- function(data, filename) {
  write.table(data, file=paste(DATA_TO_GNU_PLOT_PATH, filename, sep=""),
        append = FALSE, sep = ",", row.names = F, quote = F)
}

while("util" %in% search())
  detach("util")
attach(util)

