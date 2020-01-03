l.max <- 21
# lambda.arg <- 0.5
# omega.arg <- 0.4
lambda.arg <- guan$lambda
omega.arg <- guan$w

zoomIn <- function(l, lambda) {
  1 - (l/l.max)^lambda
}

zoomOut <- function(l, w, lambda) {
  w * (l/l.max)^lambda
}

pan <- function(l, w, lambda) {
  (1 - w) * (l/l.max)^lambda
}

zoom.in.val <- c()
zoom.out.val <- c()
pan.val <- c()

for (l in 0:21) {
  zoom.in.val <- c(zoom.in.val, zoomIn(l, lambda.arg))
  zoom.out.val <- c(zoom.out.val, zoomOut(l, omega.arg, lambda.arg))
  pan.val <- c(pan.val, pan(l, omega.arg, lambda.arg))
  #lambda.arg <- lambda.arg + 0.15
}

util$plot.eps("custom-help-operation-sequence")
plot(op.matrix.freq[,1], type="n", col=1, ylim=c(0, 1), xlim=c(0, 21), ylab="Frequency", xlab="Zoom level")

lines(x=1:21, y=op.matrix.freq[,1], lw=2, col=2)
lines(x=1:21, y=op.matrix.freq[,2], lw=2, col=3)
lines(x=1:21, y=op.matrix.freq[,3], lw=2, col=4)
lines(x=0:21, y=pan.val, lw=4, lty=2, col=2)
lines(x=0:21, y=zoom.in.val, lw=4, lty=2, col=3)
lines(x=0:21, y=zoom.out.val, lw=4, lty=2, col=4)

legend("topright", legend=c("Pan", "Zoom in", "Zoom out"), fill=c(2, 3, 4))
dev.off()
