

par(mfcol=c(2,2))
setwd(" ")
data1 <- read.table("1.dat")
data2 <- read.table("2.dat")
x1 <- data1$V1/5
y1 <- data1$V4
x2 <- data2$V1/5
y2 <- data2$V4


plot(x1,y1, "line", col="skyblue", ylim=c(0,1.1), xlab="Time (s)", ylab="FRET", lwd=2)
axis(1, at = seq(0,90,20))
axis(2, at = seq(0,1.1), )
grid(col="darkgray")
lines(x1, data1$V5, col="orange", lwd=2)

plot(x2,y2, "line", col="skyblue", ylim=c(0,1.1), xlab="Time (s)", ylab="FRET", lwd=2)
axis(1, at = seq(0,90,20))
axis(2, at = seq(0,1.1), )
grid(col="darkgray")
lines(x2, data2$V5, col="orange", lwd=2)



