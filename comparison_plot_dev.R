
library(tigerstats)

n = 2000
outsp = seq(1,6)
die3 = sample(outsp,n,replace=T)
die4 = sample(outsp,n,replace=T)
profit3 = 3*(die3>3) - 3*(die3<=3)
profit4 = 1.5*(die4>2) - 3*(die4<=2)
runningprf3 = cumsum(profit3)/seq(1,n)
runningprf4 = cumsum(profit4)/seq(1,n)

# plot(runningprf3, xlab="Tries", ylab="Profit", type="l")
# lines(runningprf4, col="red", lty=2)
# abline(h=0, col="grey")

# put together a data frame in long form
df3 <- data.frame(wager=rep(3,length(runningprf3)),runningprf=runningprf3,tries=seq(1,n))
df4 <- data.frame(wager=rep(4,length(runningprf4)),runningprf=runningprf4,tries=seq(1,n))
df <- rbind(df3, df4)

png(file="simwager%02d.png", width=600, height=400)

pp <- xyplot(runningprf~tries, data=df,
       groups = wager,
       scales=list(cex=.8, col="black", x = list(log = T), y = list(log = F)),
       auto.key = TRUE,
       par.settings = list(superpose.symbol = list(col = c("blue","red"),
                                                   pch = 1),
                           superpose.line = list(col = c("blue","red"),
                                                 lwd = 1)),
       xlab="Tries",
       xlim = c(1, n),
       ylab="Profit",
       ylim = c(-3.0, 3.0),
       main="Running profits from wagers 3 (blue) and 4 (red)",
       type=c("p","b"),
       # panel = function(x,y) {
       #   panel.xyplot(x,y, type = 'l');
       #   panel.lines(x=log(1:n), y=rep(0.0,n), col="black")
       # }) 
)

print(pp)
dev.off()