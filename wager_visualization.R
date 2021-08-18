if (!require("pacman")) install.packages("pacman")
pacman::p_load("here")

suppressWarnings(my_proj_path <- here())

if (!file.exists(file.path(my_proj_path,"animations"))) {
  dir.create(file.path(my_proj_path, "animations"))
}
setwd(file.path(my_proj_path, "animations"))

library(lattice)
library(tigerstats)

n = 20000

png(file="simwager%02d.png", width=600, height=400)

wagerLabels = c("Wager 3", "Wager 4")

outsp = seq(1,6)

for (i in 1:30){
  
  die3 = sample(outsp,n,replace=T)
  die4 = sample(outsp,n,replace=T)
  profit3 = 3*(die3>3) - 3*(die3<=3)
  profit4 = 1.5*(die4>2) - 3*(die4<=2)
  
  runningprf3 = cumsum(profit3)/seq(1,n)
  runningprf4 = cumsum(profit4)/seq(1,n)
  
  # put together a data frame in long form
  df3 <- data.frame(wager=rep(3,length(runningprf3)),runningprf=runningprf3,tries=seq(1,n))
  df4 <- data.frame(wager=rep(4,length(runningprf4)),runningprf=runningprf4,tries=seq(1,n))
  df <- rbind(df3, df4)
  
  pp <- xyplot(runningprf~tries, data=df,
               groups = wager,
               par.settings = list(superpose.line = list(col = c("blue","red"),
                                                         lwd = 1)),
               auto.key=list(space="top", columns=2,
                             text=c("Wager3 = 3(die>3) - 3(die<=3)", "Wager 4 = 1.5(die>2) - 3(die<=2)"),
                             title="Running profits from two different wagers", 
                             cex.title=2,
                             lines=TRUE, points=FALSE),
               xlab="Tries",
               xlim = c(1, n),
               scales=list(cex=c(1.1, 1.1), # increase font size
                           x = list(log = T), 
                           y = list(log = F),
                           alternating = 1,   # axes labels left/bottom 
                           tck = c(1,0)),   # ticks only with labels
               ylab="Profit",
               ylim = c(-3.5, 3.5),
               type=c("l"),
               panel=panel.superpose,
               panel.groups = function( x,y,group.number,...) {
                 # print(levels(factor(df$wager))[group.number])
                 panel.abline( h=y[ which(y==0.0) ], lty = "dotted", col = "black")
                 panel.grid(v=-1, h=-1, lty=3)
                 xt <- x[x==log(min(x)+1)] # find latest year
                 yt <- y[x==min(x)] # find value at latest year
                 panel.text(xt, yt, labels=wagerLabels[group.number], 
                            pos=4,  # show labels on right side
                            ...)
                 panel.xyplot(x,y,...)
               }
  )
  print(pp)
}

dev.off()

# This section is machine dependent. 
# It assumes we are in an Ubuntu installation with the program convert from ImageMagick

# converting .png file in .gif using ImageMagick
system("/usr/bin/convert -delay 40 *.png wager_comp_sim.gif")

# Remove .png file
file.remove(list.files(pattern=".png"))
