#!/usr/bin/env Rscript
# Run from command line with RScript NAME_OF_FILE <ARGUMENT1,...>

width <- 1200
height <- 800

args = commandArgs(trailingOnly=TRUE)

if (length(args)>1) {
  width = as.numeric(args[1])
  height = as.numeric(args[2])
} else {
  cat("If both width and height aren't provided I will use defaults.")
}
cat(paste0("Creating images of width=", width, " by height=", height,"."))

library(tigerstats)

n = 2000
wagerLabels = c("Wager3 = 300(die>3) - 300(die<=3)", "Wager 4 = 150(die>2) - 300(die<=2)")
outsp = seq(1,6)
die3 = sample(outsp,n,replace=T)
die4 = sample(outsp,n,replace=T)
cashflow3 = 300 - 600*(die3>3)
cashflow4 = 300 - 450*(die4>2)

cash3 = sapply(seq_along(1:n), 
               function(i){ ifelse(i==1, -300, 
                                   sum(cashflow3[1:(i-1)])+(300-600*(die3[i-1]>3)))})
cash4 = sapply(seq_along(1:n), 
               function(i){ ifelse(i==1, -300, 
                                   sum(cashflow4[1:(i-1)])+(300-450*(die4[i-1]>2)))})


# put together a data frame in long form
# first the wage 4 so the lattice barchart format c(1,2) puts the wager 3 last or on top
df3 <- data.frame(wager=rep(wagerLabels[1],length(cash3)),
                  cashflow=cash3,
                  tries=seq(1,n))
df4 <- data.frame(wager=rep(wagerLabels[2],length(cash4)),
                  cashflow=cash4,
                  tries=seq(1,n))
df <- rbind(df3, df4)

# print(head(df))

png(file="simbudget%02d.png", width=width, height=height)

my.settings <- list(
  superpose.polygon=list(col=c("red","blue"), border="transparent"),
  # plot.polygon = list(col=c("red")),
  strip.background = list(col="lightyellow"),
  strip.border = list(col= "black")
  )
# print(trellis.par.get())
pp <- barchart(cashflow~tries, data=df, horiz=F,
               groups = wager,
               # main="Cash needed to play",
               xlab = "Try number",
               ylab = "Cash flow, $",
               box.ratio = 5,
               aspect = "fill",
               lty=0, stack=F, 
               origin=0, reference=T,
               par.settings = my.settings,
               # layout = c(1,2),
               auto.key = list(space = "top", columns=2,
                               title="Cash needed to play",
                               cex.title=1.1),
               col=c("red","blue"),
               ylim = c(-30000,30000),
               scales=list(
                 x=list(draw=T,
                        at=c(0,500,1000,1500,2000),
                        labels=c("0","500","1,000","1,500","2,000"),
                        tck=(1)
                        ),
                 y=list(draw=T,
                        at=c(-30000,-25000,-20000,-15000,-10000,-5000,0,5000,10000,15000,20000,25000,30000),
                        labels=c("-30,000","-25,000", "-20,000","-15,000", "-10,000", "-5,000", "0", "5,000", "10,000", "15,000", "20,000","25,000", "30,000"),
                        tck=(1))
               ),
               # panel.groups = function( x,y, group.number,...) {
               #   xt <- x[y==min(y)] # find latest year
               #   yt <- y[y==min(y)] # find value at latest year
               #   print(group.number)
               #   panel.text(xt, yt, labels=wagerLabels[group.number],
               #              pos=4,  # show labels on right side
               #              ...)
               #   panel.barchart(x,y,stack=F,lty = 0.1,col = ifelse(group.number==1 , "red", "blue"),border = F, box.width=0.5,...)
               #   # panel.grid(v=-1, h=-1, lty=10)
               # }
)

print(pp)
dev.off()