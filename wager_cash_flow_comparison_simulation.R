#!/usr/bin/env Rscript
# Run from command line with RScript NAME_OF_FILE <ARGUMENT1,...>

width <- 1200
height <- 800

args = commandArgs(trailingOnly=TRUE)

if (length(args)>1) {
  width = as.numeric(args[1])
  height = as.numeric(args[2])
} else {
  cat("If both width and height aren't provided I will use defaults.\n")
}
cat(paste0("Creating images of width=", width, " by height=", height,".\n"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load("here")

suppressWarnings(my_proj_path <- here())

if (!file.exists(file.path(my_proj_path,"animations"))) {
  dir.create(file.path(my_proj_path, "animations"))
}
setwd(file.path(my_proj_path, "animations"))

library(lattice)
library(tigerstats)

my.settings <- list(
  superpose.polygon=list(col=c("red","blue"), border="transparent")
)


n = 2000

png(file="simbudget%02d.png", width=width, height=height)

wagerLabels = c("Wager3 = 300(die>3) - 300(die<=3)", "Wager 4 = 150(die>2) - 300(die<=2)")

outsp = seq(1,6)

for (i in 1:60) {
  
  die3 = sample(outsp,n,replace=T)
  die4 = sample(outsp,n,replace=T)
  cashflow3 = -300 + 600*(die3>3)
  cashflow4 = -300 + 450*(die4>2)
  

  cash3 = sapply(seq_along(1:n), 
                 function(i){ ifelse(i==1, -300, 
                                     sum(cashflow3[1:(i-1)])+(300-600*(die3[i-1]>3)))})
  cash4 = sapply(seq_along(1:n), 
                 function(i){ ifelse(i==1, -300, 
                                     sum(cashflow4[1:(i-1)])+(300-450*(die4[i-1]>2)))})
  
  df3 <- data.frame(wager=rep(wagerLabels[1],length(cash3)),
                    cashflow=cash3,
                    tries=seq(1,n))
  df4 <- data.frame(wager=rep(wagerLabels[2],length(cash4)),
                    cashflow=cash4,
                    tries=seq(1,n))
  df <- rbind(df3, df4)

  pp <- barchart(cashflow~tries, data=df, horiz=F,
                 groups = wager,
                 box.ratio = 5,
                 aspect = "fill",
                 lty=0, stack=F, 
                 origin=0, reference=T,
                 xlab = "Try number",
                 ylab = "Cash flow, $",
                 par.settings = my.settings,
                 auto.key = list(space = "top", columns=2,
                                 title="60 different cash flow histories for 2,000 plays of two different wagers",
                                 cex.title=1.1),
                 col=c("red","blue"),
                 ylim = c(-35000,35000),
                 scales=list(
                   x=list(draw=T,
                          at=c(0,500,1000,1500,2000),
                          labels=c("0","500","1,000","1,500","2,000"),
                          tck=(1)
                   ),
                   y=list(draw=T,
                          at=c(-35000,-30000,-25000,-20000,-15000,-10000,-5000,0,5000,10000,15000,20000,25000,30000,35000),
                          labels=c("-35,000","-30,000","-25,000", "-20,000","-15,000", "-10,000", "-5,000", "0", "5,000", "10,000", "15,000", "20,000","25,000", "30,000","35,000"),
                          tck=(1))
                 )
  )
  print(pp)  
}

dev.off()

# This section is machine dependent. 
# It assumes we are in an Ubuntu installation with the program convert from ImageMagick

# converting .png file in .gif using ImageMagick
system("/usr/bin/convert -delay 40 *.png wager_cash_flow_comparison.gif")

# Remove .png file
file.remove(list.files(pattern=".png"))