library(tigerstats)

n = 2000
wagerLabels = c("Wager 3", "Wager 4")
outsp = seq(1,6)
die3 = sample(outsp,n,replace=T)
die4 = sample(outsp,n,replace=T)
cashflow3 = 300*(die3>3) - 300*(die3<=3)
cashflow4 = 150*(die4>2) - 300*(die4<=2)

cash3 = cumsum(cashflow3)
cash4 = cumsum(cashflow4)

# put together a data frame in long form
df3 <- data.frame(wager=rep("wager 3",length(cash3)),cashflow=cash3,tries=seq(1,n))
df4 <- data.frame(wager=rep("wager 4",length(cash4)),cashflow=cash4,tries=seq(1,n))
df <- rbind(df3, df4)

png(file="simbudget%02d.png", width=1200, height=800)
colf <- function(w) { ifelse(test = w == "wager 3", 
                             yes = "red", no = "blue")}
my.settings <- list(
  # superpose.polygon = sapply(df$wager, function(x){ifelse( x == "wager 3", "red", "blue")}), 
  strip.background = list(col="lightyellow"),
  strip.border = list(col= "black")
  )
# print(trellis.par.get())
pp <- barchart(cashflow~tries | wager, data=df, horiz=F,
               groups = wager,
               box.ratio = 100,
               lty=0.1, stack=F, 
               origin=0, reference=T,
               par.settings = my.settings,
               # auto.key = list(space = "right"),
               ylim = c(-40000,40000),
               scales=list(
                 x=list(draw=T,
                        at=c(0,500,1000,1500,2000),
                        labels=c("0","500","1,000","1,500","2,000"),
                        tck=(1)
                        ),
                 y=list(draw=T,
                        at=c(-40000,-30000,-20000,-10000,-5000,0,5000,10000,20000,30000,40000),
                        labels=c("-40,000", "-30,000","-20,000", "-10,000", "-5,000", "0", "5,000", "10,000", "20,000", "30,000","40,000"),
                        tck=(1))
               ),
               panel.groups = function( x,y, group.number,...) {
                 xt <- x[y==min(y)] # find latest year
                 yt <- y[y==min(y)] # find value at latest year
                 panel.text(xt, yt, labels=wagerLabels[group.number], 
                            pos=4,  # show labels on right side
                            ...)
                 panel.barchart(x,y,stack=F,lty = 0.1,col = ifelse(group.number==1 , "red", "blue"),border = T, box.width=0.5,...)
                 # panel.grid(v=-1, h=-1, lty=10)
               }
)

print(pp)
dev.off()