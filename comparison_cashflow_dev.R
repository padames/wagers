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
pp <- barchart(cashflow~tries | wager, data=df, horiz=F,
               col = sapply(df$wager, function(x){ifelse( x == "wager 3", "red", "blue")}), 
               lty=0, stack=F, 
               origin=0, reference=T,
               # auto.key = list(space = "right"),
               # scales=list(x=list(draw=T, tck=(1))

)

print(pp)
dev.off()