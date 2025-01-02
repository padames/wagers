if (!require("pacman")) {
    install.packages("pacman")
}
library("pacman")

# load libraries 
pacman::p_load(char = c("here", "lattice", "tigerstats"), 
               install = TRUE)

suppressWarnings(my_proj_path <- here())

source(here::here("create_xyplot.R"))

# prepare the animations folder and switch to it 

if (!file.exists(file.path(my_proj_path,"animations"))) {
    dir.create(file.path(my_proj_path, "animations"))
}

setwd(file.path(my_proj_path, "animations"))

# prepare to run the simulations, the plots and the png files for each frame to 
# be included in the animation

numsimulations = 20000
numframes = 30

set.seed(1892)
dievalues = seq(1,6)

# set the output device for subsequent 'print' commands as a series of files
# in PNG format with a same including a sequential 2-digit number starting with 00
png(file="simwager%02d.png", width=600, height=400)

for (i in 1:numframes) {
    die3 = sample(dievalues, numsimulations, replace = T)
    die4 = sample(dievalues, numsimulations, replace = T)
    
    profit3 = 300 * (die3 > 3) - 300 * (die3 <= 3)
    profit4 = 150 * (die4 > 2) - 300 * (die4 <= 2)
    
    runningprofit3 = cumsum(profit3) / seq(1, numsimulations)
    runningprofit4 = cumsum(profit4) / seq(1, numsimulations)
    
    df3 <- data.frame( wager = rep(3, length(runningprofit3)),
                       runningprofit = runningprofit3,
                       tries = seq(1, numsimulations))
    
    df4 <- data.frame( wager = rep(4, length(runningprofit4)),
                       runningprofit = runningprofit4,
                       tries = seq(1, numsimulations))
    
    # stack these two data frames into a single one:
    df <- rbind(df3, df4)
    
    # prepare arguments to the plot function
    args_to_plot <- list( subgroup_variable = 'wager',
                          subgroup_descriptions = c("Wager3 = 3(die>3) - 3(die<=3)",
                                                    "Wager 4 = 1.5(die>2) - 3(die<=2)"),
                          text_labels_for_plot = list(title="Running profits from two different wagers",
                                                      x_label="Number of tries",
                                                      y_label="Cummulative profit"),
                          min_max_y_to_plot = c(-350, 350))
    
    pp <- create_xyplot(df, 
                        args_to_plot)
    # each frame gets created as one PNG file by the following command:
    print(pp)
}

# disconnect the print action from the PNG device so future 'print' won't create PNG files
dev.off()

# This section is machine dependent. 
# It assumes we are in a Ubuntu/Linux installation and the program convert from
# the package ImageMagick has been installed in the machine before hand.

# The delay argument is 60 centiseconds, which means there is a 0.4 second wait 
# before the new image is added to the animation gif file. 
# This gives approximate 1 frame per second
system("/usr/bin/convert -delay 60 *.png -write wager_comp_sim.gif")

# Remove all 'png' files as they are not needed anymore 
file.remove(list.files(path = here::here("animations"), 
                       pattern=".png"))
