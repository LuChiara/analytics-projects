## Read data from CSV file
DATA <- read.csv("multipleitempart1.csv")

## Adding week factor indicator 
DATA$Week_factor <- as.factor(rep(rep(seq(1,13),each=4),3))
