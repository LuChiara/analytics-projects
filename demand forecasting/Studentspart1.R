## Import data from CSV file, inside "" is the location of your CSV file in your computer. 
Data <- read.csv("singleitemSKU88.csv")

##Add seasonal factor to the data 
Data$Week_factor <-  as.factor(rep(rep(seq(1,13),each=4),3))



