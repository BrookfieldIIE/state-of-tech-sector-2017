############################
#R&D Data
#Version: 1.0
############################

#Load standard packages
library(data.table)
library(stringr)

#Load data files
inhouse <- fread("in_house_research.csv")
outsource <- fread("outsourced_research.csv")