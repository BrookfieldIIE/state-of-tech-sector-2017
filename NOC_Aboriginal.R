#########################
#Main file for processing data relating to Aboriginal population

library(data.table)
library(extrafont)
library(stringr)
library(SDMTools)
library(BFTheme)
library(ggplot2)


load("NOC_Demographics/noc_abo_processed.RDA") #Load the data


noc.abo.sumstat <- noc.abo[EDUC.ID == 1 & WA.ID == 1 & AGE4.ID == 3,.(sum(TOT),wt.mean(AVG.INC,TOT)),by=.(ABO,ABO.ID,SEX3,SEX3.ID,tech)]