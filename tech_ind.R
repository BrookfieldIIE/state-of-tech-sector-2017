###########################
#Process the NAICS and NOC file to get to tech industries

library(data.table)
library(ggplot2)
library(extrafont)
library(BFTheme)
library(stringr)

source("NOC.R")

old.tech.noc <- c("0131","0211","0212","0213","2111","2112","2113","2114","2115","2121","2132","2133","2134",
                  "2142","2146","2147","2148","2161","2171","2172","2173","2174","2175","2211","2221","5223","5241")
#Get old tech noc in here for comparison reasons

load("TECH_IND/noc_naics_2016.RDA")
individual.ranking <- fread("tech.sector.def.csv")



noc_naics_2016 <- noc_naics_2016[GEO %in% unique(noc_naics_2016[,GEO])[1:14]] #Filter it out do only Ontario is taken - make sure to update this to Canada when relevant


noc_naics_2016[,tech:=0] #Set tech as 0
noc_naics_2016[NOC %in% individual.ranking[tech==1,str_sub(noc_title,1,4)],tech:=1] #Set tech dummy for occupation as 1

#These 4 lines calculate the % of tech occupations in a given industry
noc_naics_2016[,tot.occ:=sum(TOT),by=.(IND,GEO)] 
naics.noc.sum <- noc_naics_2016[tech==1,.(sum(TOT),unique(tot.occ)),by=.(IND,GEO)]
names(naics.noc.sum) <- c("industry","geo","tech","tot")
naics.noc.sum[,pct:=100*tech/tot]

#For old noc
naics.noc.old <- naics.noc[old.tech==1,.(sum(count),unique(tot)),by=industry]
names(naics.noc.old) <- c("industry","old.tech","tot")
naics.noc.old[,old.pct:=100*old.tech/tot]

#Set tech quotients
naics.noc.sum[,tech.dum:=0]
naics.noc.sum[pct>mean(pct)*3,tech.dum:=1]

#Set old noc tech quotients
naics.noc.old[,tech.dum:=0]
naics.noc.old[old.pct>mean(old.pct)*3,tech.dum:=1]



