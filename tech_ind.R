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

naics.noc <- fread("TECH_IND/full_file.csv") #Read the main NAICS NOC file
individual.ranking <- fread("tech.sector.def.csv")

naics.noc <- melt(naics.noc,id.vars=c("Industry","Geography")) #Melt it to long form with industry and geography

names(naics.noc) <- c("industry","geography","occupation","count") #Change names to make it easier to deal with

naics.noc[,occupation:=as.character(occupation)] #Make occupations into characters

naics.noc <- naics.noc[count > 0] #Remove anything that has count of 0 - make it easy on the RAM :)

naics.noc[,occupation:=str_trim(occupation)] #Trim all the white spaces created due to B20/20 export

naics.noc[,industry:=str_trim(industry)] #Likewise, trim all the white spaces created due to B20/20 export
naics.noc[,noc:=tstrsplit(occupation," ",keep=1)] #Extract the NOC codes (and a few other things)
naics.noc[,naics:=tstrsplit(industry," ",keep=1)] #Extract the NAICS codes (and a few other things)

naics.noc <- naics.noc[geography== "Ontario - Total  (4.7%)"] #Filter it out do only Ontario is taken - make sure to update this to Canada when relevant
naics.noc <- naics.noc[nchar(naics)==4] #Filter out and only retain 4 NOC codes
naics.noc <- naics.noc[nchar(noc)==4] #Filter out and only retain 4 NAICS codes
naics.noc[,geography:=NULL] #Set geography to null since we only care about Ontario right now 


naics.noc[,tech:=0] #Set tech as 0
naics.noc[occupation %in% individual.ranking[tech==1,noc_title],tech:=1] #Set tech dummy for occupation as 1

#These 4 lines calculate the % of tech occupations in a given industry
naics.noc[,tot:=sum(count),by=naics] 
naics.noc.sum <- naics.noc[tech==1,.(sum(count),unique(tot)),by=industry]
names(naics.noc.sum) <- c("industry","tech","tot")
naics.noc.sum[,pct:=100*tech/tot]

#Set tech quotients
naics.noc.sum[,tech.dum:=0]
naics.noc.sum[pct>15,tech.dum:=1]





