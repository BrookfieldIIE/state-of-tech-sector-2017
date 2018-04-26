###################################
#This file will be used to analyze NOC Demographics
###################################

library(data.table)
library(BFTheme)
library(extrafont)
library(stringr)

source("NOC.R") #Source to get the NOC occupations for tech purposes


tech.occ <- fread("tech.sector.def.csv")
tech.occ <- tech.occ[tech==1] #Only selects for occupations with tech occupation = 1 for demographic purposes

noc.dem <- fread("NOC_Demographics/NOC_demo.csv") #Load in noc demographic file
names(noc.dem) <- c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR","DATA.QUAL.FLAG","ALT.GEO.CODE","LF.STATUS","LF.STATUS.ID",
                    "LF.STATUS.NOTE","AGE","AGE.ID","AGE.NOTE","SEX","SEX.ID","SEX.NOTE","NOC","NOC.ID","NOC.NOTE","TOT","WORKER.NA","TOT.WORKER",
                    "TOT.EMP","TOT.SLF.EMP") #Change the names of the columns

noc.dem <- noc.dem[TOT>0] #Remove everything that has 0 count for the total number
noc.dem[,NOC.NUM:=tstrsplit(NOC," ",keep=1)] #Extract the NOC codes
noc.dem <- noc.dem[nchar(NOC.NUM)==4] #Filter for everything but 4-level NOCs
noc.dem[,NOC.NUM:=NULL] #Set NOC to NULL
#At this point, should have 2,293,895 rows. Check if that's not the case

age.10 <- c(2,6,9,10,11,12,13) #Define the age category

noc.dem[,tech:=0] #Set the tech flag vs non tech flag
noc.dem[NOC %in% tech.occ[,noc_title],tech:=1] #Set the tech flag


#Add up by tech worker vs non tech worker
noc.dem.tech.map <- noc.dem[GEO.LEVEL == "2" & 
                              LF.STATUS.ID==1 & 
                              AGE.ID==1 & 
                              SEX.ID==1 ,
                            sum(TOT) , by=.(tech,ALT.GEO.CODE,GEO.NAME)] 
noc.dem.tech.map[,tot:=sum(V1),by=ALT.GEO.CODE] #Add the sum for each geographic area
noc.dem.tech.map <- noc.dem.tech.map[tech==1] #Filter out so only retain one set
noc.dem.tech.map[,pct:=V1/tot*100] #Calculate % concentration by geography
noc.dem.tech.map[,c("tech","GEO.NAME","V1","tot"):=NULL] #Remove all the redundant columns

fig.noc.tech.map <- plot.map.cma.bf(value.data=noc.dem.tech.map,
                                    plot.title = "Geographic Concentration of Technology Occupations Across Canada",
                                    plot.fig.num = "Figure x",
                                    legend.title = "Percentage Concentration of Tech Occupations",
                                    caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.bc <- plot.map.cma.bf(province.name = c("BC"),
                                       value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("59")],
                                       plot.title = "Geographic Concentration of Technology Occupations Across British Columbia",
                                       plot.fig.num = "Figure x",
                                       legend.title = "Percentage Concentration of Tech Occupations",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.prairie <- plot.map.cma.bf(province.name = c("AB","SK","MB"),
                                       value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("46","47","48")],
                                       plot.title = "Geographic Concentration of Technology Occupations Across the Prairies",
                                       plot.fig.num = "Figure x",
                                       legend.title = "Percentage Concentration of Tech Occupations",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")



fig.noc.tech.map.on <- plot.map.cma.bf(province.name = c("ON"),
                                       value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) =="35"],
                                       plot.title = "Geographic Concentration of Technology Occupations Across Ontario",
                                       plot.fig.num = "Figure x",
                                       legend.title = "Percentage Concentration of Tech Occupations",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.qc <- plot.map.cma.bf(province.name = c("QC"),
                                       value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) =="24"],
                                       plot.title = "Geographic Concentration of Technology Occupations Across Quebec",
                                       plot.fig.num = "Figure x",
                                       legend.title = "Percentage Concentration of Tech Occupations",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.atlantic <- plot.map.cma.bf(province.name = c("NL","PE","NS","NB"),
                                             value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("10","11","12","13")],
                                             plot.title = "Geographic Concentration of Technology Occupations Across the Atlantic",
                                             plot.fig.num = "Figure x",
                                             legend.title = "Percentage Concentration of Tech Occupations",
                                             caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.territories <- plot.map.cma.bf(province.name=c("YT","NT","NU"),
                                                value.data = noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("60","61","62")],
                                                plot.title = "Geographic Concentration of Technology Occupations Across the Territories",
                                                plot.fig.num = "Figure x",
                                                legend.title = "Percentage Concentration of Tech Occupations",
                                                caption = "Source: 2016 Canadian Census, BII+E Analysis")


