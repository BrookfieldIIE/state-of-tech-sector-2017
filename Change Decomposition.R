
library(data.table)
library(ggplot2)
library(extrafont)
library(stringr)
library(BFTheme)

#################################
#READ FIRST
#Main file in processing data for Decomposition of Change in Demographics
#This file uses the main data generated in NOC_demographic.R
#Run it only in initial runs as it takes a while to clean those massive data
#  source("NOC_demographic.R")

load("NOC_demographics/noc_2006_processed.RDA")
load("NOC_demographics/noc_2016_processed.RDA")

#2006 Census disaggregation
noc.2006.age <- noc.2006[Dem %in% unique(noc.2006[,Dem])[1:10]] #Subsetting for Age variable


noc.2006.age <- noc.2006.age[Geo=="Canada (01)   20000"] #Filter to get only Canada
noc.2006.age[,noc:=tstrsplit(Occ," ",keep=1)] #Get the noc code
noc.2006.age <- noc.2006.age[nchar(noc)==4] #Filter out the non 4 level stuff

noc.2006.age <- noc.2006.age[,.(sum(TOT.SEX),sum(TOT.MALE),sum(TOT.FEMALE)),by=.(tech,Dem)] #Get the sum by demographic group
names(noc.2006.age) <- c("tech","AGE","TOT","Male","Female") #Change name
noc.2006.age <- noc.2006.age[AGE!= "15 to 19 years"
                             & AGE != "20 to 24 years" 
                             & AGE != "75 years and over"] #Filter out 5 years intervals

noc.2006.age[,TOT:=NULL]
noc.2006.age <- noc.2006.age[AGE != "Total labour force by age groups"] #Remove the total by labour force
noc.2006.age <- melt(noc.2006.age,id.vars = c("tech","AGE"),value.name = "tech.total.2006",variable.name = "SEX") #Melt data into wide format
noc.2006.age[,pop.total.2006:=sum(tech.total.2006),by=.(AGE,SEX)] #Calculate total population in specific age cell regardless of in tech or not
noc.2006.age <- noc.2006.age[tech == "Tech Occupation"] #Leave only the tech numbers now for percentage calculation
noc.2006.age[,pct.pop.2006:=pop.total.2006/sum(pop.total.2006)] #Add population numbers and divide it by the total number of population in the groups considered
noc.2006.age[,pct.tech.2006:=tech.total.2006/pop.total.2006] #Find tech percentage (propensity) by dividing tech workers by total number of people in that demographic cell
noc.2006.age[,tech:=NULL]


#2016 Census Data for henry disaggregation

age.10 <- c(2,6,9,10,11,12) #Define the age category - useful in subsetting by ID for the 2016 Census. Not useful for 2006 Census given the table structure.

noc.2016.age <- noc.dem[GEO.NAME=="Canada" & LF.STATUS.ID == 1]


noc.2016.age[,c("TOT.SLF.EMP","TOT.EMP","TOT.WORKER","WORKER.NA"):=NULL] #Remove the non worker totals - only interested in people who are workers
noc.2016.age <- noc.2016.age[,sum(TOT),by=.(tech,AGE,SEX,AGE.ID)]
noc.2016.age <- noc.2016.age[SEX != "Total - Sex"]
noc.2016.age <- noc.2016.age[AGE.ID %in% age.10]
names(noc.2016.age) <- c("tech","AGE","SEX","AGE.ID","tech.total.2016")

noc.2016.age[,pop.total.2016:=sum(tech.total.2016),by=.(AGE,SEX)] #Find total workers by demographic group regardless of tech

noc.2016.age <- noc.2016.age[tech==1] #Only filter out for tech numbers now for percentage calculation
noc.2016.age[,pct.pop.2016:=pop.total.2016/sum(pop.total.2016)] #Get percentage of population in that demogrpahic cell
noc.2016.age[,pct.tech.2016:=tech.total.2016/pop.total.2016] #Get percentage of tech workers in that demographic cell
noc.2016.age[,tech:=NULL]
noc.2016.age[,AGE.ID:=NULL]


setkey(noc.2006.age,AGE,SEX)
setkey(noc.2016.age,AGE,SEX)

#Actual decomposition stuff
noc.dec.merge <- noc.2006.age[noc.2016.age]

propensity.effect <- noc.dec.merge[,sum((pct.tech.2016-pct.tech.2006)*pct.pop.2006)] #Propensity effect
composition.effect <- noc.dec.merge[,sum((pct.pop.2016-pct.pop.2006)*pct.tech.2006)] #Composition effect
interaction.effect <- noc.dec.merge[,sum((pct.pop.2016-pct.pop.2006)*(pct.tech.2016-pct.tech.2006))] #Interaction effect

total.effect <- composition.effect + interaction.effect + propensity.effect

noc.dec.merge[,share.effect:=100*((pct.pop.2016*pct.tech.2016)-(pct.pop.2006*pct.tech.2006))/total.effect]



plot.scatter.bf(noc.dec.merge.2,"pct.tech.2006","pct.tech.2016",deg.45=TRUE,unit.x="%",unit.y="%",trend.line=TRUE)









