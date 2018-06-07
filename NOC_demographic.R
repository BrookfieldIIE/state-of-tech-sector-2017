###################################
#This file will be used to prepare main NOC Demographic tables - set variable names and such
#The cleaned version will be called the following:
# - noc.classification.2006 - Demograhpic data for 2006 NOC and Census (Slightly different from 2016)
# -      NOC_Demographics/noc_2006_processed.RDA
# - noc.dem Main 2016 Noc demograhpics based on Age, Sex, and Labour Force by CMA as well as worker
# -      NOC_Demographics/noc_2016_processed.RDA
# - noc.dem Main 2016 Noc demograhpics based on Age, Sex, and Labour Force by PR - has income and immigraiton stuff as well
# -      NOC_Demographics/noc_2016_PR_processed.RDA
# - noc.demo.educ Main 2016 NOC demographics based on Age Sex Education CIP by
# -      NOC_Demographics/noc_2016_educ_processed.RDA
# - Run to prepare and clean big demographic datasets related to Occupations - Make sure all references to new column names (if any) are updated in other files
###################################
Sys.setlocale('LC_ALL','C') #Set encoding to deal with French characters

library(data.table)
library(BFTheme)
library(extrafont)
library(stringr)
library(plotly)

source("NOC.R") #Source to get the NOC occupations for tech purposes



######################
# Base stuff used to subset
tech.occ <- fread("tech.sector.def.csv") #Tech occupation in 2016
tech.occ <- tech.occ[tech==1] #Only selects for occupations with tech occupation = 1 for demographic purposes

noc.classification.2006 <- fread("NOC_Demographics/noc-s-cnp-s-2006-structure-eng.csv") #Loading classificaiton file for 2006
noc.classification.2006 <- noc.classification.2006[`Hierarchical structure`=="Unit group"] #Filter classification file for 2006
noc.classification.2006[,Code:=str_to_lower(Code)] #Get all the occupations to be lower for 2006
noc.classification.2006[,`Class title`:=str_to_lower(`Class title`)] #Change the class title to lwoercase for matching with 2006 Census Data







#####################
#Main 2016 Dataset to use - CMA/CA Level
load("NOC_Demographics/NOC_demo.RDA") #Load in noc demographic file for 2016 census
names(noc.dem) <- c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR","DATA.QUAL.FLAG","ALT.GEO.CODE","LF.STATUS","LF.STATUS.ID",
                    "LF.STATUS.NOTE","AGE","AGE.ID","AGE.NOTE","SEX","SEX.ID","SEX.NOTE","NOC","NOC.ID","NOC.NOTE","TOT","WORKER.NA","TOT.WORKER",
                    "TOT.EMP","TOT.SLF.EMP") #Change the names of the columns

noc.dem <- noc.dem[TOT>0] #Remove everything that has 0 count for the total number
noc.dem[,NOC.NUM:=tstrsplit(NOC," ",keep=1)] #Extract the NOC codes
noc.dem <- noc.dem[nchar(NOC.NUM)==4] #Filter out everything but 4-level NOCs
noc.dem[,NOC.NUM:=NULL] #Set NOC to NULL - Saves a bit more space
noc.dem[,c("GNR","DATA.QUAL.FLAG","LF.STATUS.NOTE","AGE.NOTE","SEX.NOTE","NOC.ID","NOC.NOTE"):=NULL] #Delete All irrelevant columns




#At this point, should have 2,293,895 rows. Check if that's not the case

noc.dem[,tech:=0] #Set the tech flag vs non tech flag
noc.dem[NOC %in% tech.occ[,noc_title],tech:=1] #Set the tech flag
noc.dem[,digital:=0] #Set digital flag 
noc.dem[NOC %in% tech.occ[digital=="Digital",noc_title],digital:=1] #Set digital flag 
noc.dem[,high.tech:=0] #Set High-tech
noc.dem[NOC %in% tech.occ[digital=="High-Tech"],high.tech:=1] #Set High-tech


save(noc.dem,file="NOC_Demographics/noc_2016_processed.RDA") #This Saves the processed file

rm(noc.dem) #Remove the file for regular processing

#####################
# Main 2016 Dataset to use - PR Level
load("NOC_Demographics/noc_2016_PR.RDA")
names(noc.dem.master) <- c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR","DATA.QUAL.FLAG","ALT.GEO.CODE","IM.STATUS","IM.STATUS.ID",
                           "IM.STATUS.NOTE","EDUC","EDUC.ID","EDUC.NOTE","WA","WA.ID","WA.NOTE","AGE5","AGE5.ID","AGE5.NOTE",
                           "SEX3","SEX3.ID","SEX3.NOTE","NOC691","NOC691.ID","NOC691.NOTES","TOT","MED.INC","AVG.INC")
noc.dem.master <- noc.dem.master[TOT>0]
noc.dem.master[,NOC.NUM:=tstrsplit(NOC691," ",keep=1)]
noc.dem.master <- noc.dem.master[nchar(NOC.NUM)==4]
noc.dem.master[,NOC.NUM:=NULL]
noc.dem.master[,c("GNR","DATA.QUAL.FLAG","IM.STATUS.NOTE","EDUC.NOTE","WA.NOTE","AGE5.NOTE","SEX3.NOTE","NOC691.NOTES"):=NULL]
noc.dem.master[,tech:=0]
noc.dem.master[NOC691 %in% tech.occ[,noc_title],tech:=1]
noc.dem.master[,digital:=0] #Set digital flag 
noc.dem.master[NOC691 %in% tech.occ[digital=="Digital",noc_title],digital:=1] #Set digital flag 
noc.dem.master[,high.tech:=0] #Set High-tech
noc.dem.master[NOC691 %in% tech.occ[digital=="High-Tech"],high.tech:=1] #Set High-tech

save(noc.dem.master, file="NOC_Demographics/noc_2016_PR_processed.RDA")

rm(noc.dem.master)

################################
#Process 2006 Census Data

load("NOC_Demographics/noc_2006.RDA") #Loading main demograhpic file



names(noc.2006) <- c("Geo","Occ","Dem",'TOT.SEX',"TOT.MALE","TOT.FEMALE") #Rename columns for NOC
noc.2006[,Occ:=str_trim(Occ)] #Trim all the preceding white spaces from the Occupation column
noc.2006[,Occ:=str_to_lower(Occ)]
noc.2006[,Occ.Num:=tstrsplit(Occ," ",keep=1)]
noc.2006 <- noc.2006[nchar(Occ.Num)==4]
noc.2006[,tech:="Non Tech Occupation"] #Set base for non tech occupations
noc.2006[Occ %in% noc.classification.2006[Tech==1,str_c(Code,`Class title`,sep=" ")],tech:="Tech Occupation"] #Select only the tech occupations
noc.2006[,GEO.NAME:=tstrsplit(Geo,"\\(",keep=1)]
noc.2006[,GEO.CODE:=tstrsplit(Geo,"\\(",keep=2)]
noc.2006[,GEO.CODE:=tstrsplit(GEO.CODE,"\\)",keep=1)]
noc.2006 <- noc.2006[nchar(GEO.CODE)<=5]


save(noc.2006,file="NOC_Demographics/noc_2006_processed.RDA")

rm(noc.2006)

##################
#Education stuff
load("NOC_Demographics/noc_demo_educ.RDA") #Load in education stuff
names(noc.demo.educ) <- c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR",
                          "DATA.QUALITY.FLAG","ALT.GEO.CODE","CIP82","CIP82.ID",
                          "CIP82.NOTES","AGE9","AGE9.ID","AGE9.NOTE","SEX3","SEX3.ID",
                          "SEX3.NOTES","NOC","NOC.ID","NOC.NOTES","TOT.EDUC",
                          "TOT.NO","TOT.SEC","TOT.APP","TOT.TRA","TOT.CERAPP","TOT.COL",
                          "TOT.BEBA","TOT.ATABBA","TOT.BA","TOT.ABBA")

noc.demo.educ <- noc.demo.educ[AGE9.ID==1] #Filter out age for now
noc.demo.educ[,NOC.ID:=tstrsplit(NOC," ",keep=1)] #Split NOC codes
noc.demo.educ <- noc.demo.educ[nchar(NOC.ID)==4] #Only keep 4 code stuff
noc.demo.educ[,tech:="Not Tech Occupation"] #Set tech dum to be 0
noc.demo.educ[NOC %in% tech.occ[,noc_title], tech:="Tech Occupation"] #Set tech dum to be 1 for tech occupations
noc.dem.educ[NOC %in% tech.occ[,noc_title],tech:=1]
noc.dem.educ[,digital:=0] #Set digital flag 
noc.dem.educ[NOC %in% tech.occ[digital=="Digital",noc_title],digital:=1] #Set digital flag 
noc.dem.educ[,high.tech:=0] #Set High-tech
noc.dem.educ[NOC %in% tech.occ[digital=="High-Tech"],high.tech:=1] #Set High-tech

save(noc.demo.educ,file="NOC_Demographics/noc_2016_educ_processed.RDA")

rm(noc.demo.educ)




###################
#Aboriginal Identity data - table 98-400-X2016357_English_CSV_data.csv
load("NOC_Demographics/noc_abo.RDA")
names(noc.abo) <- c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR",
                    "DATA.QUALITY.FLAG","ALT.GEO.CODE","ABO","ABO.ID","ABO.NOTE",
                    "EDUC","EDUC.ID","EDUC.NOTE","WA","WA.ID","WA.NOTE","AGE4","AGE4.ID",
                    "AGE4.NOTE","SEX3","SEX3.ID","SEX3.NOTE","OCC691","OCC691.ID","OCC691.NOTE",
                    "TOT","MED.INC","AVG.INC")

noc.abo[,c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR","DATA.QUALITY.FLAG","ALT.GEO.CODE","ABO.NOTE","EDUC.NOTE",
           "WA.NOTE","AGE4.NOTE","SEX3.NOTE","OCC691.NOTE"):=NULL]
noc.abo <- noc.abo[TOT>0]
noc.abo[,NOC:=tstrsplit(OCC691," ",keep=1)]
noc.abo <- noc.abo[nchar(NOC)==4]
noc.abo[,tech:="Not Tech Occupation"]
noc.abo[OCC691 %in% tech.occ[,noc_title], tech:="Tech occupation"]
noc.abo[OCC691 %in% tech.occ[,noc_title],tech:=1]
noc.abo[,digital:=0] #Set digital flag 
noc.abo[OCC691 %in% tech.occ[digital=="Digital",noc_title],digital:=1] #Set digital flag 
noc.abo[,high.tech:=0] #Set High-tech
noc.abo[OCC691 %in% tech.occ[digital=="High-Tech"],high.tech:=1] #Set High-tech

save(noc.abo,file="NOC_Demographics/noc_abo_processed.RDA")

rm(noc.abo)


################
#CIP very low level 98-400-X2016258_English_CSV_data
load("NOC_Demographics/noc_cip_2016.RDA")
names(noc.cip.2016) <- c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR","DATA.QUALITY.FLAG","ALT.GEO.CODE","CIP432","CIP432.ID","CIP432.NOTES",
                         "EDUC","EDUC.ID","EDUC.NOTES","AGE4","AGE4.ID","AGE4.NOTES","OCC693","OCC693.ID","OCC693.NOTES","TOT","TOT.MALE","TOT.FEMALE")

noc.cip.2016 <- noc.cip.2016[TOT>0]
noc.cip.2016[,noc:=tstrsplit(OCC693," ",keep=1)]
noc.cip.2016 <- noc.cip.2016[nchar(noc)==4]
noc.cip.2016[,cip:=tstrsplit(CIP432," ",keep=1)]
noc.cip.2016 <- noc.cip.2016[nchar(cip)==5 & cip != "Other" & cip != "Total"]
noc.cip.2016[,tech:="Not Tech Occupation"]
noc.cip.2016[OCC693 %in% tech.occ[,noc_title], tech:="Tech occupation"]
noc.cip.2016[OCC693 %in% tech.occ[,noc_title],tech:=1]
noc.cip.2016[,digital:=0] #Set digital flag 
noc.cip.2016[OCC693 %in% tech.occ[digital=="Digital",noc_title],digital:=1] #Set digital flag 
noc.cip.2016[,high.tech:=0] #Set High-tech
noc.cip.2016[OCC693 %in% tech.occ[digital=="High-Tech"],high.tech:=1] #Set High-tech
noc.cip.2016[,c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR","DATA.QUALITY.FLAG","ALT.GEO.CODE","CIP432.NOTES","EDUC.NOTES","AGE4.NOTES",
                "OCC693.NOTES"):=NULL]


save(noc.cip.2016,file="NOC_Demographics/noc_cip_2016_processed.RDA")
rm(noc.cip.2016)

