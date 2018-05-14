###################################
#This file will be used to prepare main NOC Demographic tables - set variable names and such
#The cleaned version will be called the following:
# - noc.classification.2006 - Demograhpic data for 2006 NOC and Census (Slightly different from 2016)
# -      NOC_Demographics/noc_2006_processed.RDA
# - noc.dem Main 2016 Noc demograhpics based on Age, Sex, and Labour Force by CMA as well as worker
# -      NOC_Demographics/noc_2016_processed.RDA
# - noc.demo.educ Main 2016 NOC demographics based on Age Sex Education CIP by
# -      NOC_Demographics/noc_2016_educ_processed.RDA
# - Run to prepare and clean big demographic datasets related to Occupations - Make sure all references to new column names (if any) are updated in other files
###################################


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
#Main 2016 Dataset to use
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


save(noc.dem,file="NOC_Demographics/noc_2016_processed.RDA") #This Saves the processed file

rm(noc.dem) #Remove the file for regular processing

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

save(noc.demo.educ,file="NOC_Demographics/noc_2016_educ_processed.RDA")

rm(noc.demo.educ)
