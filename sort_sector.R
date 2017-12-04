########################################
# Finding Tech Occupataion Proportion  #
# Using NOC by NAICS to find proportion#
# Of tech employees                    #
########################################

library(data.table)
library(stringr)

naics_by_noc <- fread("naics_by_noc.csv")
#Need to have a NOC dictionary that is "tech"
noc_in_tech <- c("0131","0211","0212","0213","2111","2112","2113","2114","2115",
                 "2121","2132","2133","2134","2142","2146","2147","2148","2161",
                 "2171","2172","2173","2174","2175","2211","2221","5223","5241")


#noc_in_tech - vector of noc code (in character) that is in tech sector. 4 codes

naics_by_noc <- melt(naics_by_noc,id.vars="Occupation(691)")

names(naics_by_noc) <- c("noc_full", "naics_full", "amount")
naics_by_noc[,noc_code:=tstrsplit(noc_full," ", keep = 1)]
naics_by_noc[,naics_code:=tstrsplit(naics_full," ", keep = 1)]
naics_by_noc[nchar(naics_code)==4, naics_4_code:=naics_code]
naics_by_noc[nchar(noc_code)==4,noc_4_code:=noc_code]

naics_by_noc[,employment_share:=amount/max(amount),by=naics_code]
tech_share_by_naics <- naics_by_noc[noc_code %in% noc_in_tech,sum(employment_share),by=.(naics_code,naics_full)]
tech_share_by_naics <- tech_share_by_naics[nchar(naics_code)==4]
names(tech_share_by_naics) <- c("naics_code","naics_full","tech_share")

threshold <- mean(tech_share_by_naics[,tech_share])*3

tech_industry <- tech_share_by_naics[tech_share >= threshold]