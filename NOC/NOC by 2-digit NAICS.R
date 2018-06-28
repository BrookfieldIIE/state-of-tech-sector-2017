############# NOCs by 2-digit NAICS

library(tidyr)
library(tidyverse)
library(BFTheme)
library(skimr)
setwd("~/GitHub/state-of-tech-sector-2017/")

source("NOC.R")
tech.occ.def <- read_csv("tech.sector.def.csv")

tech.occ.def <- tech.occ.def %>%
  select(noc_title, digital) %>%
  filter(!is.na(digital)) #%>%
  mutate(noc_code = substr(noc_title, 1,4))

setwd("~/GitHub/state-of-tech-sector-2017/NOC")
download.file("http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/CompDataDownload.cfm?LANG=E&PID=111858&OFT=CSV","NOCx2NAICS.zip")
unzip("98-400-X2016298_ENG_CSV.ZIP")

noc.naics <- read_csv("98-400-X2016298_English_CSV_data.csv", n_max = 239085)
names(noc.naics) <- c(census.2016.varnames[names(noc.naics)[1:19]], "TOT", "Employed", "Unemployed")

noc.naics <- noc.naics %>%
  #filter(NOC693A %in% tech.occ.def$noc_title, AGE5 == "Total - Age", SEX3 == "Total - Sex") %>%
  filter(AGE5 == "Total - Age", SEX3 == "Total - Sex") %>%
  select(NAICS23A, NOC693A, TOT, Employed, Unemployed) %>%
  left_join(tech.occ.def, by = c("NOC693A" = "noc_title" ))

write.csv(noc.naics,"noc.naics.csv")
  

