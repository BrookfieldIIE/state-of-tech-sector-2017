############# NOCs by 2-digit NAICS

library(tidyr)
library(tidyverse)
library(BFTheme)
library(skimr)
library(data.table)

source("NOC.R")
tech.occ.def <- read_csv("tech.sector.def.csv")

tech.occ.def <- tech.occ.def %>%
  select(noc_title, digital) %>%
  filter(!is.na(digital)) %>%
  mutate(noc_code = substr(noc_title, 1,4))

# setwd("~/GitHub/state-of-tech-sector-2017/NOC")
# download.file("http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/CompDataDownload.cfm?LANG=E&PID=111858&OFT=CSV","NOCx2NAICS.zip")
# unzip("98-400-X2016298_ENG_CSV.ZIP")

  
## Download isn't working right via R. Get .zip of .csv from here: http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/Rp-eng.cfm?TABID=4&LANG=E&A=R&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=01&GL=-1&GID=1341679&GK=1&GRP=1&O=D&PID=111858&PRID=10&PTYPE=109445&S=0&SHOWALL=0&SUB=0&Temporal=2017&THEME=124&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0

noc.naics <- read_csv("NOC/98-400-X2016298_English_CSV_data.csv", n_max = 239085)
names(noc.naics) <- c(census.2016.varnames[names(noc.naics)[1:19]], "TOT", "Employed", "Unemployed")

noc.naics <- noc.naics %>%
  filter(AGE5 == "Total - Age", SEX3 == "Total - Sex") %>%
  select(NAICS23A, NOC693A, TOT, Employed, Unemployed) %>%
  left_join(tech.occ.def, by = c("NOC693A" = "noc_title" )) %>%
  mutate(NOCDigit = word(NOC693A, start = 1, end = 1, sep = fixed(" "))) %>%
  filter(str_length(NOCDigit) == 4) %>%
  select(-NOCDigit)

sum(noc.naics$Employed)

noc.naics.sum <- noc.naics %>%
  filter(NAICS23A != "Total - Industry - North American Industry Classification System (NAICS) 2012",
         NAICS23A != "Industry - not applicable") %>%
  select(-NOC693A, -TOT, -Unemployed, -noc_code) %>%
  group_by(NAICS23A, digital) %>%
  summarise_all(sum) %>%
  ungroup()

noc.naics.sum$digital <- noc.naics.sum$digital %>%
  replace_na("Non-Tech")

sum(noc.naics.sum$Employed)

#Absolute
noc.naics.abs <- noc.naics.sum %>%
  filter(NAICS23A != "All industries", digital != "Non-Tech") %>%
  group_by(NAICS23A) %>%
  mutate(TechTot = sum(Employed)) %>%
  ungroup() %>%
  mutate(NAICS23A = word(NAICS23A, start = 2, end = -1, sep = fixed(" "))) %>%
  mutate(NAICS23A = str_wrap(NAICS23A, width = 30)) %>%
  arrange(desc(TechTot)) %>%
  top_n(20, TechTot) %>%
  select(-TechTot)

sum(noc.naics.abs$Employed)

#Share of industry - double check math
noc.naics.tot <- noc.naics.sum %>%
  filter(NAICS23A != "All industries") %>%
  group_by(NAICS23A) %>%
  select(-digital) %>%
  summarise_all(sum)

sum(noc.naics.tot$Employed)

noc.naics.share <- noc.naics.sum %>%
  filter(NAICS23A != "All industries", digital != "Non-Tech") %>%
  left_join(noc.naics.tot, by = "NAICS23A") %>%
  mutate(share = Employed.x/Employed.y*100) %>%
  select(-Employed.x, -Employed.y) %>%
  group_by(NAICS23A) %>%
  mutate(TechTot = sum(share)) %>%
  ungroup() %>%
  arrange(desc(TechTot)) %>%
  top_n(20, TechTot) %>%
  select(-TechTot) %>%
  mutate(NAICS23A = word(NAICS23A, start = 2, end = -1, sep = fixed(" "))) %>%
  mutate(NAICS23A = str_wrap(NAICS23A, width = 30))

#Plot abs
plot.column.bf(noc.naics.abs, x = "Employed", cat = "NAICS23A", group.by = "digital", 
               stacked = TRUE,
               order.bar = "descending",
               y.axis = "Employment",
               caption = "StatCan PID 98-400-X2016298")


#Plot share of industry
plot.column.bf(noc.naics.share, x = "share", cat = "NAICS23A", group.by = "digital", 
               stacked = TRUE,
               order.bar = "descending",
               label.unit = "%",
               y.axis = "Share of industry employment",
               caption = "StatCan PID 98-400-X2016298")

write.csv(noc.naics,"noc.naics.csv")
