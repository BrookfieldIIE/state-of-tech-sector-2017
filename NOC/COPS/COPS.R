############# COPS Projections

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


cops <- read_csv("NOC/COPS/employment_emploi_2017_2026.csv")
noc.to.cops <- read_csv("NOC/COPS/NOC_occ_grouping_eng_2017-2026.csv")

cops <- cops %>%
  select(-Occupation_Name, -Nom_de_la_profession) %>%
  gather(Year, Emp, -Code)

noc.to.cops <- noc.to.cops %>%
  gather(Occ2, Occ, -CODE, - `Occupational Group`) %>%
  select(-Occ2) %>%
  filter(!is.na(Occ)) %>%
  mutate(noc_code = substr(Occ, 1,4)) %>%
  inner_join(tech.occ.def, by = "noc_code") %>%
  select(-Occ, -noc_code, -noc_title) %>%
  distinct() %>%
  filter(CODE != "N5222" | digital != "Digital", CODE != "N7246" | digital != "Digital")

tech.cops <- noc.to.cops %>%
  left_join(cops, by = c("CODE" = "Code")) %>%
  mutate(Year = as.numeric(Year)) %>%
  select(-CODE, -`Occupational Group`) %>%
  group_by(digital, Year) %>%
  summarise_all(sum)

cops.total <- cops %>%
  filter(Code == "N0000") %>%
  mutate(Year = as.numeric(Year))

tech.cops.share <- tech.cops %>%
  left_join(cops.total, by = "Year") %>%
  mutate(share = Emp.x/Emp.y*100) %>%
  select(-Emp.x, -Emp.y, -Code)


#Plots
##Abs
plot.line.bf(tech.cops, x = "Year", y = "Emp", group.by = "digital",
             caption = "Source: Canadian Occupational Projection System (COPS)",
             x.axis = "Year",
             y.axis = "Employment",
             show.points = FALSE) + 
  scale_y_continuous(limits=c(0,NA), labels = scales::comma)

##Share
plot.line.bf(tech.cops.share, x = "Year", y = "share", group.by = "digital",
             caption = "Source: Canadian Occupational Projection System (COPS)",
             x.axis = "Year",
             y.axis = "Share of Canadian Employment",
             unit.y = "%",
             show.points = FALSE)

  