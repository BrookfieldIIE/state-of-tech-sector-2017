###################
# GDP
###################

library(data.table)
library(tidyr)
library(tidyverse)
library(skimr)
library(lubridate)
library(BFTheme)
library(extrafont)

setwd("~/GitHub/state-of-tech-sector-2017/Output")

##########
# Acquire data
##########

download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/03790031-eng.zip","03790031-eng.zip")
unzip("03790031-eng.zip")
gdp.n <- read_csv("03790031-eng.csv", guess_max=10000)

# Label Industries as digital/tech and goods/services