###################
# ICT Capital Contribution and Share
###################

library(data.table)
library(tidyr)
library(tidyverse)
library(skimr)
library(lubridate)
library(BFTheme)
library(extrafont)
setwd("~/GitHub/state-of-tech-sector-2017/Investment")

##########
# Acquire data
##########

download.file("https://www.conference-board.org/retrievefile.cfm?filename=TED_FLATFILE_ADJ_MAR20181.txt&type=subsite","ted.txt")
ted <- read_tsv("ted.txt")

##########
# Arrange data
##########

ted.cont <- ted %>%
  filter(TED_VERSION == "TED_II", COUNTRY == "Canada", INDICATOR_LONG == "ICT Capital Contribution") %>% # | INDICATOR_LONG == "ICT Capital Share") %>%
  select(-COUNTRY, -COUNTRY_ISO, -DESCRIPTION, -TED_VERSION, -INDICATOR_NR, -INDICATOR_SHORT )
  
ted.share <- ted %>%
  filter(TED_VERSION == "TED_II", COUNTRY == "Canada", INDICATOR_LONG == "ICT Capital Share") %>%
  select(-COUNTRY, -COUNTRY_ISO, -DESCRIPTION, -TED_VERSION, -INDICATOR_NR, -INDICATOR_SHORT )


##########
# Make plots
##########

plot.line.bf(as.data.table(ted.cont), "YEAR", "VALUE", group.by = "INDICATOR_LONG",
             plot.title = "ICT Capital Contribution to GDP Growth",
             plot.fig.num = "Figure x.x",
             y.axis = "Contribution to GDP Growth",
             x.axis = "Year",
             caption = "Conference Board Total Economy Database")

plot.line.bf(as.data.table(ted.share), "YEAR", "VALUE", group.by = "INDICATOR_LONG",
             plot.title = "ICT Capital Share of GDP Growth",
             plot.fig.num = "Figure x.x",
             y.axis = "Share of GDP Growth",
             x.axis = "Year",
             caption = "Conference Board Total Economy Database")
