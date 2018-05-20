###################
# Tech Investment
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
# Make plots
##########

spillover <- read_csv("digitalspillover.csv")
spillover <- spillover %>%
  mutate(GDPShare = GDPShare*100)

#Change to area charts
plot.line.bf(as.data.table(spillover), "Year", "GDPShare", group.by = "DigitalEconomy",
             plot.title = "Digital Economy - Contribution to GDP",
             plot.fig.num = "Figure x.x",
             unit.y = "%",
             y.axis = "Output as a share of Canadian GDP (%)",
             x.axis = "Year",
             caption = "Proprietary data from Huawei and Oxford Economics from Digital Spillover: Measuring the true impact of the Digital Economy") +
  geom_area()
