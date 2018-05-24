###################
# Tech Investment
###################

library(data.table)
library(tidyr)
library(tidyverse)
library(skimr)
library(lubridate)
library(BFTheme)
library(stringr)
library(extrafont)
setwd("~/GitHub/state-of-tech-sector-2017/Investment")

##########
# Acquire data
##########

download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/03800068-eng.zip","03800068-eng.zip")
unzip("03800068-eng.zip")
cap.form <- read_csv("03800068-eng.csv", guess_max=10000)



bus.vec <- c("v62143970", #Business Computing
              "v62143971", #Business Comm
              "v62143980", #Business R&D - may remove later
              "v62143981") #Business Software

govt.vec <- c("v62143991", #Govt Computing
              "v62143992", #Govt Comm
              "v62144002", #Govt R&D - may remove later
              "v62144003") #Govt Software

##########
# Arrange data
##########

skim(cap.form)
skim(cap.form$Value)

cap.form <- cap.form %>%
  mutate(Date = ymd(Ref_Date, truncated = 1)) %>%
  filter(str_detect(Value, "\\d"), PRI == "Chained (2007) dollars", SEAS == "Seasonally adjusted at annual rates", Date > "1980-12-01") %>%
  select(-Coordinate, -PRI, -SEAS, -Ref_Date, -GEO) %>%
  mutate(EST = str_sub(EST,1,str_length(EST)-14)) %>%
  mutate(Value = as.numeric(Value))
  
bus.cap <- cap.form %>%
  filter(Vector %in% bus.vec) %>%
  select(-Vector)

govt.cap <- cap.form %>%
  filter(Vector %in% govt.vec) %>%
  select(-Vector)

total.cap <- cap.form %>%
  filter(EST == "Total gross fixed capital formation") %>%
  select(-Vector)

#rm(cap.form)

bus.cap.perc <- bus.cap %>% left_join(total.cap, by = "Date") %>%
  mutate(Value = Value.x/Value.y, EST = EST.x) %>%
  select(-EST.x, -EST.y, -Value.x, -Value.y)

govt.cap.perc <- govt.cap %>% left_join(total.cap, by = "Date") %>%
  mutate(Value = Value.x/Value.y, EST = EST.x) %>%
  select(-EST.x, -EST.y, -Value.x, -Value.y)

##########
#Plots
##########

#Need to fix fonts for export and change to area charts

# ggplot(bus.cap, aes(x=Date, y=Value, group=EST, col=EST, fill=EST)) + 
#   brookfield.base.theme() + 
#   geom_line() +
#   #geom_area(position = 'stack') +
#   scale_x_date(date_labels = "%Y") + scale_y_continuous(labels = scales::dollar) +
#   xlab("Year") + ylab("Gross Fixed Capital Flow ($M)")


# Business capital formation - $
plot.line.bf(as.data.table(bus.cap), "Date", "Value", group.by = "EST",
             plot.title = "Digital Investment by Business in Canada",
             plot.fig.num = "Figure x.x",
             unit.y = "$",
             y.axis = "Gross Fixed Capital Flow for Digital Assets ($M)",
             x.axis = "Year",
             caption = "StatCan CANSIM Table 380-0068: Gross fixed capital formation")

# Business capital formation - Share
plot.line.bf(as.data.table(bus.cap.perc), "Date", "Value", group.by = "EST",
             plot.title = "Digital Investment by Business in Canada",
             plot.fig.num = "Figure x.x",
             unit.y = "%",
             y.axis = "Digital Share of Gross Fixed Capital Flow (%)",
             x.axis = "Year",
             caption = "StatCan CANSIM Table 380-0068: Gross fixed capital formation")

# Govt Capital Formation - $
plot.line.bf(as.data.table(govt.cap), "Date", "Value", group.by = "EST",
             plot.title = "Digital Investment by Government in Canada",
             plot.fig.num = "Figure x.x",
             unit.y = "$",
             y.axis = "Gross Fixed Capital Flow for Digital Assets ($M)",
             x.axis = "Year",
             caption = "StatCan CANSIM Table 380-0068: Gross fixed capital formation")

# Govt Capital Formation - Share
plot.line.bf(as.data.table(govt.cap.perc), "Date", "Value", group.by = "EST",
             plot.title = "Digital Investment by Government in Canada",
             plot.fig.num = "Figure x.x",
             unit.y = "%",
             y.axis = "Digital Share of Gross Fixed Capital Flow (%)",
             x.axis = "Year",
             caption = "StatCan CANSIM Table 380-0068: Gross fixed capital formation")

# Govt Capital Formation - Share
plot.line.bf(as.data.table(govt.cap.perc), "Date", "Value", group.by = "EST", fill = "EST",
             plot.title = "Digital Investment by Government in Canada",
             plot.fig.num = "Figure x.x",
             unit.y = "%",
             y.axis = "Digital Share of Gross Fixed Capital Flow (%)",
             x.axis = "Year",
             caption = "StatCan CANSIM Table 380-0068: Gross fixed capital formation") +
  geom_area()

