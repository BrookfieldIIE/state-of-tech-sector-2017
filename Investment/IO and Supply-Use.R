###################
# Supply-Use
###################

library(data.table)
library(tidyr)
library(tidyverse)
library(skimr)
library(lubridate)
library(BFTheme)
library(extrafont)
library(network)
library(sna)
library(GGally)
library('igraph')
setwd("~/GitHub/state-of-tech-sector-2017/Investment")

##########
# Acquire data
##########

#User guide to Supply-Use: http://www.statcan.gc.ca/pub/13-606-g/2016001/article/14619-eng.htm
#Concordance: http://www.statcan.gc.ca/eng/statistical-programs/document/1303_D7_T9_V1
# Categorize sectors to special aggregates with concordance table

#Input-Output
download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/03810037-eng.zip","03810037-eng.zip")
unzip("03810037-eng.zip")
inout <- read_csv("03810037-eng.csv", guess_max=10000)

cat.dest <- c("Computer and peripheral equipment manufacturing (x 1,000)",
          "Software publishers (x 1,000)",
          "Telecommunications (x 1,000)",
          "Data processing, hosting, and related services (x 1,000)",
          "Computer systems design and related services (x 1,000)",
          "Construction, computer and electronic product manufacturing (x 1,000)",
          "Machinery and equipment, computer and electronic product manufacturing (x 1,000)",
          "Intellectual property products, computer and electronic product manufacturing (x 1,000)")

cat.ori <- c("Computer and peripheral equipment manufacturing",
             "Software publishers",
             "Telecommunications",
             "Data processing, hosting, and related services",
             "Computer systems design and related services",
             "Construction, computer and electronic product manufacturing",
             "Machinery and equipment, computer and electronic product manufacturing",
             "Intellectual property products, computer and electronic product manufacturing")

##########
# Arrange data
##########

skim(inout)

dest.n <- inout %>%
  filter(Ref_Date == 2014, VALUATION == "Purchaser price", DESTINATION == "Total (x 1,000)", ORIGIN != "Total") %>%
  mutate(DESTINATION = str_sub(DESTINATION,1,str_length(DESTINATION)-10)) %>%
  select(-GEO, -Vector, -Coordinate , -Ref_Date, -VALUATION, -DESTINATION) %>%
  mutate(Digital = if_else(ORIGIN %in% cat.ori, "Digital", "Non-Digital"))

origin.n <- inout %>%
  filter(Ref_Date == 2014, VALUATION == "Purchaser price", DESTINATION != "Total (x 1,000)", ORIGIN == "Total") %>%
  select(-GEO, -Vector, -Coordinate , -Ref_Date, -VALUATION, -ORIGIN) %>%
  mutate(DESTINATION = str_sub(DESTINATION,1,str_length(DESTINATION)-10)) %>%
  mutate(Digital = if_else(DESTINATION %in% cat.ori, "Digital", "Non-Digital"))

dest.e <- inout %>%
  filter(ORIGIN %in% cat.ori, Ref_Date == 2014, VALUATION == "Purchaser price", ORIGIN != "Total", DESTINATION != "Total (x 1,000)", Value > 500000) %>%
  select(-GEO, -Vector, -Coordinate , -Ref_Date, -VALUATION) %>%
  mutate(DESTINATION = str_sub(DESTINATION,1,str_length(DESTINATION)-10)) %>%
  rename(to = DESTINATION, from = ORIGIN, weight = Value)

origin.e <- inout %>%
  filter(DESTINATION %in% cat.dest, Ref_Date == 2014, VALUATION == "Purchaser price", ORIGIN != "Total", DESTINATION != "Total (x 1,000)", Value > 300000) %>%
  select(-GEO, -Vector, -Coordinate , -Ref_Date, -VALUATION) %>%
  mutate(DESTINATION = str_sub(DESTINATION,1,str_length(DESTINATION)-10)) %>%
  rename(to = DESTINATION, from = ORIGIN, weight = Value)

dest <- inout %>%
  filter(ORIGIN %in% cat.ori, Ref_Date == 2014, VALUATION == "Purchaser price", ORIGIN != "Total", DESTINATION != "Total (x 1,000)", Value > 500000) %>%
  filter(ORIGIN != "Telecommunications") %>%
  select(-GEO, -Vector, -Coordinate , -Ref_Date, -VALUATION, -ORIGIN) %>%
  mutate(DESTINATION = str_sub(DESTINATION,1,str_length(DESTINATION)-10)) %>%
  group_by(DESTINATION) %>%
  summarise_all(sum) %>%
  top_n(20) %>%
  #arrange(desc()) %>%
  ungroup()
  
dest$DESTINATION <- str_wrap(dest$DESTINATION, 50)

##########
#Plots
##########

plot.column.bf(as.data.table(dest), x="Value", cat="DESTINATION",
               plot.title = "Top Consumers of Canadian Digital Sector Outputs",
               plot.fig.num = "Figure x.x",
               order.bar = TRUE)


ggplot(dest, aes(DESTINATION, fill=bfc)) + geom_bar(aes(weight=Value)) +
  brookfield.base.theme()

##########
# Network Plots
##########

# Make a column graph first for digital economy destinations

## Network graph
# Make node size = sector total
# Categorize sectors
# https://gephi.org/

net <- graph_from_data_frame(d=dest.e, directed=F) 
net <- simplify(net, remove.multiple = F, remove.loops = T) 
E(net)$width <- E(net)$weight/1000000
l <- layout_in_circle(net)
plot(net, layout=l)

plot(net)
plot(net, edge.arrow.size=.4,vertex.label=NA)
plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

# https://gist.github.com/Vessy/6047440
# https://gephi.org/users/quick-start/

# http://www.htmlwidgets.org/showcase_visNetwork.html
# http://www.htmlwidgets.org/showcase_networkD3.html
# http://kateto.net/network-visualization
# https://briatte.github.io/ggnet/

net <- graph_from_data_frame(d=origin.e, vertices = origin.n, directed=T)
net <- simplify(net, remove.multiple = F, remove.loops = T)
E(net)$width <- E(net)$weight/1000000
l <- layout_in_circle(net)
plot(net, layout=l)

visNetwork(origin.n, origin.e) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
