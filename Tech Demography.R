########################
#Main file for processing and generating tech demography stuff
#This file will be structured as in the report and uses the main data generated in NOC_demographic.R
#Run it only in initial runs as it takes a while to clean those massive data
#  source("NOC_demographic.R")

#Note - this file is still currently a mess right now - don't take anything from it yet

library(data.table)
library(ggplot2)
library(BFTheme)
library(extrafont)
library(stringr)
library(SDMTools)




load("NOC_Demographics/noc_2016_processed.RDA")
load("NOC_Demographics/noc_2016_educ_processed.RDA")
load("NOC_Demographics/noc_2006_processed.RDA")
load("NOC_Demographics/noc_2016_PR_processed.RDA")
load("NOC_Demographics/noc_age_2016_processed.RDA")



#########################
#First set of graphs


noc.dem.sumstat <- noc.dem.master[GEO.LEVEL == "1" &
                                     IM.STATUS.ID==1 &
                                     AGE5.ID == 1 &
                                    EDUC.ID == 1 &
                                    WA.ID == 1 &
                                    SEX3.ID == 1]

noc.dem.sumstat[,c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","ALT.GEO.CODE","IM.STATUS",
                   "IM.STATUS.ID","EDUC","EDUC.ID","WA","WA.ID","AGE5","AGE5.ID","SEX3","SEX3.ID"):=NULL]
rm(noc.dem.master)

noc.dem.sumstat <- noc.dem.sumstat[tech==1]
noc.dem.sumstat[,NOC.TITLE:=str_wrap(str_sub(NOC691,5),30)]
noc.dem.sumstat[digital==1,digital.label:="Digital"]
noc.dem.sumstat[digital==0,digital.label:="High-Tech"]

setkey(noc.dem.sumstat,TOT)
main.top.10.plot <- plot.column.bf(noc.dem.sumstat[23:32],"TOT","NOC.TITLE",group.by = "digital.label",colours = set.colours(2,categorical.choice=c("dark.blue","light.blue")),
                                   label = TRUE,
                                   plot.title = "Top 10 Technology Occupations by Employment in Canada",
                                   plot.fig.num = "Figure x",
                                   order.bar = "ascending",
                                   caption = "Source: 2016 Canadian Census")

setkey(noc.dem.sumstat,AVG.INC)
main.top.10.inc.plot <- plot.column.bf(noc.dem.sumstat[23:32],"AVG.INC","NOC.TITLE",group.by = "digital.label",colours = set.colours(2,categorical.choice=c("dark.blue","light.blue")),
                                   label = TRUE,
                                   label.unit = "$",
                                   plot.title = "Top 10 Technology Occupations by Income in Canada",
                                   plot.fig.num = "Figure x",
                                   order.bar = "ascending",
                                   caption = "Source: 2016 Canadian Census")

setkey(noc.dem.sumstat,digital,TOT)
noc.dem.sumstat



#Add up by tech worker vs non tech worker
noc.dem.tech.map <- noc.dem[GEO.LEVEL == "2" & 
                              LF.STATUS.ID==1 & 
                              AGE.ID==1 & 
                              SEX.ID==1 ,
                            sum(TOT) , by=.(tech,ALT.GEO.CODE,GEO.NAME)] 
noc.dem.tech.map[,tot:=sum(V1),by=ALT.GEO.CODE] #Add the sum for each geographic area
noc.dem.tech.map <- noc.dem.tech.map[tech==1] #Filter out so only retain one set
noc.dem.tech.map[,pct:=V1/tot*100] #Calculate % concentration by geography



setkey(noc.dem.tech.map,V1) #Setkey for the macro-level plot, ordered by size of tech workforce
noc.dem.tech.map.top <- noc.dem.tech.map[143:152] #Extract the top 10
noc.dem.tech.map.top[,GEO.NAME:=str_wrap(GEO.NAME,30)] #Wrap the Geography name to 30 characters
noc.dem.tech.map.top[,Year:=2016] #Set the year
noc.dem.tech.map.top[,tech:=NULL] #Set tech variable to 0

setkey(noc.dem.tech.map,pct) #Setkey to order macro tech data by percentage
noc.dem.tech.map.top.pct <- noc.dem.tech.map[143:152] #Extract the top 10
noc.dem.tech.map.top.pct[,GEO.NAME:=str_wrap(GEO.NAME,30)] #Wrap character number to 30 characters
noc.dem.tech.map.top.pct[,Year:=2016] #Set the year
noc.dem.tech.map.top.pct[,tech:=NULL] #Set tech variable to 0


#####################
#Digital stuff - not for change
noc.dem.tech.map.dig <- noc.dem[GEO.LEVEL == "2" & 
                              LF.STATUS.ID==1 & 
                              AGE.ID==1 & 
                              SEX.ID==1 ,
                            sum(TOT) , by=.(tech,digital,ALT.GEO.CODE,GEO.NAME)] 
noc.dem.tech.map.dig[,tot:=sum(V1),by=ALT.GEO.CODE] #Add the sum for each geographic area
noc.dem.tech.map.dig <- noc.dem.tech.map.dig[tech==1] #Filter out so only retain one set
noc.dem.tech.map.dig[,pct:=V1/tot*100] #Calculate % concentration by geography
noc.dem.tech.map.dig[,tot.tech:=sum(V1),by=ALT.GEO.CODE]
noc.dem.tech.map.dig[,tot.tech.pct:=100*tot.tech/tot]
noc.dem.tech.map.dig[digital==0,digital.lab:="High-Tech"]
noc.dem.tech.map.dig[digital==1,digital.lab:="Digital"]



setkey(noc.dem.tech.map.dig,tot.tech) #Setkey for the macro-level plot, ordered by size of tech workforce
noc.dem.tech.map.top.dig <- noc.dem.tech.map.dig[285:304] #Extract the top 10
noc.dem.tech.map.top.dig[,GEO.NAME:=str_wrap(GEO.NAME,30)] #Wrap the Geography name to 30 characters
noc.dem.tech.map.top.dig[,Year:=2016] #Set the year
noc.dem.tech.map.top.dig[,tech:=NULL] #Set tech variable to 0

setkey(noc.dem.tech.map.dig,tot.tech.pct) #Setkey to order macro tech data by percentage
noc.dem.tech.map.top.pct.dig <- noc.dem.tech.map.dig[285:304] #Extract the top 10
noc.dem.tech.map.top.pct.dig[,GEO.NAME:=str_wrap(GEO.NAME,30)] #Wrap character number to 30 characters
noc.dem.tech.map.top.pct.dig[,Year:=2016] #Set the year
noc.dem.tech.map.top.pct.dig[,tech:=NULL] #Set tech variable to 0



# 2006 Census Top Counts
noc.dem.2006.tech.map <- noc.2006[nchar(GEO.CODE)==5 & Dem == "Total labour force by age groups",
                                 sum(TOT.SEX),by=.(tech,GEO.CODE,GEO.NAME)] #Sum up total by CMA for Canada
noc.dem.2006.tech.map[,tot:=sum(V1),by=.(GEO.CODE)] #Get the total number by CMA tech or non tech
noc.dem.2006.tech.map <- noc.dem.2006.tech.map[tech=="Tech Occupation"] #Only keep tech occupations
noc.dem.2006.tech.map[,pct:=V1/tot*100] #Get the percentage


#Changes in absolute number 
setkey(noc.dem.tech.map.top,ALT.GEO.CODE) #Setkey and order by Geo code
setkey(noc.dem.tech.map.top.pct,ALT.GEO.CODE) #Setkey and order by GEO Code
setkey(noc.dem.2006.tech.map,GEO.CODE) #Setkey and order by GEO Code
noc.dem.tech.map.top.2006 <- noc.dem.2006.tech.map[GEO.CODE %in% noc.dem.tech.map.top[,ALT.GEO.CODE]] #Select only for Geography in the 2016 top table
noc.dem.tech.map.top.2006[,Year:=2006] #Set the Year for the Census
noc.dem.tech.map.top.2006[,tech:=NULL] #Remove the tech dummy column
names(noc.dem.tech.map.top.2006) <- c("ALT.GEO.CODE","GEO.NAME","V1","tot","pct","Year") #Change name of 2006 table
noc.dem.tech.map.top <- rbindlist(list(noc.dem.tech.map.top,noc.dem.tech.map.top.2006)) #Bind the tables together for 2016 and 2006
noc.dem.tech.map.top[,GEO.NAME:=str_trim(GEO.NAME)] #Trim the white space that exists
noc.dem.tech.map.top[ALT.GEO.CODE == "24421" & Year == 2016,GEO.NAME:="Quebec"] #Need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top[ALT.GEO.CODE == "24462" & Year == 2016,GEO.NAME:="Montreal"] #Need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top[ALT.GEO.CODE == "24421" & Year == 2006,GEO.NAME:="Quebec"] #Need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top[ALT.GEO.CODE == "24462" & Year == 2006,GEO.NAME:="Montreal"] #need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top[GEO.NAME=="Kitchener", GEO.NAME:="Kitchener - Cambridge -\nWaterloo"] #Making sure they're all the same labels
rm(noc.dem.tech.map.top.2006) #Remove 2006 table

noc.dem.tech.map[,c("tech","GEO.NAME","V1","tot"):=NULL] #Remove all the redundant columns - for map

#Changes in relative number 
noc.dem.tech.map.top.pct.2006 <- noc.dem.2006.tech.map[GEO.CODE %in% noc.dem.tech.map.top.pct[,ALT.GEO.CODE]] #Select only for Geography in the 2016 top table
noc.dem.tech.map.top.pct.2006[,Year:=2006] #Set the Year for the Census
noc.dem.tech.map.top.pct.2006[,tech:=NULL] #Remove the tech dummy column
names(noc.dem.tech.map.top.pct.2006) <- c("ALT.GEO.CODE","GEO.NAME","V1","tot","pct","Year") #Change name of 2006 table
noc.dem.tech.map.top.pct <- rbindlist(list(noc.dem.tech.map.top.pct,noc.dem.tech.map.top.pct.2006)) #Bind the tables together for 2016 and 2006
noc.dem.tech.map.top.pct[,GEO.NAME:=str_trim(GEO.NAME)] #Trim the white space that exists
noc.dem.tech.map.top.pct[ALT.GEO.CODE == "24421" & Year == 2016,GEO.NAME:="Quebec"] #Need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top.pct[ALT.GEO.CODE == "24462" & Year == 2016,GEO.NAME:="Montreal"] #Need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top.pct[ALT.GEO.CODE == "24421" & Year == 2006,GEO.NAME:="Quebec"] #Need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top.pct[ALT.GEO.CODE == "24462" & Year == 2006,GEO.NAME:="Montreal"] #need to find a better solution to do this but right now, manually changing stuff
noc.dem.tech.map.top.pct[GEO.NAME=="Kitchener", GEO.NAME:="Kitchener - Cambridge -\nWaterloo"] #Making sure they're all the same labels
rm(noc.dem.tech.map.top.pct.2006) #Remove 2006 table

noc.dem.tech.map[,c("tech","GEO.NAME","V1","tot"):=NULL] #Remove all the redundant columns - for map


noc.dem.geo.absolute <- plot.column.bf(noc.dem.tech.map.top.dig,"V1","GEO.NAME",
                                       order.bar = "ascending",
                                       stacked = TRUE,
                                       group.by = "digital.lab",
                                       colours = set.colours(2,categorical.choice = c("dark.blue","light.blue")),
                                       plot.title = "Geographical Distribution of Technology Occupations, Canada",
                                       plot.fig.num = "Figure x")



noc.dem.geo.relative <- plot.column.bf(noc.dem.tech.map.top.pct.dig[Year==2016],"pct","GEO.NAME",
                                       order.bar = "ascending",
                                       stacked = TRUE,
                                       group.by = "digital.lab",
                                       colours = set.colours(2,categorical.choice = c("dark.blue","light.blue")),
                                       label.unit = "%",
                                       plot.title = "Geographical Concentration (%) of Technology Occupations, 2016 Canada",
                                       plot.fig.num = "Figure x")


setkey(noc.dem.tech.map.top,Year)
noc.dem.geo.absolute.change <- plot.change.arrow.bf(noc.dem.tech.map.top,"V1","GEO.NAME","Year",
                                                    plot.title = "10 Years Change in Absolute Number of Tech Workers by Canadian Cities",
                                                    plot.fig.num = "Figure x",
                                                    caption = "Source: 2016, 2006 Canadian Census")

setkey(noc.dem.tech.map.top.pct,Year)
noc.dem.geo.relative.change <- plot.change.arrow.bf(noc.dem.tech.map.top.pct[GEO.NAME!="Carleton Place"],"pct","GEO.NAME","Year",
                                                    plot.title = "10 Years Change in Relative Number of Tech Workers by Canadian Cities",
                                                    plot.fig.num = "Figure x",
                                                    unit.x = "%",
                                                    caption = "Source: 2016, 2006 Canadian Census")


fig.noc.tech.map <- plot.map.cma.bf(value.data=noc.dem.tech.map,
                                    plot.title = "Geographic Concentration of Technology Occupations Across Canada",
                                    plot.fig.num = "Figure x",
                                    legend.title = "Percentage Concentration of Tech Occupations",
                                    caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.bc <- plot.map.cma.bf(province.name = c("BC"),
                                       value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("59")],
                                       plot.title = "Geographic Concentration of Technology Occupations Across British Columbia",
                                       plot.fig.num = "Figure x",
                                       legend.title = "Percentage Concentration of Tech Occupations",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.prairie <- plot.map.cma.bf(province.name = c("AB","SK","MB"),
                                            value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("46","47","48")],
                                            plot.title = "Geographic Concentration of Technology Occupations Across the Prairies",
                                            plot.fig.num = "Figure x",
                                            legend.title = "Percentage Concentration of Tech Occupations",
                                            caption = "Source: 2016 Canadian Census, BII+E Analysis")



fig.noc.tech.map.on <- plot.map.cma.bf(province.name = c("ON"),
                                       value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) =="35"],
                                       plot.title = "Geographic Concentration of Technology Occupations Across Ontario",
                                       plot.fig.num = "Figure x",
                                       legend.title = "Percentage Concentration of Tech Occupations",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.qc <- plot.map.cma.bf(province.name = c("QC"),
                                       value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) =="24"],
                                       plot.title = "Geographic Concentration of Technology Occupations Across Quebec",
                                       plot.fig.num = "Figure x",
                                       legend.title = "Percentage Concentration of Tech Occupations",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.atlantic <- plot.map.cma.bf(province.name = c("NL","PE","NS","NB"),
                                             value.data=noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("10","11","12","13")],
                                             plot.title = "Geographic Concentration of Technology Occupations Across the Atlantic",
                                             plot.fig.num = "Figure x",
                                             legend.title = "Percentage Concentration of Tech Occupations",
                                             caption = "Source: 2016 Canadian Census, BII+E Analysis")

fig.noc.tech.map.territories <- plot.map.cma.bf(province.name=c("YT","NT","NU"),
                                                value.data = noc.dem.tech.map[str_sub(ALT.GEO.CODE,1,2) %in% c("60","61","62")],
                                                plot.title = "Geographic Concentration of Technology Occupations Across the Territories",
                                                plot.fig.num = "Figure x",
                                                legend.title = "Percentage Concentration of Tech Occupations",
                                                caption = "Source: 2016 Canadian Census, BII+E Analysis")


###########################
#Decomposing pay by age
noc.age.2016[,c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GNR","DATA.QUALITY.FLAG","ALT.GEO.CODE","GEO.NAME"):=NULL]
noc.age.2016 <- noc.age.2016[EMP.ID %in% c(1,4)]
noc.age.2016 <- noc.age.2016[WA.ID==1]
noc.age.2016[,c("WA","WA.ID"):=NULL]
noc.age.2016.pay <- noc.age.2016[EMP.ID==4,.(AGE6,SEX3,OCC691,FL.TOT,tech)]
noc.age.2016.par <- noc.age.2016[EMP.ID==1,.(AGE6,SEX3,OCC691,FL.TOT,tech)]
setkey(noc.age.2016.par,AGE6,SEX3,OCC691)
setkey(noc.age.2016.pay,AGE6,SEX3,OCC691)
noc.age.2016.par[,AVG.INC:=noc.age.2016.pay[,FL.TOT]]
rm(noc.age.2016.pay)
noc.age.2016.pay.sumstat <- noc.age.2016.par[,.(sum(FL.TOT),wt.mean(AVG.INC,FL.TOT)),by=.(AGE6,SEX3,tech)]

test.data %>%
  plot_geo() %>%
  add_polygons(x=~long,y=~lat) %>%
  layout(geo=list(scope="north america",projection=list(type="conic equal area")))
p <- leaflet(areas.cmaDF) %>% addPolygons(lng=~long,lat=~lat,group=~group)
