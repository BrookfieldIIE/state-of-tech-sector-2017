########################
#Main file for processing and generating tech demography stuff
#This file will be structured as in the report and uses the main data generated in NOC_demographic.R
#Run it only in initial runs as it takes a while to clean those massive data
#  source("NOC_demographic.R")

#Note - this file is still currently a mess right now - don't take anything from it yet







#Add up by tech worker vs non tech worker
noc.dem.tech.map <- noc.dem[GEO.LEVEL == "2" & 
                              LF.STATUS.ID==1 & 
                              AGE.ID==1 & 
                              SEX.ID==1 ,
                            sum(TOT) , by=.(tech,ALT.GEO.CODE,GEO.NAME)] 
noc.dem.tech.map[,tot:=sum(V1),by=ALT.GEO.CODE] #Add the sum for each geographic area
noc.dem.tech.map <- noc.dem.tech.map[tech==1] #Filter out so only retain one set
noc.dem.tech.map[,pct:=V1/tot*100] #Calculate % concentration by geography
noc.dem.tech.map[,c("tech","GEO.NAME","V1","tot"):=NULL] #Remove all the redundant columns



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



#Do a simple education attainment by sex - should aggregate by age next
noc.demo.educ.prim <- noc.demo.educ[ GEO_NAME=="Canada"& CIP82.ID==1,.(sum(TOT.EDUC),sum(TOT.NO),sum(TOT.SEC),sum(TOT.APP),sum(TOT.COL),sum(TOT.BEBA),
                                                                       sum(TOT.BA),sum(TOT.ABBA)),by=.(tech,SEX3)] #Get the primary facts table


names(noc.demo.educ.prim) <- c("tech","SEX","Total","No Degree","Secondary School","Apperenticeship and Trade Schools","College, CEGEP","University Degree Below Bachelors",
                               "Bachelors","Above Bachelors")
noc.demo.educ.prim <- melt(noc.demo.educ.prim,id.vars = c("tech","SEX"),variable.name = "Education",value.name="Count")

noc.demo.educ.prim[,total:=max(Count),by=.(tech,SEX)]
noc.demo.educ.prim[,pct:=Count/total*100]

educ.graph <- plot.column.bf(noc.demo.educ.prim[SEX=="Total - Sex" & Education!="Total"],
                             "pct","tech",stacked = TRUE,group.by = "Education",
                             label.unit = "%",plot.title = "Educational Composition of Tech Occupations",
                             plot.fig.num = "Figure X",caption = "Source: 2016 Canadian Census, BII+E Analysis")

educ.graph.sex <- plot.column.bf(noc.demo.educ.prim[SEX != "Total - Sex" & tech=="Tech Occupation" & Education != "Total"],
                                 "pct","SEX",stacked = TRUE, group.by = "Education",
                                 label.unit = "%",
                                 plot.title = "Educational Composition by Sex - Technology Occupations",
                                 plot.fig.num = "Figure X", caption = "Source: 2016 Canadian Census, BII+E Analysis")

noc.demo.educ.cip <- noc.demo.educ[GEO_NAME=="Canada",sum(TOT.EDUC),by=.(tech,CIP82,SEX3)]

noc.demo.educ.cip[,total:=max(V1),by=.(tech,SEX3)]
noc.demo.educ.cip[,pct:=V1/total*100]



##########################################
# 2006 Classification



test.data %>%
  plot_geo() %>%
  add_polygons(x=~long,y=~lat) %>%
  layout(geo=list(scope="north america",projection=list(type="conic equal area")))
p <- leaflet(areas.cmaDF) %>% addPolygons(lng=~long,lat=~lat,group=~group)
