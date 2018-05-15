#######################
#NOC Education stuff


load("NOC_Demographics/noc_2016_educ_processed.RDA")
load("NOC_Demographics/noc_2006_processed.RDA")



#Do a simple education attainment by sex - should aggregate by age next
noc.demo.educ.prim <- noc.demo.educ[ GEO.NAME=="Canada"& CIP82.ID==1,.(sum(TOT.EDUC),sum(TOT.NO),sum(TOT.SEC),sum(TOT.APP),sum(TOT.COL),sum(TOT.BEBA),
                                                                       sum(TOT.BA),sum(TOT.ABBA)),by=.(tech,SEX3)] #Get the primary facts table


names(noc.demo.educ.prim) <- c("tech","SEX","Total","No Degree","Secondary School","Apperenticeship and Trade Schools","College, CEGEP","University Degree Below Bachelors",
                               "Bachelors","Above Bachelors") #Change the column names - prepare to melt
noc.demo.educ.prim <- melt(noc.demo.educ.prim,id.vars = c("tech","SEX"),variable.name = "Education",value.name="Count") #Melt so Education is a single column

noc.demo.educ.prim[,total:=max(Count),by=.(tech,SEX)] #Calculate the max
noc.demo.educ.prim[,pct:=Count/total*100] #Calculate the percentage

educ.graph <- plot.column.bf(noc.demo.educ.prim[SEX=="Total - Sex" & Education!="Total"],
                             "pct","tech",stacked = TRUE,group.by = "Education",
                             label.unit = "%",plot.title = "Educational Composition of Tech Occupations",
                             plot.fig.num = "Figure X",caption = "Source: 2016 Canadian Census, BII+E Analysis") #General educational stacked bar

educ.graph.sex <- plot.column.bf(noc.demo.educ.prim[SEX != "Total - Sex" & tech=="Tech Occupation" & Education != "Total"],
                                 "pct","SEX",stacked = TRUE, group.by = "Education",
                                 label.unit = "%",
                                 plot.title = "Educational Composition by Sex - Technology Occupations",
                                 plot.fig.num = "Figure X", caption = "Source: 2016 Canadian Census, BII+E Analysis") #By Sex educational stacked bar



###############################
#Major demographic stuff
noc.demo.educ.cip <- noc.demo.educ[GEO.NAME=="Canada",sum(TOT.EDUC),by=.(tech,CIP82,SEX3)] #Don't care much about other degrees, so aggregate by major chocie
names(noc.demo.educ.cip) <- c("tech","CIP82","SEX3","TOT") #Change column names
noc.demo.educ.cip[,total:=max(TOT),by=.(tech,SEX3)] #Find overall total
noc.demo.educ.cip[,pct:=TOT/total*100] #Find percentage

noc.demo.educ.cip[,cip.code:=tstrsplit(CIP82,"[.]",keep=1)] #Get the CIP codes to filter out for only major components
noc.demo.educ.cip.pri <- noc.demo.educ.cip[nchar(cip.code)>2] #Get primary CIP as a separate table for 10 years change
noc.demo.educ.cip.pri <- noc.demo.educ.cip.pri[CIP82!="Other"] #Filter out other since 2006 doesn't have that
noc.demo.educ.cip.pri[,cip.code:=NULL] #From here basically prepare the table to merge
noc.demo.educ.cip.pri[,total:=TOT]
noc.demo.educ.cip.pri[,TOT:=NULL]
noc.demo.educ.cip.pri[,pct:=NULL]
noc.demo.educ.cip.pri[,Year:=2016]
names(noc.demo.educ.cip.pri) <- c("tech","CIP","SEX","TOT","Year")


##########################################
# 2006 Education Major for Change graph
noc.2006.educ <- noc.2006[Dem %in% unique(Dem)[103:115]] #Filter out to only get the data for major programs
noc.2006.educ <- noc.2006.educ[Geo == "Canada (01)   20000"] #Filter out so only consider Canada

noc.2006.educ <- noc.2006.educ[,.(sum(TOT.SEX),sum(TOT.MALE),sum(TOT.FEMALE)),by=.(tech,Dem)] #Get all the sums by CIP
noc.2006.educ[,Dem:=str_trim(Dem)] #Remove the white spaces for CIP
names(noc.2006.educ) <- c("tech","Dem","Total - Sex","Male","Female") #Change name
noc.2006.educ <- melt(noc.2006.educ,id.vars=c("tech","Dem"),variable.name="Sex",value.name="TOT") #Melt so Sex is one column
noc.2006.educ[,Year:=2006] #Prepare to merge
names(noc.2006.educ) <- c("tech","CIP","SEX","TOT","Year")

noc.cip.change <- rbindlist(list(noc.demo.educ.cip.pri,noc.2006.educ)) #Merge the tables
noc.cip.change[CIP=="Health, parks, recreation and fitness",CIP:="Health and related fields"] #Concordance between 2000 CIP and 2016 CIP
noc.cip.change <- noc.cip.change[CIP != "Total - Major field of study - Classification of Instructional Programs (CIP) 2016" &
                                   CIP != "Total labour force by major field of study - Classification of Instructional Programs, 2000" &
                                   CIP != "No postsecondary certificate, diploma or degree"] #Filter out the "big" stuff that messes up the visualization

setkey(noc.cip.change,CIP,Year) #Order the table to prepare to plot change arrow graphs



cip.change.male <- plot.change.arrow.bf(noc.cip.change[SEX == "Male" & tech == "Tech Occupation"],"TOT","CIP","Year",
                                        plot.title = "Change over 10 years, Major Choice by Male Technology Workers",
                                        plot.fig.num = "Figure X",
                                        x.axis = "Size of labour force",
                                        caption = "Source: 2016 & 2006 Canadian Census, BII+E Analysis")


cip.change.female <- plot.change.arrow.bf(noc.cip.change[SEX == "Female" & tech == "Tech Occupation"],"TOT","CIP","Year",
                                          plot.title = "Change over 10 years, Major Choice by Female Technology Workers",
                                          plot.fig.num = "Figure X",
                                          x.axis = "Size of labour force",
                                          caption = "Source: 2016 & 2006 Canadian Census, BII+E Analysis")

