#######################
#NOC Education stuff
library(data.table)
library(ggplot2)
library(extrafont)
library(SDMTools)
library(BFTheme)
library(stringr)
library(ggrepel)

load("NOC_Demographics/noc_2016_educ_processed.RDA")
load("NOC_Demographics/noc_2006_processed.RDA")
load("NOC_Demographics/noc_2016_PR_processed.RDA")



#Do a simple education attainment by sex - should aggregate by age next
noc.demo.educ.prim <- noc.demo.educ[ GEO.NAME=="Canada"& CIP82.ID==1,.(sum(TOT.EDUC),sum(TOT.NO),sum(TOT.SEC),sum(TOT.APP),sum(TOT.COL),sum(TOT.BEBA),
                                                                       sum(TOT.BA),sum(TOT.ABBA)),by=.(tech,SEX3)] #Get the primary facts table


names(noc.demo.educ.prim) <- c("tech","SEX","Total","No Degree","Secondary School","Apperenticeship and Trade Schools","College, CEGEP","University Degree Below Bachelors",
                               "Bachelors","Above Bachelors") #Change the column names - prepare to melt
noc.demo.educ.prim <- melt(noc.demo.educ.prim,id.vars = c("tech","SEX"),variable.name = "Education",value.name="Count") #Melt so Education is a single column

noc.demo.educ.prim[,total:=max(Count),by=.(tech,SEX)] #Calculate the max
noc.demo.educ.prim[,pct:=Count/total*100] #Calculate the percentage


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


####################################
#Pay by educational level in Technology - include Regression stuff
noc.educ.pay.tech <- noc.dem.master[GEO.NAME == "Canada" & IM.STATUS.ID == 1 & WA.ID == 1 & AGE5.ID == 1 & SEX3.ID == 1,.(sum(TOT),wt.mean(AVG.INC,TOT)),by=.(tech,EDUC,EDUC.ID)]
noc.educ.pay.tech.decomp <- noc.dem.master[GEO.NAME == "Canada" & IM.STATUS.ID == 1 & WA.ID == 1 & AGE5.ID == 1,.(sum(TOT),wt.mean(AVG.INC,TOT)),by=.(tech,EDUC,EDUC.ID,SEX3,SEX3.ID)]
noc.educ.pay.tech.decomp <- noc.educ.pay.tech.decomp[EDUC != "Total - Highest certificate, diploma or degree" & SEX3 != "Total - Sex"]
noc.educ.pay.tech.decomp <- noc.educ.pay.tech.decomp[tech==1]
noc.educ.pay.tech.decomp[,educ.dum:=0]
noc.educ.pay.tech.decomp[EDUC=="University certificate, diploma or degree at bachelor level or above",educ.dum:=1]
noc.educ.pay.tech.decomp <- noc.educ.pay.tech.decomp[,wt.mean(V2,V1),by=.(SEX3,SEX3.ID,educ.dum)]
noc.educ.pay.tech.decomp[,SEX3.ID:=SEX3.ID-2]


noc.dem.master[,educ.dum:=0]
noc.dem.master[EDUC=="University certificate, diploma or degree at bachelor level or above",educ.dum:=1]
noc.educ.pay.tech.decomp <- noc.dem.master[GEO.NAME == "Canada" & IM.STATUS.ID == 1 & WA.ID == 1 & AGE5.ID == 1 & EDUC.ID != 1,
                                           .(sum(TOT),wt.mean(AVG.INC,TOT),wt.sd(AVG.INC,TOT)),
                                           by=.(tech,educ.dum,SEX3,SEX3.ID)]



###################################
#Detailed CIP
load("NOC_Demographics/noc_cip_2016_processed.RDA")

################
#Gephi stuff
noc.cip.2016.gephi <- noc.cip.2016[EDUC.ID == 1 & AGE4.ID == 1]
noc.cip.2016.gephi[,CIP432:=str_sub(CIP432,6)]
noc.cip.2016.gephi[,OCC693:=str_sub(OCC693,5)]

gephi.noc.nodes <- unique(noc.cip.2016.gephi[TOT>=100,.(OCC693,tech)])
gephi.cip.nodes <- unique(noc.cip.2016.gephi[TOT>=100,CIP432])
noc.cip.2016.gephi[,tot.cip:=sum(TOT),by=CIP432]
noc.cip.2016.gephi[,pct:=100*TOT/tot.cip]
noc.cip.2016.gephi[,c("TOT.MALE","TOT.FEMALE"):=NULL]
noc.cip.2016.gephi[,c("EDUC","EDUC.ID","CIP432.ID","AGE4","AGE4.ID","OCC693.ID"):=NULL]
noc.cip.2016.gephi[,c("noc","cip","tech","digital","high.tech"):=NULL]
names(noc.cip.2016.gephi) <- c("Source","Target","TOT","TOT.CIP","Weight")
noc.cip.2016.gephi <- noc.cip.2016.gephi[TOT>100]


write.csv(noc.cip.2016.gephi,file="Gephi/noc_cip_2016_gephi.csv",row.names = FALSE)



gephi.cip.nodes <- data.table(CIP432=gephi.cip.nodes,type="Major Concentration")
names(gephi.cip.nodes) <- c("Id","Type")
names(gephi.noc.nodes) <- c("Id","Type")
gephi.cip.noc.nodes <- rbindlist(list(gephi.cip.nodes,gephi.noc.nodes))
write.csv(gephi.cip.noc.nodes,file="Gephi/nodes_noc_cip.csv",row.names = FALSE)



############

pay.premium.by.educ <- plot.change.arrow.bf(noc.educ.pay.tech[EDUC.ID != 1],"V2",
                                            cat="EDUC",
                                            time.var = "tech", 
                                            unit.x="$",
                                            plot.title = "Pay Premium in Tech by Educational Groups",
                                            plot.fig.num="Figure x",
                                            caption="Source: 2016 Canadian Census")


educ.graph <- plot.column.bf(noc.demo.educ.prim[SEX=="Total - Sex" & Education!="Total"],
                             "pct","tech",stacked = TRUE,group.by = "Education",
                             label.unit = "%",plot.title = "Educational Composition of Tech Occupations",
                             plot.fig.num = "Figure X",caption = "Source: 2016 Canadian Census, BII+E Analysis") #General educational stacked bar

educ.graph.sex <- plot.column.bf(noc.demo.educ.prim[SEX != "Total - Sex" & tech=="Tech Occupation" & Education != "Total"],
                                 "pct","SEX",stacked = TRUE, group.by = "Education",
                                 label.unit = "%",
                                 plot.title = "Educational Composition by Sex - Technology Occupations",
                                 plot.fig.num = "Figure X", caption = "Source: 2016 Canadian Census, BII+E Analysis") #By Sex educational stacked bar



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

