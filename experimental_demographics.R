library(data.table)
library(BFTheme)
library(extrafont)
library(ggplot2)


load("NOC_Demographics/NOC_demo.RDA") #Load in noc demographic file
names(noc.dem) <- c("CENSUS.YEAR","GEO.CODE","GEO.LEVEL","GEO.NAME","GNR","DATA.QUAL.FLAG","ALT.GEO.CODE","LF.STATUS","LF.STATUS.ID",
                    "LF.STATUS.NOTE","AGE","AGE.ID","AGE.NOTE","SEX","SEX.ID","SEX.NOTE","NOC","NOC.ID","NOC.NOTE","TOT","WORKER.NA","TOT.WORKER",
                    "TOT.EMP","TOT.SLF.EMP") #Change the names of the columns

noc.dem <- noc.dem[TOT>0] #Remove everything that has 0 count for the total number
noc.dem[,NOC.NUM:=tstrsplit(NOC," ",keep=1)] #Extract the NOC codes
noc.dem <- noc.dem[nchar(NOC.NUM)==4] #Filter for everything but 4-level NOCs
noc.dem[,NOC.NUM:=NULL] #Set NOC to NULL
#At this point, should have 2,293,895 rows. Check if that's not the case

age.10 <- c(2,6,9,10,11,12,13) #Define the age category

noc.dem[,tech:=0] #Set the tech flag vs non tech flag
noc.dem[NOC %in% tech.occ[,noc_title],tech:=1] #Set the tech flag

noc.dem <- noc.dem[AGE.ID %in% age.10]
noc.dem <- noc.dem[SEX.ID != 1]
noc.dem <- noc.dem[LF.STATUS.ID==1]
noc.dem <- noc.dem[GEO.NAME=="Canada"]

noc.dem[,AGE.NEW:=AGE]
noc.dem[AGE=="65 to 74 years",AGE.NEW:="64 years and over"]
noc.dem[AGE=="75 years and over",AGE.NEW:="64 years and over"]

noc.dem.cell <- noc.dem[,sum(TOT.WORKER),by=.(tech,AGE.NEW,SEX)]

noc.dem.cell[,Share:=100*min(V1)/sum(V1),by=.(AGE.NEW,SEX)]

noc.dem.cell[,dot:=V1/1000]


first.set <- generate.dot(121,nrow=10,starting.coord=c(2,0),dual.colour.break = 1,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
second.set <- generate.dot(184,nrow=10,starting.coord=c(2,12),dual.colour.break = 4,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
third.set <- generate.dot(187,nrow=10,starting.coord=c(2,24),dual.colour.break = 4,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
fourth.set <- generate.dot(205,nrow=10,starting.coord=c(2,36),dual.colour.break = 3,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
fifth.set <- generate.dot(144,nrow=10,starting.coord=c(2,48),dual.colour.break = 1,colour.title = c("In Tech Occupation","Not in Tech Occupation"))

sixth.set <- generate.dot(125,nrow=10,direction = "left",starting.coord=c(-2,0), dual.colour.break = 3,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
seventh.set <- generate.dot(199,nrow=10,direction = "left",starting.coord=c(-2,12), dual.colour.break = 15,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
eigth.set <- generate.dot(198,nrow=10,direction = "left",starting.coord=c(-2,24), dual.colour.break = 15,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
ninth.set <- generate.dot(214,nrow=10,direction = "left",starting.coord=c(-2,36), dual.colour.break = 13,colour.title = c("In Tech Occupation","Not in Tech Occupation"))
tenth.set <- generate.dot(167,nrow=10,direction = "left",starting.coord=c(-2,48), dual.colour.break = 7,colour.title = c("In Tech Occupation","Not in Tech Occupation"))

test.plot <- ggplot() +
  brookfield.base.theme() +
  geom_point(data=first.set,aes(x,y,colour = colour)) +
  geom_point(data=second.set,aes(x,y,colour = colour)) +
  geom_point(data=third.set,aes(x,y,colour = colour)) +
  geom_point(data=fourth.set,aes(x,y,colour = colour)) +
  geom_point(data=fifth.set,aes(x,y,colour = colour)) +
  geom_point(data=sixth.set,aes(x,y,colour = colour)) +
  geom_point(data=seventh.set,aes(x,y,colour = colour)) +
  geom_point(data=eigth.set,aes(x,y,colour = colour)) +
  geom_point(data=ninth.set,aes(x,y,colour = colour)) +
  geom_point(data=tenth.set,aes(x,y,colour = colour)) +
  scale_colour_manual(values = set.colours(2,categorical.choice = c("pink","dark.blue"))) +
  annotate("text", x = 0, y = 5, label = "15-24",size = 3, family = "RooneySans-Regular") +
  annotate("text", x = 0, y = 17, label = "25-34",size = 3, family = "RooneySans-Regular") +
  annotate("text", x = 0, y = 29, label = "35-44",size = 3, family = "RooneySans-Regular") +
  annotate("text", x = 0, y = 41, label = "45-54",size = 3, family = "RooneySans-Regular") +
  annotate("text", x = 0, y = 53 , label = "55-64",size = 3, family = "RooneySans-Regular") +
  
  annotate("text", x = 0, y = 58, label = "Age",size = 3, family = "RooneySans-Regular") +
  annotate("text", x = -7, y = 60, label = "Male",size = 4, family = "RooneySans-Regular") +
  annotate("text", x = 7, y = 60, label = "Female",size = 4, family = "RooneySans-Regular") +
  
  annotate("text", x = 15.5, y = 5, label = "0.8%", family = "RooneySans-Regular") +
  annotate("text", x = 21.5, y = 17, label = "2.2%", family = "RooneySans-Regular") +
  annotate("text", x = 21.5, y = 29, label = "2.1%", family = "RooneySans-Regular") +
  annotate("text", x = 23.5, y = 41, size = 4, family = "RooneySans-Regular",label = "1.5%") +
  annotate("text", x = 17.5, y = 53, size = 4, family = "RooneySans-Regular", label = "0.7%") +
  
  annotate("text", x = -15.5, y = 5, label = "2.4%", family = "RooneySans-Regular") +
  annotate("text", x = -22.5, y = 17, label = "7.5%", family = "RooneySans-Regular") +
  annotate("text", x = -22.5, y = 29, label = "7.6%", family = "RooneySans-Regular") +
  annotate("text", x = -24.5, y = 41, label = "6.1%", family = "RooneySans-Regular") +
  annotate("text", x = -19.5, y = 53, label = "4.2%", family = "RooneySans-Regular") +
  
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size=4/0.352777778)) +
  guides(colour = guide_legend(title = "")) +
  labs(subtitle = "Employment in Tech Occupations by Age and Sex (Each dot is 1,000 people)",title = "Figure X",caption = "Source: 2016 Canadian Census, BII+E Analysis, \nNote: Each point represents 1,000 people")


