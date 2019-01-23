

library(data.table)
library(ggplot2)
library(BFTheme)
library(extrafont)
library(stringr)
library(SDMTools)






load("NOC_Demographics/noc_2016_PR_processed.RDA")


noc.2016.imm <- noc.dem.master[EDUC.ID==1 & WA.ID==1,.(sum(TOT),wt.mean(AVG.INC,TOT)),by=.(IM.STATUS,IM.STATUS.ID,SEX3,AGE5,tech)] #Consolidate the mainsummary stats table to work with
names(noc.2016.imm) <- c("IM.STATUS","IM.STATUS.ID") #Change names

noc.2016.imm <- noc.2016.imm[IM.STATUS %in% c("Non-immigrants","Immigrants")] #Only keep the columns for Immigrant vs non immigrants

noc.2016.imm.for.graph <- noc.2016.imm[SEX3 != "Total - Sex" & AGE5 == "Total - Age"] #only keep the total sex and prime age workers for now
noc.2016.imm.for.graph[,demo:=str_c(AGE5," ", IM.STATUS," ",SEX3)] #Combine Age and Sex to label the graph
noc.2016.imm.for.graph[,category:='Tech Occupation "Premium"'] #Set the label
noc.2016.imm.for.graph[tech==0,category:='Average Non-Tech Salary'] #Set the label for graph
noc.2016.imm.for.graph[,category:=reorder(category,-tech)] #Order by so that tech
noc.2016.imm.for.graph[,pay:=max(V2)-min(V2),by=demo] #Cal
noc.2016.imm.for.graph[tech==0,pay:=V2]

plot.column.bf(noc.2016.imm.for.graph,"pay","demo", 
               group.by = "category",
               stacked = TRUE,
               colours = set.colours(2,categorical.choice = c("pink","dark.blue")),
               label.unit = "$",
               plot.title = 'Technology "Premium" for Different Immigrant Groups',
               plot.fig.num = "Figure x",
               caption = "Source: 2006, 2016 Canadian Census") +
  coord_flip() +
  theme(axis.text.x = element_text(size=9, margin=ggplot2::margin(t=2), family = "RooneySans-Light",angle=0))

