#######################
#Processing and defining tech sector occupations
#This will end up generating an individual ranking file that is accessible by other code that has a tech occupation in it.



library(data.table)
library(ggplot2)
library(ggthemes)
library(stringr)
library(openxlsx)
library(BFTheme)
library(extrafont)
library(psych)

tech.cut.off <- 21 #Define the tech cut off here



crosswalk <- fread("NOC/onetnoc.csv") #Import Crosswalk file
knowledge <- read.xlsx("NOC/Knowledge.xlsx") #Import knowledge file
skill <- read.xlsx("NOC/Skills.xlsx") #Import skill file
work.activity <- read.xlsx("NOC/Work Activities.xlsx") #Import work activity file
work.style <- read.xlsx("NOC/Work Styles.xlsx") #Import work style file

#Combine skill knowledge and work activity in one thing
knowledge <- as.data.table(knowledge)
skill <- as.data.table(skill)
work.activity <- as.data.table(work.activity)
full.skill <- rbindlist(list(knowledge,skill,work.activity))
names(full.skill) <- c("onet","title","element.id","element.name","scale.id","scale.name","value","N","stder","lci","uci","sup","nr","date","source")
full.skill[,c("N","date","source","lci","uci"):=NULL]

#Use the crosswalk to crosswalk occupations over
setkey(crosswalk,onet)
setkey(full.skill,onet)
full.crosswalk.skill <- crosswalk[full.skill,nomatch=0]
full.avg.crosswalk.skill <- full.crosswalk.skill[,mean(value),by=.(noc_title,element.id,element.name,scale.id,scale.name)]
rm(full.crosswalk.skill) #Remove redundancies
rm(full.skill)
rm(work.activity,skill,knowledge)



#full.avg.crosswalk.skill[,tech.dum:=0] #Use full tech skill definition to define tech dummy
#for(n in seq(1,7)){
#  full.avg.crosswalk.skill[scale.id=="LV" & V1 >= n & element.id %in% unique(tech.skill.dic[anc.val==n & tech.dum=="Yes",element.id]),tech.dum:=1]
#}

#This makes this into a wide table, for explanatory purposes and not production run
#full.avg.crosswalk.skill.w <- reshape(full.avg.crosswalk.skill[scale.id=="LV"],v.names = c("V1"),timevar="element.id", idvar="noc_title",direction="wide",drop=c("element.name","tech.dum"))


#Set tech values only using core tech skills
#full.avg.crosswalk.skill[,tech.dum.2:=0]
#full.avg.crosswalk.skill[element.id %in% c("2.B.3.e","2.B.3.b","2.C.3.a","4.A.3.b.1","2.C.3.b","2.C.9.a"),tech.dum.2:=1]
#full.avg.crosswalk.skill[,prod:=prod(V1),by=.(noc_title,element.id)] #Multiply importance and levels

#noc.tech.score <- full.avg.crosswalk.skill[tech.dum.2==1 & scale.id=="IM",prod(prod),by=noc_title] #Add the product by occupations

#Transform work style and crosswalk over
work.style <- as.data.table(work.style)
names(work.style) <- c("onet","title","element.id","element.name","scale.id","scale.name","value","N","stder","lci","uci","sup","date","source")
work.style[,c("N","date","source","lci","uci"):=NULL]

setkey(work.style,onet)
setkey(crosswalk,onet)
full.crosswalk.style <- crosswalk[work.style,nomatch=0]
full.avg.crosswalk.style <- full.crosswalk.style[,mean(value),by=.(noc_title,element.id,element.name,scale.id,scale.name)]
rm(work.style,full.crosswalk.style)

full.avg.crosswalk.style.innovation <- full.avg.crosswalk.style[element.id=="1.C.7.a"]
full.avg.crosswalk.style.innovation[,c("element.id","element.name","scale.id","scale.name"):=NULL]
names(full.avg.crosswalk.style.innovation) <- c("noc_title","innovation")


#setkey(full.avg.crosswalk.style.innovation,noc_title)
#setkey(noc.tech.score,noc_title)
#noc.tech.score <- noc.tech.score[full.avg.crosswalk.style.innovation,nomatch=0]

#Set tech values using science skills
#full.avg.crosswalk.skill[,tech.dum.2:=0]
#full.avg.crosswalk.skill[element.id %in% c("2.C.4.a","2.C.4.b","2.C.4.c","2.C.4.d"),tech.dum.2:=1]
#full.avg.crosswalk.skill[,prod:=prod(V1),by=.(noc_title,element.id)] #Multiply importance and levels

#noc.science.score <- full.avg.crosswalk.skill[tech.dum.2==1 & scale.id=="IM",sum(prod),by=noc_title] #Add the product by occupations
#names(noc.science.score) <- c("noc_title","science")

#setkey(noc.science.score,noc_title)
#noc.tech.score <- noc.tech.score[noc.science.score,nomatch=0]

#noc.tech.score[,RD:=V1*innovation]
#noc.tech.score[,STEM:=V1+science]

#noc.tech.score[,tech:="Not Tech"] #Define tech
#noc.tech.score[STEM>=89 & innovation >= 3.39,tech:="Tech"] #Define not tech

#noc.tech.score[,rank.tech:=frankv(V1,order=-1)]
#noc.tech.score[,rank.science:=frankv(science,order=-1)]

#for(n in seq(1,484)){
#  noc.tech.score[n,harm.rank:=harmonic.mean(c(rank.tech,rank.science))]
#}



#Individual Ranking method
tech.skills <- c("2.C.4.a","2.C.4.b","2.C.4.c","2.C.4.d","2.B.3.e","2.B.3.b","2.C.3.a","4.A.3.b.1","2.C.3.b","2.C.9.a")

#Get all the raw points for each skills
individual.ranking <- full.avg.crosswalk.skill[element.id %in% tech.skills,prod(V1),by=.(noc_title,element.id)]
individual.ranking <- reshape(individual.ranking,direction="wide",v.names = c("V1"),timevar="element.id",idvar="noc_title")

#Merge all the tech skill raw score with innovation score
setkey(individual.ranking,noc_title)
setkey(full.avg.crosswalk.style.innovation,noc_title)
individual.ranking <- individual.ranking[full.avg.crosswalk.style.innovation,nomatch=0]

#Rank for all the tech skills
for(n in c(tech.skills)){
  individual.ranking[,str_c("rank.",n):=frankv(get(str_c("V1.",n)),order=-1)]
}

#Rank for innovation work style
individual.ranking[,rank.V1.1.C.7.a:=frankv(innovation,order=-1)]

#Calculate the regular harmonic mean
for(n in seq(1,484)){
  individual.ranking[n,harm.rank:=harmonic.mean(c(rank.2.C.4.a,rank.2.C.4.b,rank.2.C.4.c,
                                            rank.2.C.4.d,rank.2.B.3.b,rank.2.B.3.e,
                                            rank.2.C.3.a,rank.4.A.3.b.1,rank.2.C.3.b,rank.2.C.9.a))]
}

#Calculate the harmonic mean for tech skills only
for(n in seq(1,484)){
  individual.ranking[n,harm.rank.tech:=harmonic.mean(c(rank.2.B.3.b+1,rank.2.B.3.e+1,
                                                  rank.2.C.3.a+1,rank.4.A.3.b.1+1,rank.2.C.3.b+1,rank.2.C.9.a+1))]
}

#Define the technology sector
individual.ranking[,tech:=0]
individual.ranking[harm.rank < tech.cut.off, tech:=1]


#Write the CSV out
write.csv(individual.ranking,"tech.sector.def.csv",row.names=FALSE)

#Clean up the environment for the next file
rm(crosswalk,full.avg.crosswalk.skill,full.avg.crosswalk.style,full.avg.crosswalk.style.innovation,individual.ranking)




test.plot <- ggplot(mtcars,aes(mpg,cyl)) +
  geom_point() +
  theme(axis.title.x = element_text(family="RooneySans Regular"))

