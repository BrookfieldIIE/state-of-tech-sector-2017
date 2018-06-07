library(data.table)
library(ggplot2)
library(extrafont)
library(SDMTools)
library(BFTheme)
library(stringr)
library(ggrepel)

load("NOC_Demographics/noc_abo_processed.RDA") #Load the data for Aboriginal

load("NOC_Demographics/noc_2016_vis_processed.RDA")

noc.vis.2016.by.tech <- noc.vis.2016[,.(sum(TOT),wt.mean(AVG.INC,TOT)),by=.(VIS15,VIS15.ID,EDUC7,EDUC7.ID,WA4,WA4.ID,AGE4,AGE4.ID,SEX3,SEX3.ID,tech)]

noc.vis.2016.by.tech <- noc.vis.2016.by.tech[AGE4=="25 to 64 years"]


noc.vis.2016.by.tech.sumstat <- noc.vis.2016.by.tech[EDUC7.ID==1 & WA4.ID==1 & AGE4.ID==3 & SEX3.ID==1]

noc.vis.2016.by.tech.gen <- noc.vis.2016.by.tech[EDUC7.ID==1 & WA4.ID == 1 & AGE4.ID == 3 & SEX3.ID %in% c(2,3)]

noc.vis.2016.by.tech.gen.sum <- noc.vis.2016.by.tech.gen
noc.vis.2016.by.tech.gen.sum[,pct:=100*V1/sum(V1),by=.(SEX3,SEX3.ID,VIS15,VIS15.ID)]
noc.vis.2016.by.tech.gen <- reshape(noc.vis.2016.by.tech.gen,v.names="V2",idvar =c("VIS15","SEX3"),direction = "wide",timevar="tech")
noc.vis.2016.by.tech.gen[,V1:=NULL]
names(noc.vis.2016.by.tech.gen) <- c(names(noc.vis.2016.by.tech.gen)[1:11],"non.tech","tech")

participation.plot <- plot.scatter.bf(noc.vis.2016.by.tech.gen.sum[tech == "Tech Occupation" & VIS15 != "Total - Visible minority" &
                                                                     VIS15 != "Total visible minority population" &
                                                                     VIS15 != "Visible minority, n.i.e." &
                                                                     VIS15 != "Multiple visible minorities"],"V2","pct",group.by = "SEX3",
                                      x.axis = "Average pay in Tech Occupations",
                                      y.axis = "Participation Rate in Tech Occupations",
                                      unit.x = "$",
                                      plot.limit = c(0,105000,0,20),
                                      unit.y = "%",
                                      legend.title = "Sex",
                                      plot.title = "Pay and Participation by Visible Minority and Sex",
                                      plot.fig.num = "Figure x",
                                      caption = "Source: 2016 Canadian Census \nNote: Each Point Represents a Visible Minority - Sex pair")

participation.plot <- participation.plot + geom_text_repel(family="RooneySans-Regular",aes(label = VIS15),force=2)


plot.vis.gen <- plot.scatter.bf(noc.vis.2016.by.tech.gen[VIS15 != "Total - Visible minority" &
                                                           VIS15 != "Total visible minority population" &
                                                           VIS15 != "Visible minority, n.i.e." &
                                                           VIS15 != "Multiple visible minorities"],"non.tech","tech",
                                group.by="SEX3",
                                colours = set.colours(2,categorical.choice=c("light.blue","dark.blue")),
                                x.axis = "Average pay in Non-Tech Occupation",
                                y.axis = "Average pay in Tech Occupation",
                                unit.x = "$",
                                unit.y = "$",
                                plot.limit = c(0,85000,0,100000),
                                deg.45 = TRUE,
                                legend.title = "Sex",
                                plot.title = "Pay Difference between Tech and Non-Tech Occupation by Visible Minority and Sex",
                                plot.fig.num = "Figure x",
                                caption = "Source: 2016 Canadian Census \nNote 1: Each point represents a Visible Minority - Sex pair \nNote 2:Drawn With 45 Degrees Line")


plot.vis.gen <- plot.vis.gen + geom_text_repel(family="RooneySans-Regular",aes(label = VIS15),force=2, segment.colour = set.colours(1,categorical.choice="grey"),alpha=0.7)


#Aboriginal data
noc.abo.sumstat <- noc.abo[EDUC.ID == 1 & WA.ID == 1 & AGE4.ID == 3,.(sum(TOT),wt.mean(AVG.INC,TOT)),by=.(ABO,ABO.ID,SEX3,SEX3.ID,tech)]

noc.abo.sumstat.w <- noc.abo.sumstat
noc.abo.sumstat.w[,V1:=NULL]
noc.abo.sumstat.w <- reshape(noc.abo.sumstat.w,idvar=c("ABO","ABO.ID","SEX3","SEX3.ID"),timevar="tech",v.names = "V2",direction="wide")
noc.abo.sumstat.w <- noc.abo.sumstat.w[ABO.ID %in% c(4,5,6)]
noc.abo.sumstat.w <- noc.abo.sumstat.w[SEX3.ID != 1]
names(noc.abo.sumstat.w) <- c("ABO","ABO.ID","SEX3","SEX3.ID","non.tech","tech")
noc.abo.sumstat.w <- noc.abo.sumstat.w[non.tech>0]

plot.with.aboriginal <- plot.scatter.bf(noc.vis.2016.by.tech.gen[VIS15 != "Total - Visible minority" &
                                                                   VIS15 != "Total visible minority population" &
                                                                   VIS15 != "Visible minority, n.i.e." &
                                                                   VIS15 != "Multiple visible minorities"],"non.tech","tech",
                                        colours = set.colours(1,categorical.choice=c("grey")),
                                        x.axis = "Average pay in Non-Tech Occupation",
                                        y.axis = "Average pay in Tech Occupation",
                                        unit.x = "$",
                                        unit.y = "$",
                                        plot.limit = c(0,85000,0,100000),
                                        deg.45 = TRUE,
                                        plot.title = "Pay Difference between Tech and Non-Tech Occupation for Aboriginal Population Against Other Visible Minority",
                                        plot.fig.num = "Figure x",
                                        caption = "Source: 2016 Canadian Census \nNote 1: Each point represents a Visible Minority - Sex pair \nNote 2:Drawn With 45 Degrees Line")

plot.with.aboriginal <- plot.with.aboriginal + geom_point(data=noc.abo.sumstat.w,aes(non.tech,tech,colour=SEX3),size=2.3) +
  scale_colour_manual(values = set.colours(2,categorical.choice = c("dark.blue","light.blue"))) +
  geom_text_repel(data=noc.abo.sumstat.w,aes(label=ABO),family="RooneySans-Regular",force=2, segment.colour = set.colours(1,categorical.choice="grey"))
