library(BFTheme)
library(ggplot2)
library(data.table)
library(stringr)
library(extrafont)
library(ggrepel)

load("Graphs/noc.dem.sumstat.RDA")

setkey(noc.dem.sumstat,TOT)
main.top.10.plot <- plot.column.bf(noc.dem.sumstat[23:32],"TOT","NOC.TITLE",group.by = "digital.label",
                                   colours = set.colours(2,categorical.choice=c("light.blue","dark.blue")),
                                   label = TRUE,
                                   plot.title = "Top 10 Technology Occupations by Employment in Canada",
                                   plot.fig.num = "Figure 1",
                                   order.bar = "ascending",
                                   caption = "Source: 2016 Canadian Census")



load("Graphs/pct.change.noc.RDA")
pct.change.noc.graph <- plot.column.bf(pct.change.noc,x="pct_change",
                                       cat = "occ", order.bar = "ascending",
                                       plot.title = "Change in Share of employment of different occupational groups",
                                       plot.fig.num = "Figure 2",
                                       caption = "Source: 2006, 2016 Canadian Census, BII+E Analysis") +
  scale_y_continuous(limits = c(-50,75),breaks = c(-50,-25,0,25,50,75),labels = c("-50%","-25%","0%","25%","50%","75%"))



load("Graphs/tech.cops.RDA")
tech.cops.abs <- plot.line.bf(tech.cops, x = "Year", y = "Emp", group.by = "digital",
                              caption = "Source: Canadian Occupational Projection System (COPS)",
                              plot.title = "Projected Employment Growth for Tech Occupations: 2016-2026",
                              plot.fig.num = "Figure 3",
                              colours = set.colours(2,categorical.choice = c("light.blue","dark.blue")),
                              x.axis = "Year",
                              y.axis = "Employment",
                              show.points = FALSE) + 
  theme(axis.text.x = element_text(size=9, margin=ggplot2::margin(t=2), family = "RooneySans-Light",angle=0)) +
  scale_y_continuous(limits=c(0,NA), labels = scales::comma)



setkey(noc.dem.sumstat,AVG.INC)
main.top.10.inc.plot <- plot.column.bf(noc.dem.sumstat[23:32],"AVG.INC","NOC.TITLE",group.by = "digital.label",
                                       colours = set.colours(2,categorical.choice=c("light.blue","dark.blue")),
                                       label = TRUE,
                                       label.unit = "$",
                                       plot.title = "Top 10 Technology Occupations by Income in Canada",
                                       plot.fig.num = "Figure 4",
                                       order.bar = "ascending",
                                       caption = "Source: 2016 Canadian Census")


load("Graphs/noc.demo.educ.prim.RDA")
educ.graph <- plot.column.bf(noc.demo.educ.prim[SEX=="Total - Sex" & Education!="Total"],
                             "pct","tech",
                             stacked = TRUE,
                             group.by = "Education",
                             label.unit = "%",
                             plot.title = "Educational Composition of Tech Occupations",
                             plot.fig.num = "Figure 5",
                             colours = set.colours(7,categorical.choice = c("green","orange","magenta",
                                                                            "yellow","pink","light.blue",
                                                                            "dark.blue")),
                             caption = "Source: 2016 Canadian Census, BII+E Analysis") +
  theme(axis.text.x = element_text(size=10,margin=margin(t=2),hjust=0.5,angle=0)) +
  guides(fill=guide_legend(title = "",ncol=2))#General educational stacked bar


load("Graphs/noc.naics.abs.RDA")
noc.naics.abs.plot <- plot.column.bf(noc.naics.abs, x = "Employed", cat = "NAICS23A", group.by = "digital", 
                                     stacked = TRUE,
                                     order.bar = "descending",
                                     y.axis = "Employment",
                                     colours = set.colours(2,categorical.choice = c("light.blue","dark.blue")),
                                     plot.title = "Employment of Tech Workers by Industry Groups",
                                     plot.fig.num = "Figure 6",
                                     caption = "Source: 2016 Canadian Census, BII+E Analysis")


load("Graphs/noc.naics.share.RDA")
noc.naics.share.graph <- plot.column.bf(noc.naics.share, x = "share", cat = "NAICS23A", group.by = "digital", 
                                  stacked = TRUE,
                                  order.bar = "descending",
                                  label.unit = "%",
                                  colours = set.colours(2,categorical.choice = c("light.blue","dark.blue")),
                                  plot.title = "Share of Tech Workers by Industry Groups",
                                  plot.fig.num = "Figure 7",
                                  y.axis = "Share of Industry Employment",
                                  caption = "Source: 2016 Canadian Census, BII+E Analysis")




load("Graphs/noc.dem.tech.map.top.pct.dig.RDA")

noc.dem.geo.relative <- plot.column.bf(noc.dem.tech.map.top.pct.dig[Year==2016],"pct","GEO.NAME",
                                       order.bar = "ascending",
                                       stacked = TRUE,
                                       group.by = "digital.lab",
                                       colours = set.colours(2,categorical.choice = c("light.blue","dark.blue")),
                                       label.unit = "%",
                                       plot.title = "Geographical Concentration (%) of Technology Occupations, 2016 Canada",
                                       plot.fig.num = "Figure 8",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

load("Graphs/noc.dem.tech.map.top.dig.RDA")

noc.dem.geo.absolute <- plot.column.bf(noc.dem.tech.map.top.dig,"V1","GEO.NAME",
                                       order.bar = "ascending",
                                       stacked = TRUE,
                                       group.by = "digital.lab",
                                       colours = set.colours(2,categorical.choice = c("light.blue","dark.blue")),
                                       plot.title = "Geographical Distribution of Technology Occupations, Canada",
                                       plot.fig.num = "Figure 9",
                                       caption = "Source: 2016 Canadian Census, BII+E Analysis")

load("Graphs/noc.dem.tech.map.top.RDA")
noc.dem.geo.absolute.change <- plot.change.arrow.bf(noc.dem.tech.map.top,"V1","GEO.NAME","Year",
                                                    plot.title = "10 Years Change in Absolute Number of Tech Workers by Canadian Cities",
                                                    plot.fig.num = "Figure 10",
                                                    annotate = TRUE,
                                                    nudge.beg = 0.03,
                                                    nudge.end = 0.035,
                                                    label = TRUE,
                                                    caption = "Source: 2016, 2006 Canadian Census") +
  scale_x_continuous(limits = c(0,300000), breaks = c(0,50000,100000,150000,200000,250000),labels = c("0","50,000","100,000","150,000","200,000","250,000"),expand = c(0.06,0.6))

load("Graphs/noc.dem.tech.map.top.pct.RDA")
noc.dem.geo.relative.change <- plot.change.arrow.bf(noc.dem.tech.map.top.pct[GEO.NAME!="Carleton Place"],"pct","GEO.NAME","Year",
                                                    plot.title = "10 Years Change in Relative Number of Tech Workers by Canadian Cities",
                                                    plot.fig.num = "Figure 11",
                                                    unit.x = "%",
                                                    caption = "Source: 2016, 2006 Canadian Census")


load("Graphs/age.sex.dot.RDA")
age.sex.dots <- ggplot() +
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
  scale_colour_manual(values = set.colours(2,categorical.choice = c("dark.blue","pink"))) +
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
  annotate("curve",
           x = -20.5, 
           y = 52.5,
           xend = -24,
           yend = 55,
           curvature = -0.5,
           colour = set.colours(1,categorical.choice = "pink")) +
  annotate("text",x = -24,
           y = 58,
           size=9*0.352777778,
           label = "This means that 4.2% of\n Male aged 55-64 years old \n work in a tech occupation",
           family = "RooneySans-Regular") +
  
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
  scale_x_continuous(limits = c(-27,27)) +
  labs(subtitle = "Employment in Tech Occupations by Age and Sex (Each dot represents 1,000 people)",
       title = "Figure 12",
       caption = "Source: 2016 Canadian Census, BII+E Analysis, \nNote: Each point represents 1,000 people")







educ.graph.sex <- plot.column.bf(noc.demo.educ.prim[SEX != "Total - Sex" & tech=="Tech Occupation" & Education != "Total"],
                                 "pct","SEX",stacked = TRUE, group.by = "Education",
                                 label.unit = "%",
                                 colours = set.colours(7, categorical.choice = c("green","orange","magenta","yellow","pink","light.blue","dark.blue")),
                                 plot.title = "Educational Composition by Sex - Technology Occupations",
                                 plot.fig.num = "Figure 13", caption = "Source: 2016 Canadian Census, BII+E Analysis") +
  theme(axis.text.x = element_text(size=10,margin=margin(t=2),hjust=0.5,angle=0)) +
  guides(fill=guide_legend(title="",ncol=2))#By Sex educational stacked bar


load("Graphs/mars.sots.RDA")

mars.sots[,Metric:=str_wrap(Metric, 30)]

overall.score.gen <- ggplot(data=mars.sots[Graph=="overall.score.gender"], aes(Metric, Score, group=Gender)) +
  brookfield.base.theme() +
  geom_col(aes(fill=Gender),position="dodge", width=0.7) +
  geom_text(aes(label=str_c(Score, Significance)), position=position_dodge(width=0.7),hjust=-0.25,family="RooneySans-Regular") +
  scale_fill_manual(values = set.colours(2,categorical.choice = c("yellow","dark.blue"))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5),breaks = c(0,1,2,3,4,5)) +
  labs(x="",
       y="Average Response Scores (1=Strongly Disagree; 5=Strongly agree)",
       subtitle = "Toronto Tech sector DIB Scores by Respondent Gender",
       title = "Figure 14",
       caption = "Source:MaRS Discovery District analysis using survey dataset powered by Fortay and Feminuity \nNote: *** denotes statistically different from men score at the 1% level; \n** at the 5% level; * at the 10% level; N = 425") +
  coord_flip() +
  guides(fill=guide_legend(reverse=TRUE))

belonging.score.gen <- ggplot(data=mars.sots[Graph=="belonging.score.gender"], aes(Metric, Score, group=Gender)) +
  brookfield.base.theme() +
  geom_col(aes(fill=Gender),position="dodge", width=0.7) +
  geom_text(aes(label=str_c(Score, Significance)), position=position_dodge(width=0.7),hjust=-0.25,family="RooneySans-Regular") +
  scale_fill_manual(values = set.colours(2,categorical.choice = c("yellow","dark.blue"))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5),breaks = c(0,1,2,3,4,5)) +
  labs(x="",
       y="Average Response Scores (1=Strongly Disagree; 5=Strongly agree)",
       subtitle = "Toronto Tech sector Belonging Scores by Respondent Gender",
       title = "Figure 15",
       caption = "Source:MaRS Discovery District analysis using survey dataset powered by Fortay and Feminuity \nNote: *** denotes statistically different from men score at the 1% level; \n** at the 5% level; * at the 10% level; N = 425") +
  coord_flip()+
  guides(fill=guide_legend(reverse=TRUE))

inclusion.score.gen <- ggplot(data=mars.sots[Graph=="inclusion.score.gender"], aes(Metric, Score, group=Gender)) +
  brookfield.base.theme() +
  geom_col(aes(fill=Gender),position="dodge", width=0.7) +
  geom_text(aes(label=str_c(Score, Significance)), position=position_dodge(width=0.7),hjust=-0.25,family="RooneySans-Regular") +
  scale_fill_manual(values = set.colours(2,categorical.choice = c("yellow","dark.blue"))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5),breaks = c(0,1,2,3,4,5)) +
  labs(x="",
       y="Average Response Scores (1=Strongly Disagree; 5=Strongly agree)",
       subtitle = "Toronto Tech sector Inclusion Scores by Respondent Gender",
       title = "Figure 16",
       caption = "Source:MaRS Discovery District analysis using survey dataset powered by Fortay and Feminuity \nNote: *** denotes statistically different from men score at the 1% level; \n** at the 5% level; * at the 10% level; N = 425") +
  coord_flip()+
  guides(fill=guide_legend(reverse=TRUE))


overall.score.vismin <- ggplot(data=mars.sots[Graph=="overall.score.vismin"], aes(Metric, Score, group=Gender)) +
  brookfield.base.theme() +
  geom_col(aes(fill=Gender),position="dodge", width=0.7) +
  geom_text(aes(label=str_c(Score, Significance)), position=position_dodge(width=0.7),hjust=-0.25,family="RooneySans-Regular") +
  scale_fill_manual(values = set.colours(4,categorical.choice = c("pink","dark.blue","yellow","magenta"))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5),breaks = c(0,1,2,3,4,5)) +
  labs(x="",
       y="Average Response Scores (1=Strongly Disagree; 5=Strongly agree)",
       subtitle = "Toronto Tech sector DIBS Scores by Respondent Race",
       title = "Figure 17",
       caption = "Source:MaRS Discovery District analysis using survey dataset powered by Fortay and Feminuity \nNote: *** denotes statistically different from men score at the 1% level; \n** at the 5% level; * at the 10% level; N = 425") +
  coord_flip()+
  guides(fill=guide_legend(reverse=TRUE))

inclusion.score.vismin <- ggplot(data=mars.sots[Graph=="inclusion.score.vismin"], aes(Metric, Score, group=Gender)) +
  brookfield.base.theme() +
  geom_col(aes(fill=Gender),position="dodge", width=0.7) +
  geom_text(aes(label=str_c(Score, Significance)), position=position_dodge(width=0.7),hjust=-0.25,family="RooneySans-Regular") +
  scale_fill_manual(values = set.colours(4,categorical.choice = c("pink","dark.blue","yellow","magenta"))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5),breaks = c(0,1,2,3,4,5)) +
  labs(x="",
       y="Average Response Scores (1=Strongly Disagree; 5=Strongly agree)",
       subtitle = "Toronto Tech sector Belonging Scores by Respondent Race",
       title = "Figure 18",
       caption = "Source:MaRS Discovery District analysis using survey dataset powered by Fortay and Feminuity \nNote: *** denotes statistically different from men score at the 1% level; \n** at the 5% level; * at the 10% level; N = 425") +
  coord_flip()+
  guides(fill=guide_legend(reverse=TRUE))


belonging.score.vismin <- ggplot(data=mars.sots[Graph=="belonging.score.vismin"], aes(Metric, Score, group=Gender)) +
  brookfield.base.theme() +
  geom_col(aes(fill=Gender),position="dodge", width=0.7) +
  geom_text(aes(label=str_c(Score, Significance)), position=position_dodge(width=0.7),hjust=-0.25,family="RooneySans-Regular") +
  scale_fill_manual(values = set.colours(4,categorical.choice = c("pink","dark.blue","yellow","magenta"))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5),breaks = c(0,1,2,3,4,5)) +
  labs(x="",
       y="Average Response Scores (1=Strongly Disagree; 5=Strongly agree)",
       subtitle = "Toronto Tech sector Belonging Scores by Respondent Race",
       title = "Figure 19",
       caption = "Source:MaRS Discovery District analysis using survey dataset powered by Fortay and Feminuity \nNote: *** denotes statistically different from men score at the 1% level;\n** at the 5% level; * at the 10% level; N = 425") +
  coord_flip()+
  guides(fill=guide_legend(reverse=TRUE))



diversity.score.vismin <- ggplot(data=mars.sots[Graph=="diversity.score.vismin"], aes(Metric, Score, group=Gender)) +
  brookfield.base.theme() +
  geom_col(aes(fill=Gender),position="dodge", width=0.7) +
  geom_text(aes(label=str_c(Score, Significance)), position=position_dodge(width=0.7),hjust=-0.25,family="RooneySans-Regular") +
  scale_fill_manual(values = set.colours(4,categorical.choice = c("pink","dark.blue","yellow","magenta"))) +
  scale_y_continuous(expand = c(0,0), limits=c(0,5),breaks = c(0,1,2,3,4,5)) +
  labs(x="",
       y="Average Response Scores (1=Strongly Disagree; 5=Strongly agree)",
       subtitle = "Toronto Tech sector Belonging Scores by Respondent Race",
       title = "Figure 20",
       caption = "Source:MaRS Discovery District analysis using survey dataset powered by Fortay and Feminuity \nNote: *** denotes statistically different from men score at the 1% level; \n** at the 5% level; * at the 10% level; N = 425") +
  coord_flip()+
  guides(fill=guide_legend(reverse=TRUE))



load("Graphs/noc.vis.2016.by.tech.gen.RDA")
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
                                plot.fig.num = "Figure 21",
                                caption = "Source: 2016 Canadian Census \nNote 1: Each point represents a Visible Minority - Sex pair \nNote 2:Drawn With 45 Degrees Line") + 
  geom_text_repel(family="RooneySans-Regular",aes(label = VIS15),force=2, segment.colour = set.colours(1,categorical.choice="grey"),alpha=0.7)


load("Graphs/noc.vis.2016.by.tech.gen.sum.RDA")
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
                                      plot.fig.num = "Figure 22",
                                      caption = "Source: 2016 Canadian Census \nNote: Each Point Represents a Visible Minority - Sex pair") +
  geom_text_repel(family="RooneySans-Regular",aes(label = VIS15),force=2, segment.colour=set.colours(1,categorical.choice="grey"), alpha=0.7)

load("Graphs/sensitivity.around.ranking.RDA")

tech.sensitivity <- plot.scatter.bf(sensitivity.around.ranking,"rank","num",
                                    y.axis = "Number of identified tech workers",
                                    x.axis = "Rank cut-off",
                                    plot.title = "How number of tech workers change by varying cut-off for tech",
                                    plot.fig.num = "Figure 23")






export.bf.plot("Graphs/Figure_1.pdf",main.top.10.plot,p.width=7.25,p.height=7)

export.bf.plot("Graphs/Figure_2.pdf",pct.change.noc.graph,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_3.pdf",tech.cops.abs,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_4.pdf",main.top.10.inc.plot,p.width=7.25,p.height=7)

export.bf.plot("Graphs/Figure_5.pdf",educ.graph,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_6.pdf",noc.naics.abs.plot,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_7.pdf",noc.naics.share.graph,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_8.pdf",noc.dem.geo.relative,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_9.pdf",noc.dem.geo.absolute,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_10.pdf",noc.dem.geo.absolute.change,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_11.pdf",noc.dem.geo.relative.change,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_12.pdf",age.sex.dots,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_13.pdf",educ.graph.sex,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_14.pdf",overall.score.gen,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_15.pdf",belonging.score.gen,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_16.pdf",inclusion.score.gen,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_17.pdf",overall.score.vismin,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_18.pdf",inclusion.score.vismin,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_19.pdf",belonging.score.vismin,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_20.pdf",diversity.score.vismin,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_21.pdf",plot.vis.gen,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_22.pdf",participation.plot,p.width=7.25,p.height=6)

export.bf.plot("Graphs/Figure_23.pdf",tech.sensitivity,p.width=7.25,p.height=6)


