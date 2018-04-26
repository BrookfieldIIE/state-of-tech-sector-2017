########################
#All the one-time misc graphics

library(data.table)
library(BFTheme)
library(extrafont)
library(stringr)



#########
#Digital Spillover stuff
spillover <- fread("misc/digital_spillover.csv",header=TRUE)
spillover <- melt(spillover,id.vars = "asset",value.name = "USDM",variable.name="Year")

spillover[,Year:=as.character(Year)]
spillover[,Year:=as.numeric(Year)]
spillover[,USDM:=str_remove_all(USDM,",")]
spillover[,USDM:=as.numeric(USDM)]

plot.spillover <- plot.line.bf(spillover[Year<2025],"Year","USDM",group.by="asset",
                               show.points = TRUE,
                               colours = set.colours(4,categorical.choice=c("light.blue","dark.blue","pink","magenta")),
                               plot.title = "Digital Spillover in Canada",
                               plot.fig.num = "Figure X",
                               unit.y = "$",
                               y.axis = "Million $ USD",
                               legend.title = "Asset Type",
                               caption = "Source: Oxford Economics & Huawei (2017)")
export.bf.plot("Graphics/spillover.plot",plot.spillover)






