
library(data.table)
library(extrafont)
library(stringr)
library(SDMTools)
library(BFTheme)

JVWS <- fread("14100356.csv")
tech.occ <- fread("tech.sector.def.csv") #Tech occupation in 2016

names(JVWS) <- c("REG_DATE","GEO","DGUID","NOC","STAT","UOM",
                 "UOM_ID","SCALAR","SCALAR_ID","VECTOR","COORDINATE",
                 "VALUE","STATUS","SYMBOL","TERMINATED","DECIMALS")

JVWS[,Year:=str_sub(REG_DATE,1,4)]
JVWS <- JVWS[GEO=="Canada"]

JVWS_NOC4 <- JVWS[NOC %in% tech.occ[,str_sub(noc_title,6)]]

JVWS_NOC4[,tech:="Non-tech Occupations"]
JVWS_NOC4[NOC %in% tech.occ[tech==1,str_sub(noc_title,6)],tech:="Tech Occupations"]
JVWS_NOC4[,VALUE:=as.numeric(VALUE)]
JVWS_NOC4[,c("UOM","UOM_ID","SCALAR","SCALAR_ID","VECTOR",
             "COORDINATE","STATUS","SYMBOL","TERMINATED","DECIMALS",
             "GEO","DGUID"):=NULL]
JVWS_NOC4_w <- reshape(JVWS_NOC4, direction = "wide", v.names = "VALUE",timevar = "STAT",
                     idvar = c("REG_DATE","NOC","Year","tech"))
names(JVWS_NOC4_w) <- c("REG_DATE","NOC","Year","tech","Vac","AVG_WAGE")
JVWS_NOC4_consolidated <- JVWS_NOC4_w[,wt.mean(AVG_WAGE,Vac),by=.(REG_DATE,tech)]
JVWS_NOC4_consolidated <- JVWS_NOC4_consolidated[3:26]

plot.wages.JVWS <- plot.line.bf(JVWS_NOC4_consolidated,
                           "REG_DATE","V1",
                           group.by = "tech",
                           show.points = FALSE)


JVWS_NOC4_tech <- JVWS_NOC4_w[tech=="Tech Occupations"]
JVWS_NOC4_tech[,digital:="High Tech Occupations"]
JVWS_NOC4_tech[NOC %in% tech.occ[digital=="Digital",str_sub(noc_title,6)],digital:="Digital Occupations"]
JVWS_NOC4_tech_consolidated <- JVWS_NOC4_tech[,wt.mean(AVG_WAGE,Vac),by=.(REG_DATE,digital)]
JVWS_NOC4_tech_consolidated <- JVWS_NOC4_tech_consolidated[3:26]

JVWS_NOC4_tech_count <- JVWS_NOC4_tech[,sum(Vac,na.rm=TRUE),by=.(REG_DATE,digital)]
JVWS_NOC4_tech_count <- JVWS_NOC4_tech_count[3:26]
JVWS_NOC4_tech_count[,ln.vac:=log(V1)]

plot.wages.JVWS.dig.num <- plot.line.bf(JVWS_NOC4_tech_count,
                                        "REG_DATE","ln.vac",
                                        group.by="digital",
                                        show.points = FALSE)

plot.wages.JVWS.dig <- plot.line.bf(JVWS_NOC4_tech_consolidated,
                                "REG_DATE","V1",
                                group.by = "digital",
                                show.points = FALSE)

JVWS_wages_tech <- JVWS_NOC4[,wt.mean()]






