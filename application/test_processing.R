test_1 <- fread("flow_stocks_fixed_non_residential_capital.csv")
test_1 <- test_1[CATEGORY=="Investment"]

test_1[,COMPONENT:=str_replace(COMPONENT," \\(x 1,000,000\\)","")]
test_1[,Vector:=NULL]
test_1[,Coordinate:=NULL]
test_1[,Value:=str_replace(Value,"\\.\\.","NA")]
test_1[,Value:=as.numeric(Value)]
test_5 <- dcast(test_1, Ref_Date + GEO + PRICES + NAICS ~ COMPONENT, value.var="Value")
test_5[,Software:=as.numeric(Software)]

require(plotly)
require(data.table)
test_5 <- as.data.table(test_5)
p <- plot_ly(test_5[PRICES=="2007 constant prices"], y=~Software,x=~Ref_Date, type="scatter",mode="lines", color=~NAICS)

p_2 <- plot_ly(test_5[PRICES=="2007 constant prices" & NAICS !="Total all industries"],y=~Software, x=~`Computer and electronic products`,type="scatter", color=~NAICS)
summary(lm(Software ~ `Computer and electronic products`,data=test_5[PRICES=="2007 constant prices"]))
