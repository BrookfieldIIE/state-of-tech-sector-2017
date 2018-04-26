#######################################
#Combine different CANSIM tables      #
#Version 0.5                          #
#For this version, combine using 4    #
#Criteria: GEO, Ref_year, NAICS       #
#And Prices                           #
#Column names have to be Ref_Date GEO #
#PRICES VALUE                         #
#######################################



require(data.table)
require(stringr)

source("test_function.R")



combine_two_cansim_tables <- function(table_1,table_2){
  require(data.table)
  table_1 <- fread(str_c("/data/",table_1,".csv",sep=""))
  table_2 <- fread(str_c("/data/",table_2,".csv",sep=""))
  setkey(table_1,c("GEO","Ref_year","NAICS","PRICES"))
  setkey(table_2,c("GEO","Ref_year","NAICS","PRICES"))
  return(table_1[table_2,no_match=0])
}