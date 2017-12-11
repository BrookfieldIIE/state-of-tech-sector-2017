library(data.table)
library(stringr)


######
#Labour Productivity data
######

lab_productivity <- fread("lab_productivity.csv")

lab_productivity[,Coordinate:=NULL]
lab_productivity[,Vector:=NULL]
lab_productivity[Value == "..", Value:=NA]
lab_productivity[,Value:=as.numeric(Value)]

lab_productivity <- dcast(lab_productivity, Ref_Date + GEO + NAICS ~ ESTIMATES, value.var="Value", fun.aggregate=mean)

names(lab_productivity) <- c("Year","Geo","NAICS","avg_hrs",
                             "tot_hrs","lab_prod_2007","lab_share","nom_vadd",
                             "real_vadd_2007","tot_comp","avg_hr_comp","tot_jobs",
                             "unit_lab_cost","unit_lab_cost_us")


job_vacancy <- fread("job_vacancy_job_survey.csv")

job_vacancy[,Coordinate:=NULL]
job_vacancy[,Vector:=NULL]
job_vacancy[Value == "..", Value:=NA]
job_vacancy[Value == "F", Value:=NA]
job_vacancy[Value == "x", Value:=NA]
job_vacancy[,Value:=as.numeric(Value)]
job_vacancy <- dcast(job_vacancy, Ref_Date + GEO + NAICS + NOC3 ~ STATS, value.var="Value",fun.aggregate=mean)