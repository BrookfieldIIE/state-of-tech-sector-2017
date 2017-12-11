library(data.table)

cb_org <- fread("cb_org.csv")
cb_people <- fread("cb_people.csv")
cb_org <- cb_org[location_country_code=="CAN"]
cb_people <- cb_people[location_country_code=="CAN"]