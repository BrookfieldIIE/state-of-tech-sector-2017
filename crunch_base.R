#####################
# Crunchbase data anlysis
# Author: Viet Vu
# Email: viet.vu@ryerson.ca
#####################



library(data.table)
library(httr)
library(stringr)
library(jsonlite)

#API Key - expires on June 15th, 2018. Research Account
user_key <- "6f9d9be70efda6d28e21c5fadd3f541b"


#Load in all data pulled from daily export December 18th, 2017
cb_acqui <- fread("csv_export/acquisitions.csv")
cb_catgroup <- fread("csv_export/category_groups.csv")
cb_degrees <- fread("csv_export/degrees.csv")
cb_eventapp <- fread("csv_export/event_appearances.csv")
cb_event <- fread("csv_export/events.csv")
cb_fundround <- fread("csv_export/funding_rounds.csv")
cb_funds <- fread("csv_export/funds.csv")
cb_invpart <- fread("csv_export/investment_partners.csv")
cb_inv <- fread("csv_export/investors.csv")
cb_ipos <- fread("csv_export/ipos.csv")
cb_jobs <- fread("csv_export/jobs.csv")
cb_orgpar <- fread("csv_export/org_parents.csv")
cb_org <- fread("csv_export/organizations.csv")
cb_people <- fread("csv_export/people.csv")

#Start processing data from the organizations based in Canada
cb_org_can <- cb_org[country_code=="CAN"]
cb_org_can_uuid <- cb_org_can[,uuid]
setkey(cb_org_can,"uuid")
setkey(cb_jobs,"org_uuid")
cb_org_can_uuid <- data.table(uuid=cb_org_can_uuid)
setkey(cb_org_can_uuid,"uuid")

#Isolate people who works/worked with organizations in Canada
cb_org_can_people <- cb_jobs[cb_org_can_uuid, nomatch=0]

cb_org_can_people_uuid <- data.table(cb_org_can_people[,"person_uuid"])

setkey(cb_eventapp,"participant_uuid")
setkey(cb_org_can_people_uuid,"person_uuid")

#Isolate people who works/worked with organizations in Canada that attended events
cb_event_can_people <- cb_eventapp[cb_org_can_people_uuid, nomatch=0]


#Main API function. Takes general path and key
cb_api <- function(path,key) {
  require(jsonlite) #Process JSON file into list
  require(data.table) #General data processing
  path <- str_c(path,"?locations=%27Canada%27", "&user_key=", key) #Form path and filter location to Canada. To be fix for more general case
  url <- str_c("https://api.crunchbase.com/v3.1/", path) #Forming URL for the pull
  resp <- GET(url) #Get response from API pull
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE) #Check to see if response is JSON, exit if not
  }
  json_data <- fromJSON(content(resp, "text"), simplifyVector = TRUE, flatten=TRUE) #Turn raw JSON into list
  tot_items <- json_data$data$paging$total_items #Get total number of items for reference
  if(tot_items<=100){
    data <- as.data.table(matrix(unlist(json_data$data$items), 
                                 nrow=tot_items, 
                                 dimnames = list(NULL,names(json_data$data$items)))) #Turn all observations into table
    return(data) #Return data if it only has one page or less
  }
  else{
    data <- as.data.table(matrix(unlist(json_data$data$items), 
                               nrow=100, 
                               dimnames = list(NULL,names(json_data$data$items)))) #Turn the observations (first 100) into a data table
  }
  num_pages <- json_data$data$paging$number_of_pages #Get number of pages for pagination
  for(n in seq(2, num_pages)){ #Sequence from second page to last page
    url <- json_data$data$paging$next_page_url #Get the url for the next page
    url <- str_c(url, "&user_key=",key) #Add userkey to the url
    resp <- GET(url) #Get response from API pull
    json_data <- fromJSON(content(resp, "text"), simplifyVector = TRUE, flatten=TRUE) #Turn data into JSON
    if(n==num_pages){ #Check if it's at last page
      mat_row <- tot_items-(n-1)*100 #Get the right number of observations for the matrix
      temp_data <- as.data.table(matrix(unlist(json_data$data$items), 
                                        nrow=mat_row, 
                                        dimnames = list(NULL,names(json_data$data$items)))) #Turn into data frame (case where less than 100 observations)
    }
    else{
      temp_data <- as.data.table(matrix(unlist(json_data$data$items), 
                                        nrow=100, 
                                        dimnames = list(NULL,names(json_data$data$items)))) #Otherwise, turn 100 observations into frame and continue
    }
    data <- rbind(data,temp_data) #Add to the fdefault frame from page 1
  }
  return(data) #Return data after cycle terminates
}

org_api <- cb_api("organizations",user_key)