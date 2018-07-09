library(dplyr)
library(parsedate)
library(lubridate)

## Script for arranging the ADL data taken from the UCI repository 
#(https://archive.ics.uci.edu/ml/datasets/Activities+of+Daily+Living+(ADLs)+Recognition+Using+Binary+Sensors)
## To run, source this script or call prepADL with the users you wish to arrange (default is both users A and B)
## This script automatically saves the output CSV to /data/ADL.csv

getUserData <- function(user, fixInputErrors = FALSE, path = "data/ADL"){
  activity <- read.table(paste0(path,"/Ordonez",user,"_ADLs.txt"),skip = 2) %>% 
    transmute(start = parsedate::parse_date(paste(V1,V2)),end = parsedate::parse_date(paste(V3,V4)),type = "Activity",label = V5) 
  

  
  sensors <- read.table(paste0(path,"/Ordonez",user,"_Sensors.txt"),skip = 2) %>% 
    transmute(start = parsedate::parse_date(paste(V1,V2)),end = parsedate::parse_date(paste(V3,V4)),type = "Sensor",label = V5)
  
  if(fixInputErrors){
    # Fix errorneous time input in 3 cases (end < start)
    day(activity$end) <- with(activity,ifelse(day(start) < day(end),day(end),day(start)))
    hour(activity$end) <- with(activity,ifelse(start < end,hour(end),hour(start)))
  } else{
    ## Remove all errorneous samples
    activity <- activity %>% filter(start <= end)
  }
  
  dplyr::bind_rows(activity,sensors) %>% mutate(sessionId = user)
}

prepADL <- function(userIds = c("A","B")){
  
  dfs <- lapply(userIds,getUserData)
  
  dplyr::bind_rows(dfs)
}

ADLData <- prepADL()
ADLData <- ADLData 
write.csv(ADLData,"data/ADL.csv",row.names = F)
