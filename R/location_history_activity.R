parse_location_history <- function(path_to_file, from_date = '2017-01-01', to_date = '2018-01-01'){
  library(jsonlite)
  system.time(x <- fromJSON("C:/users/ommendel/Downloads/Location History.json"))
  loc = x$locations
  
  
  
  # converting time column from posix milliseconds into a readable time scale
  library(lubridate)
  library(zoo)
  
  loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")
  loc$date <- as.Date(loc$time, '%Y-%m-%d')
  loc$year <- year(loc$date)
  loc$month_year <- as.yearmon(loc$date)
  
  
  
  # converting longitude and latitude from E7 to GPS coordinates
  loc$lat = loc$latitudeE7 / 1e7
  loc$lon = loc$longitudeE7 / 1e7

  
  
  loc_subset <- loc
  if(!is.null(from_date)){
    loc_subset <- loc_subset[loc_subset$time >=as.POSIXct(from_date),]
  }
  if(!is.null(to_date)){
    loc_subset <- loc_subset[loc_subset$time <= as.POSIXct(to_date),]
  }
  
  return(loc_subset)
  
}


get_main_activity_events <- function(loc_subset){
  activities <- loc_subset$activity
  list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
  activities  <- activities[list.condition]
  
  activities_df <- do.call("bind_rows", activities)
  main_activity <- sapply(activities_df$activity, function(x) x[[1]][1][[1]][1])
  activities_df <- data.frame(main_activity = main_activity, 
                             start = as.POSIXct(as.numeric(activities_df$timestampMs)/1000, origin = "1970-01-01"))
  
  activities_df <- activities_df %>% 
    arrange(start) %>% 
    transmute(label = main_activity, start = start, end = lead(start),start,type = 'ACTIVITY', sessionId = 1)
  
  
  source("R/timelineUtils.R")
  
  joined_activities <- joinAdjacentEvents(activities_df,minGapInSeconds = 180) %>% arrange(start)
  joined_activities$label <- ifelse(joined_activities$label=='TILTING','STILL',joined_activities$label)
  joined_activities <- joined_activities %>% filter(label !='UNKNOWN', label != 'TILTING') %>% filter(!is.na(end))
  joined_activities$duration <- with(joined_activities,end-start)
  
  ## Total duration per activity
  joined_activities %>% group_by(label) %>% summarize(total_time = sum(duration)) %>% arrange(desc(total_time))
  
  
  return(joined_activities)
}

get_location_places <- function(loc_subset){
  library(fpc)
  library(dplyr)
  loc_points <- loc_subset %>% 
    filter(accuracy >=0 & accuracy < 50) %>% 
    data.frame()
  
  sample_for_model <- loc_points %>% sample_n(5000) %>% select(lat,lon)
  
  model <- dbscan(sample_for_model,method = 'hybrid',eps = 0.005,MinPts = 5)
  
  prediction <- predict(model,data = sample_for_model,newdata = loc_points[,c('lat','lon')])
  loc_points$placeId <- prediction

  loc_points <- loc_points %>% arrange(time) %>%
    transmute(label = placeId, start = time, end = lead(time),start,type = 'LOCATION', sessionId = 1)
  
  
  source("R/timelineUtils.R")
  
  joined_locations <- joinAdjacentEvents(loc_points,minGapInSeconds = 180) %>% arrange(start)
  
#  places <- loc_points %>% group_by(placeId) %>% summarize(lat = median(lat),lon = median(lon))
#  library(leaflet)
#  leaflet(places) %>% addTiles() %>% addMarkers()
  
  return(joined_locations)
}