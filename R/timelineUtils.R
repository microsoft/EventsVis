library(data.table)
library(dplyr)
library(googleVis)


getTimeline = function(sessionDataset, joinNeighbors = TRUE, joinGapInSeconds = 10, plotWidth = "automatic"){
  if(missing(sessionDataset) | is.null(sessionDataset)){
    return(NULL)
  }
  
  sessionDataset <- sessionDataset %>% select(type,label,start,end)
  
  if(is.numeric(sessionDataset$start)){
    sessionDataset$start <- as.POSIXct(sessionDataset$start,origin = "1970-01-01",tz = "UTC")
    sessionDataset$end <- as.POSIXct(sessionDataset$end,origin = "1970-01-01",tz = "UTC")
  }
  
  #  if(joinNeighbors & joinGapInSeconds > 0){
  #    toTimeLine <- joinAdjacentEvents(sessionDataset,minGapInSeconds = joinGapInSeconds)
  #  } else{
  #    toTimeLine <- sessionDataset
  #  }
  
  
  
  lanes = length(unique(sessionDataset$type))
  
  library(googleVis)
  tl <- gvisTimeline(data=sessionDataset, rowlabel="label",
                     start="start", end="end",
                     options=list(timeline="{showRowLabels:true, colorByRowLabel:true}",
                                  colors= "['#cbb69d', '#603913', '#c69c6e']",
                                  height = 200*lanes,
                                  width = plotWidth))
  
  tl
  
}





joinAdjacentEvents <- function(sessionEvents, minGapInSeconds=10){
  
  library(lubridate)
  
  colnames <- names(sessionEvents)
  
  joined <- sessionEvents %>% arrange(start) %>%
    group_by(type,label, sessionId) %>%                                                     # for each label and type
    mutate(
      prevEnd = lag(end),
      dist = difftime(start, lag(end), units = "secs")) %>%
    mutate(isDistinct = ifelse(is.na(dist) | dist > minGapInSeconds, 1, 0),                # flag distinct events: distances > minGapInSeconds''
           joinedEventId = cumsum(isDistinct)) %>%                             # create session id
    group_by(type,label, sessionId, joinedEventId) %>%                                      # group by adjacent types, labels and joints
    summarise(start = min(start),                                        
              end = max(end)) %>%
    ungroup()
  joined <- joined %>% data.frame()
  joined[,colnames]
  
}

getPartOfDay <- function(events,eventType = 'TIME_PartOfDay'){
  
  if(nrow(events)==0){
    return(data.frame())
  }
  
  if(length(unique(events$sessionId))> 1){
    stop('Part of day events are calculated for one session only.')
  }
  
  dataTZ = attr(events$start,"tzone")
  
  allHours <- c()
  events <- data.frame(events)
  for(i in 1:nrow(events)){
    thisEventHours <- seq.POSIXt(from = events[i,'start'],to = events[i,'end'],by='hour')
    allHours <- list(allHours,thisEventHours)
  }
  allHours <- as.POSIXct(unlist(allHours),origin = '1970-01-01',tz = dataTZ)
  #print(allHours)
  minute(allHours) <- 0
  second(allHours) <- 0
  
  allHours <- unique(allHours)
  
  #allHours
  podID <- sapply(allHours, partOfDayID)
  podDF <- data.frame(start = allHours, pod = podID) %>% 
    mutate(label = partOfDayForID(pod),end = lead(start), 
           type = eventType,sessionId = unlist(events[1,'sessionId'])) %>%
    select(-pod)
 
  distinctPODs <- joinAdjacentEvents(podDF,1)
  distinctPODs$end <- as.POSIXct(ifelse(is.na(distinctPODs$end),distinctPODs$start+hours(1),distinctPODs$end),origin = lubridate::origin)
  return(distinctPODs)
  
  
}

getDayOfWeek <- function(events,eventType = 'TIME_DayOfWeek'){
  
  
  if(nrow(events)==0){
    return(data.frame())
  }
  
  if(length(unique(events$sessionId))> 1){
    stop('Part of day events are calculated for one session only.')
  }
  
  dataTZ = attr(events$start,"tzone")
  allDays <- c()
  events <- data.frame(events)
  for(i in 1:nrow(events)){
    thisEventDays <- seq.POSIXt(from = events[i,'start'],to = events[i,'end'],by='day')
    allDays <- list(allDays,thisEventDays)
  }
  allDays <- as.POSIXct(unlist(allDays),origin = '1970-01-01',tz = dataTZ)
  
  #allHours <- seq.POSIXt(min(events$start,na.rm = T),max(events$end,na.rm = T),by='hours')
  hour(allDays) <- 0
  minute(allDays) <- 0
  second(allDays) <- 0
  
  allDays <- unique(allDays)
  weekday <- weekdays(allDays)
  weekdayDF <- data.frame(start = allDays, label = weekday) %>% 
    mutate(end = lead(start), type = eventType,sessionId = unlist(events[1,'sessionId']))
  distinctDOWs <- joinAdjacentEvents(weekdayDF,1)
  distinctDOWs$end <- as.POSIXct(ifelse(is.na(distinctDOWs$end),distinctDOWs$start+days(1),distinctDOWs$end),origin = lubridate::origin)
  
  return(distinctDOWs)
  
  
}

dayOrNightForTime <- function(ts){
  library(lubridate)
  h <- hour(ts)
  if(h > 21 && h < 24 || h < 6){
    return("NIGHT")
  }else{
    return("DAY")
  }
}

partOfDayForTime <- function(ts){
  library(lubridate)
  h <- hour(ts)
  if(h > 22 && h <= 24 || h < 6){
    return("Night (10pm - 6am)")
  }else if (h >=6 & h<12){
    return("Morning (6am - 12pm)")
  } else if(h >=12 & h < 17){
    return("Noon (12pm - 5pm)")
  } else{
    return("Evening (5pm - 10pm)")
  }
}

partOfDayForID <- function(id){

  podsFactor <- factor(id)
  pods <- c("Night (10pm - 6am)","Morning (6am - 12pm)","Noon (12pm - 5pm)","Evening (5pm - 10pm)")
  levels(podsFactor) <- pods

  return(podsFactor)
}

partOfDayIDForTime <- function(ts){
  library(lubridate)
  h <- hour(ts)
  if(h > 22 && h <= 24 || h < 6){
    return(0)
  }else if (h >=6 & h<12){
    return(1)
  } else if(h >=12 & h < 17){
    return(2)
  } else{
    return(3)
  }
}

partOfDay <- Vectorize(partOfDayForTime)
partOfDayID <- Vectorize(partOfDayIDForTime)
dayOrNight <- Vectorize(dayOrNightForTime)
