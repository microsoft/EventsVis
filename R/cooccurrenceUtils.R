library(visNetwork)
library(dplyr)
library(RColorBrewer)

getCooccurrenceGraph <- function(session_events, thresholdForCoccurring = 50){
  if(missing(session_events) | is.null(session_events)| nrow(session_events)==0){
    return(NULL)
  }
  
  events <- session_events %>% transmute(sessionId = sessionId, event = as.character(paste0(type,":",label)), start = start, end = end, 
                                        duration = ifelse(is.numeric(start),end - start,difftime(end,start,units = 'sec')))
  events_df <- events %>% arrange(start) %>% group_by(event) %>% summarize(value = sum(duration), counter = n()) %>% mutate(id = 1:n())
  
  edges <- getCoocsInner(events,events_df,tolerance = thresholdForCoccurring)
  
  if(nrow(edges) > 0){
    edges <- edges %>% group_by(first, second) %>% summarize(value = n()) %>% rename(from = first, to = second)
    edges$title <- paste0("<p> <BR><b># of co-occurrences = </b>",edges$value,"</p>")
  }
  
  nodes <- events_df %>% mutate(label = event,title = paste0("<p> <U>",label,"</U><BR><b>Count</b> = ",counter,"</p>"))
  
  return(list(edges,nodes))
}

getCooccurrenceVisNetwork <- function(graph){

  edges <- graph[[1]]
  nodes <- graph[[2]]
  
  if(nrow(nodes)>3){
    if(nrow(nodes) > 12){
      colors <- rep(brewer.pal(12,'Paired'),nrow(nodes)/10)
      nodes$color <- colors[1:nrow(nodes)]
    } else{
      nodes$color <- brewer.pal(nrow(nodes),'Paired')
    }
  }
  
  visNetwork(nodes, edges)   %>%
    visNodes(size = 40) %>%
    visOptions(selectedBy = "label", 
               highlightNearest = TRUE, 
               nodesIdSelection = F) %>%
    visInteraction(keyboard = TRUE,
                   dragNodes = T, 
                   dragView = T, 
                   zoomView = T)
  
  
  
}

getCooccurringEvents <- function(session_events,tolerance = 25){
  session_events <- session_events %>% mutate(event = as.character(paste0(type,":",label)), duration = end-start)
  events_df <- session_events %>% arrange(start) %>% group_by(event) %>% summarize(value = sum(duration), counter = n()) %>% mutate(id = 1:n())
  getCoocsInner(session_events,events_df,tolerance)
}

getCoocsInner <- function(session_events, events_df, tolerance=25){
  library(lubridate)
  
  if(is.null(session_events$id)){
    session_events <- session_events %>% inner_join(events_df,by='event')
  }
  session_events <- session_events  %>% arrange(start)
  startTimes <- as.numeric(session_events$start)
  endTimes <- as.numeric(session_events$end)
  ids <- as.factor(session_events$id)
  len <- length(startTimes)
  
  coocs <- list()
  counter <- 0
  
  for(i in 1:(len-1)){
    for(j in (i+1):len){
      if(startTimes[j] < (endTimes[i] + tolerance) & startTimes[j] > startTimes[i]){
        #coocs <- list(coocs,list(first = i,second = j))
        counter <- counter + 1
        coocs[[counter]] <- data.frame(one = ids[i],two = ids[j])
      }
      if(endTimes[j] + tolerance > endTimes[i]){
        break
      }
    }
  }
  
  #Convert list of data frames to data frame
  if(length(coocs) != 0){
    cooccurrences <- do.call("rbind", coocs)
    cooccurrences$one <- as.numeric(cooccurrences$one)
    cooccurrences$two <- as.numeric(cooccurrences$two)
    cooccurrences <- cooccurrences %>% mutate(first = ifelse(one < two,one,two),second = ifelse(one < two,two,one)) %>% select(first,second)
  } else{
    return(data.frame())
  }
  #print(cooccurrences)
  if(is.null(cooccurrences)){
    return(data.frame())
  } else{
    return(cooccurrences)
  }
  
}

all_combinations <- function(sessionDataset,tolerance = 25){
  session_events <- sessionDataset %>% mutate(event = as.character(paste0(type,":",label)), duration = end-start,id = 1:n())

  all_combinations <- data.frame()
  all_types <- as.character(unique(session_events$type))
  pair_types <- t(combn(all_types,2)) %>% data.frame()
  
  prev_types <- session_events %>% filter(type == all_types[1])
  for (i in 2 :length(all_types)){
    this_type <- session_events %>% filter(type == all_types[i])
    this_type <-  bind_rows(this_type,prev_types)
    prev_types <- this_type
    coocs <- getCoocsInner(session_events = this_type,events_df = NULL, tolerance = tolerance)
    all_combinations <- bind_rows(all_combinations,coocs)
    
    new_events <- create_new_coocs(coocs,session_events)
    session_events <- bind_rows(session_events,new_events)
  }
}

create_new_coocs<-function(coocs,session_events){
  new_coocs <- inner_join(coocs,session_events,by=c('first'='id')) %>% select(first,second,'first_label' = 'label','first_type' = 'type','first_start'='start','first_end'='end')
  new_coocs <- inner_join(new_coocs,session_events,by=c('second'='id'))%>% select(first, second, first_label, first_type,'second_label' = 'label','second_type' = 'type',first_start,first_end,'second_start'='start','second_end'='end')
  
  max_id <- max(session_events$id)

  cooc_events <- with(new_coocs,data.frame(label = paste(first,second,sep = "<--->"),
                                           type = paste(first_type,second_type,sep = "<--->"),
                                           start = max(first_start,second_start),
                                           end = min(first_end,second_end),
                                           sessionId = unlist(session_events[1,'sessionId']),
                                           id = (max_id+1):(max_id+nrow(new_coocs))
                                           ))
  return(cooc_events)
}

#Event <- setClass("Event",slots = c(start = "numeric",end = "numeric",type ="character",label = "character"))
