if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(googleVis)) install.packages("googleVis")
if(!require(data.table)) install.packages("data.table")
if(!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(lubridate)) install.packages("lubridate")
if(!require(sqldf)) install.packages("sqldf")
if(!require(parsedate)) install.packages("parsedate")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(visNetwork)) install.packages("visNetwork")


library(shiny)
library(shinydashboard)
library(googleVis)
library(data.table)
library(dplyr)
library(DT)
library(sqldf)
library(ggplot2)
library(visNetwork)
library(RColorBrewer)

TRY_PARSE_DATE <<- FALSE

shinyServer(function(input, output, session) {
  
  ####-------- Reactive functions (data aquisition) --------#####
  
  
  
  ## Get + prep entire dataset from CSV
  getDataset <- reactive({
    library(data.table)
    
    dataset <- withProgress({
      if(is.null(input$inputFile)){
        dataset <- fread("data/ADL.csv",stringsAsFactors = F)
      } else{
        dataset <- fread(input$inputFile$datapath,stringsAsFactors = F,na.strings=c("NA","N/A","null",""," "),showProgress = T)
      }
      
      if(is.null(dataset)){
        return(NULL)
      }
      if(!is.null(dataset$start)){
        if(!is.numeric(dataset$start)){
          if(TRY_PARSE_DATE){
            dataset$start <- parsedate::parse_date(dataset$start)
          } else{
            dataset$start <- as.POSIXct(dataset$start)
          }
        }
      }
      if(!is.null(dataset$end)){
        if(!is.numeric(dataset$end)){
          if(TRY_PARSE_DATE){
            dataset$end <- parsedate::parse_date(dataset$end)
          } else{
            dataset$end <- as.POSIXct(dataset$end)
          }
        }
      } else{
        dataset$end <- dataset$start + 1
      }
      
      if(is.null(dataset$sessionId)){
        dataset$sessionId <- 1
      }
      
      dataset$type <- as.factor(dataset$type)
      dataset$sessionId <- as.factor(dataset$sessionId)
      
      
      
      return(dataset)
      
      
      
    },message = "Loading dataset")
    
    print(paste('loaded',nrow(dataset),'records'))
    return(dataset)
  })
  
  
  ## Get all the possible values of sessionId
  getSessions <- reactive({
    dataset <- getDataset()
    if(!is.null(dataset)){
      sessions <- unlist(unique(dataset$sessionId))
      return(sessions)
    }
    else{
      return(NULL)
    }
  })
  
  ## Get a dataset filtered by the selected session and types
  getFilteredSession <- reactive({
    dataset <- getDataset()
    
    
    
    if(!is.null(dataset) & !is.null(input$sessionSelect) & !is.null(input$typesSelect)){
      filtered <- dataset %>% filter(sessionId == input$sessionSelect, type %in% input$typesSelect) %>% data.frame()
      print(input$sessionSelect)
      cat("\nFound",nrow(filtered),"events for session",input$sessionSelect,"\n")
      
      if(input$groupAdjacent){
        source("R/timelineUtils.R")
        filtered <- joinAdjacentEvents(filtered,minGapInSeconds =input$minGap)
      }
      
      if(input$DayOfWeek){
        debugSource("R/timelineUtils.R")
        days <- getDayOfWeek(filtered)
        
      } else{
        days <- data.frame()
      }
      
      if(input$PartOfDay){
        debugSource("R/timelineUtils.R")
        pods <- getPartOfDay(filtered)
      } else{
        pods <- data.frame()
      }
      
      filtered <- bind_rows(filtered,days) %>% bind_rows(pods) %>% arrange(start)
      
      
      return(filtered)
    }
    return(NULL)
    
  })
  
  ## Get a dataset filtered by the selected time range
  getFilteredEntireDataset <- reactive({
    dataset <- getDataset()
    if(!is.null(dataset) & !is.null(input$typesSelect)){
      filtered <- dataset %>% filter(type %in% input$typesSelect) %>% data.frame()
      
      if(input$groupAdjacent){
        source("R/timelineUtils.R")
        filtered <- joinAdjacentEvents(filtered,minGapInSeconds =input$minGap)
      }
      
      return(filtered)
    }
    return(NULL)
  })
  
  #### --- Server side UI elements ---- ####
  
  ## server side selectInput for session selection (out of the list of possible sessions in the data) 
  output$sessionSelect <- renderUI({
    input$inputFile
    sessions <- getSessions()
    if(is.null(sessions)){
      return(NULL)
    }
    selectInput("sessionSelect", "Choose Session:", as.list(sessions),selected = sessions[1]) 
  })
  
  ## Slider for selecting min and max time/date values for timeline 
  output$slider <- renderUI({
    input$sessionSelect
    dataset <- getFilteredSession()
    if(is.null(dataset)){
      s <- NULL
    } else{
      #dataset <- dataset %>% filter(sessionId == input$sessionSelect) %>% arrange(start)
      dataset <- dataset %>% arrange(start)
      
      mini = dataset[1,'start']
      maxi = max(dataset$end)
      
      mini = dataset[1,'start']
      maxDateValueToShow <- ifelse(nrow(dataset) > 50,50,nrow(dataset))
      endVal = dataset[maxDateValueToShow,'end']
      s <- sliderInput("slider","Time range",min = mini,max = maxi,value = c(mini,endVal),step = 1)
    }
    s
  })
  
  ## update slider values if session changes
  observe({
    input$sessionSelect
    
    dataset <- getDataset()
    if(is.null(dataset) | is.null(input$sessionSelect)){
      return(NULL)
    }
    dataset <- dataset %>% filter(sessionId == input$sessionSelect) %>% arrange(start)
    
    maxi = max(dataset$end)
    mini = dataset[1,'start']    
    
    maxDateValueToShow <- ifelse(nrow(dataset) > 50,50,nrow(dataset))
    endVal = dataset[maxDateValueToShow,'end']
    updateSliderInput(session,inputId = "slider",label = "Time range",min = mini,max = maxi,value = c(mini,endVal),step = 1)
  })
  
  output$typesSelect <- renderUI({
    dataset <- getDataset()
    if(is.null(dataset)){
      return(list())
    }
    
    types <- unlist(unique(dataset$type))
    
    selectizeInput(
      'typesSelect', 'Select event types to show', choices = types, multiple = TRUE,selected = types)
    
  })
  
  output$eventsSelect <- renderUI({
    dataset <- getFilteredSession()
    if(is.null(dataset)){
      return(list())
    }
    
    dataset$event = as.character(paste0(dataset$type,":",dataset$label))
    
    events <- unlist(unique(dataset$event))
    cat('events for coocs',events)
    selectizeInput(
      'eventsSelect', 'Select events to show', choices = events, multiple = TRUE,selected = events)
    
  })
  
  
  ####-------- Timeline plot --------#####
  
  output$timeline <- googleVis::renderGvis({
    source("R/timelineUtils.R")
    sessionDataset = getFilteredSession()
    
    if(is.null(sessionDataset) | is.null(input$slider)){
      return(NULL)
    }
    
    validate(
      need(!is.null(sessionDataset$sessionId),"session column missing"),
      need(!is.null(sessionDataset$type),"type column missing"),
      need(!is.null(sessionDataset$start),"start column missing"),
      need(!is.null(sessionDataset$end),"end column missing"),
      need(!is.null(sessionDataset$label),"label column missing"),
      errorClass = "validation"
    )
    sessionDataset <- sessionDataset %>% filter(start <= end) %>% filter(start >= input$slider[1], end <= input$slider[2])
    
    
    tl <- getTimeline(sessionDataset = sessionDataset,
                      joinNeighbors = input$groupAdjacent,
                      joinGapInSeconds = input$minGap, plotWidth = max(1000,nrow(sessionDataset)*50))
    
    tl
    
  })
  
  ####-------- DT (tabular) --------#####
  
  
  output$sql <- DT::renderDataTable({
    dataset = getFilteredSession()
    dataset$start <- as.character(dataset$start)
    dataset$end <- as.character(dataset$end)
    if(is.null(dataset)){
      return(data.frame())
    }
    library(sqldf)
    sqldf::sqldf(input$query)
  }, options = list(
    pageLength = 15,
    lengthMenu = c(5, 10, 15, 20,100)
  )
  )
  
  ####-------- across sessions plots --------#####
  
  output$distributions <- renderPlot({
    dataset = getDataset()
    if(is.null(dataset) | is.null(input$typesSelect)){
      return(NULL)
    }
    
    dataset <- dataset %>% filter(type %in% input$typesSelect)
    
    dataset$event <- paste0(dataset$type,": ",dataset$label)
    
    
    events <- unlist(sort(table(dataset$event),decreasing = T))
    if(!is.na(input$numEventsForDistribution)){
      print(input$numEventsForDistribution)
      if(input$numEventsForDistribution < length(events)){
        events <- events[1:input$numEventsForDistribution]
      }
    }
    #print(events)
    
    dataset <- dataset %>% filter(event %in% names(events)) %>% mutate(duration = as.integer(end) - as.integer(start))
    
    grouped <- dataset %>% group_by(sessionId,event) %>% summarize(totalDuration = sum(duration)) %>% mutate(Frequency = totalDuration)
    
    
    ggplot(grouped,aes(x = sessionId, y = Frequency,fill = event))+ ggtitle("Total duration per event across all sessions") + 
      geom_bar(position = "fill",stat = "identity") + coord_flip()
    
    #print(grouped)
    
  })
  
  ####-------- in session plots --------#####
  
  output$inSessionDistribution <- renderPlot({
    dataset = getFilteredSession()
    if(!is.null(dataset)){
      
      dataset <- dataset %>% filter(type %in% input$typesSelect)
      
      dataset$event <- paste0(dataset$type,": ",dataset$label)
      
      
      events <- unlist(sort(table(dataset$event),decreasing = T))
      if(!is.na(input$numEventsForDistribution)){
        print(input$numEventsForDistribution)
        if(input$numEventsForDistribution < length(events)){
          events <- events[1:input$numEventsForDistribution]
        }
      }
      #print(events)
      
      dataset <- dataset %>% filter(event %in% names(events)) %>% mutate(duration = as.integer(end) - as.integer(start))
      grouped <- dataset %>% group_by(event) %>% summarize(totalDuration = as.integer(sum(duration)))
      
      library(ggplot2)
      library(ggthemes)
      ggplot(grouped, aes(event, totalDuration)) + geom_col() + ggtitle(paste("Total duration per event in session",input$sessionSelect)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      
      
    } 
  })
  
  
  ####------ consecutive visNetwork ----- #####
  output$consecutives <- renderVisNetwork({
    source("R/networkUtils.R")
    input$sessionSelect
    dataset = getFilteredSession()
    if(!is.null(dataset)){
      
      dataset <- dataset %>% filter(type %in% input$typesSelect)
      withProgress({
        if(input$consecutivePerSession==FALSE){
          session = input$sessionSelect
          getConsecutiveNetwork(events = dataset,sesId = session,maxTimeForConsecutiveInSeconds = input$maxTimeForConsecutiveInSeconds,byDuration = (input$byDuration=="Sum by duration"))
        } else{
          getConsecutiveNetwork(events = dataset,sesId = NULL, maxTimeForConsecutiveInSeconds = input$maxTimeForConsecutiveInSeconds,byDuration = (input$byDuration=="Sum by duration"))
        }
      },message = "Graph is rendering")
    }
  })
  
  output$visNetworkTitle <- renderText({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    x <- input$consecutivePerSession
    
    # This will change the value of input$inText, based on x
    if(x==TRUE){
      paste("Which events occur after other events across all sessions?")
    } else{
      paste("Which events occur after other events? (session", input$sessionSelect,"only)")
    }
  })
  
  
  coocsGraph <- reactive({
    input$eventsSelect
    source("R/cooccurrenceUtils.R")
    input$sessionSelect
    dataset = getFilteredSession()
    print(head(dataset))
    print(input$eventsSelect)
    dataset <- dataset %>% mutate(event = as.character(paste0(type,":",label))) %>% filter(event %in% input$eventsSelect)
    print(head(dataset))
    if(!is.null(dataset)){
      graph <- withProgress({
        graph <- getCooccurrenceGraph(session_events = dataset,thresholdForCoccurring =input$cooccurrenceThreshold)
        
        
        
      },message = "Loading coocs")
    }
    
    graph
  })
  
  ####------ cooccurring events visNetwork ----- #####
  output$cooccurring <- renderVisNetwork({
    source("R/cooccurrenceUtils.R")
    
    graph <- coocsGraph()
    edges <- graph[[1]]
    if(nrow(edges) > 200){
      stop('Too many co-occurrences for visualization. Consider selecting less events, types of time events')
      return(NULL)
    } else{
      return(getCooccurrenceVisNetwork(graph))
    }
  })
  
  output$visNetworkCoocsTitle <- renderText({
    
    paste("Which events co-occur with other events? (session", input$sessionSelect,"only)")
    
  })
  
  ####---- Co-occcurrences table ---- ####
  output$coocsTable <- DT::renderDataTable({
    graph <- coocsGraph()
    edges <- graph[[1]]
    nodes <- graph[[2]]
    edges <- ungroup(edges)

    graph$edges <- edges
    
    edges$from <- as.integer(edges$from)
    edges$to <- as.integer(edges$to)
    
    edges$title <- NULL
    coocs <- inner_join(edges,nodes,by = c('from' = 'id'))
    coocs <- inner_join(coocs,nodes,by = c('to' = 'id')) %>% transmute(Event1 = label.x,Event2 = label.y,count = value.x) %>% arrange(desc(count))
    names(coocs)[3] <- 'Number of co-occurrences'
    coocs
  },caption = 'Number of co-occurrences between events in the data',options = list(
    pageLength = 10))
  
  
  
  
  
})