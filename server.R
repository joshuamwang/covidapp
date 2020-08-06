source('global.R')

shinyServer(function(input, output,session) {
  #Loading libraries here, allows for progress bar to be added.
  oldJournal <<- NULL
  days <<- NULL
  # withProgress(message="Initializing...",{
  #   
  #   setProgress(message='Loading data from cloud...',value=1)
  # 
  #   # future({
  #   #   GARGLE_API <- readRDS('apiKey.RDS')
  #   #   gs4_auth_configure(api_key=GARGLE_API)
  #   #   data <- read_sheet('https://docs.google.com/spreadsheets/d/1znGDGnYkUdgi0Rs243NksgCYxAZM265q24TNOB-9nZw/')
  #   # })
  #   oldJournal <<- NULL
  # })
  
  # output$searchUI <- renderUI({
  #   key <- event_data(event='plotly_click',source='mainPlot')$key
  #   if(length(oldJournal)==0){
  #     oldJournal <<- "All Journals"
  #     selectInput(inputId='search',label=NULL,choices=c('All Journals',sort(unique(summarizedData$Journal))),selected='All Journals',multiple=FALSE)
  #   }else{
  #     if(key!=oldJournal){
  #       oldJournal <<- key
  #       selectInput(inputId='search',label=NULL,choices=c('All Journals',sort(unique(summarizedData$Journal))),selected=key,multiple=FALSE)
  #     }
  #   }
  # })

  observeEvent(event_data(event='plotly_click',source='mainPlot')$key,{
    key <- event_data(event='plotly_click',source='mainPlot')$key
    updateSelectInput(session,inputId='search',label=NULL,choices=c('All Journals',sort(unique(summarizedData$Journal))),selected=key)
  })
  
  output$mainPlot <- renderPlotly({
    journal <- input$search
    ga_collect_event(event_label=journal)
    x <- input$x
    y <- input$y
    xMedian <- ""
    yMedian <- ""
    xFreq <- ""
    yFreq <- ""
    #future({
    if(x=='Covid'){
      x <- 6
      xMedian <- "COVID Median: "
      xFreq <- "# of COVID Articles: "
    }else{
      x <- 4
      xMedian <- "Non-COVID Median: "
      xFreq <- "# of Non-COVID Articles: "
    }
    
    if(y=='Covid'){
      y <- 6
      yMedian <- "COVID Median: "
      yFreq <- "# of COVID Articles: "
    }else{
      y <- 2
      yMedian <- 'Pre-COVID Median: '
      yFreq <- "# of Pre-COVID Articles: "
    }
    
    if(journal=='All Journals'){
      opacity <- 1
    }else{
      opacity <- 0.6
    }
  
    colorVar <- rep('rgba(17, 157, 255,0.5)',dim(summarizedData)[1])
    opacityVar <- rep(1,dim(summarizedData)[1])

    fig <- plot_ly(summarizedData, x = summarizedData[,x], y = summarizedData[,y], key=~Journal,source='mainPlot',
                   type='scatter',mode='markers',size=~I(50*covid_completeness),#opacity=opacity,
                   #marker=list(color = 'rgba(17, 157, 255,0.5)',line=list(color = 'rgba(17, 157, 255,0.5)')),
                   text=~paste('</br><b>','Journal: </b>',Journal,'</br>',
                               "</br><b>","# of COVID Articles: </b>",summarizedData[,7],
                               "</br><b>","COVID Article Completeness: </b>",round(summarizedData[,12]*100,1),"%",
                               "</br><b>","COVID Median: </b>",summarizedData[,6],'days','</br>',
                               "</br><b>","# of Non-COVID Articles: </b>",summarizedData[,5],
                               "</br><b>","Non-COVID Article Completeness: </b>",round(summarizedData[,13]*100,1),"%",
                               "</br><b>","Non-COVID Median: </b>",summarizedData[,4],'days','</br>',
                               "</br><b>","# of Pre-COVID Articles: </b>",summarizedData[,3],
                               "</br><b>","Pre-COVID Article Completeness: </b>",round(summarizedData[,14]*100,1),"%",
                               "</br><b>","Pre-COVID Median: </b>",summarizedData[,2],'days','</br>'),
                hoverinfo='text',hoverlabel=list(align='left'))
    
    if(journal!='All Journals'){
      # fig <- fig %>% add_trace(x=summarizedData[summarizedData$Journal==journal,x],opacity=1,key=~Journal,source='mainPlot',
      #                          y=summarizedData[summarizedData$Journal==journal,y],type='scatter',
      #                          hovertext=~paste('</br> Journal: ',Journal,
      #                                      '</br> # of Covid Articles: ',count,
      #                                      '</br> # of Non-Covid Articles: ',count_noncovid),hoverinfo='text',
      #                          mode='markers',marker=list(color='red',line=list(color='red')))
      
      colorVar[which(summarizedData$Journal==journal)] <- 'orange'
      opacityVar[which(summarizedData$Journal!=journal)] <- 0.25
      
      fig <- fig %>% add_segments(x=summarizedData[summarizedData$Journal==journal,x],
                                  xend=summarizedData[summarizedData$Journal==journal,x],
                                  y=0,hoverinfo='none',
                                  yend=summarizedData[summarizedData$Journal==journal,y],
                                  name='linear',mode='lines',line=list(color='red',dash='dash'))
      fig <- fig %>% add_segments(x=0,hoverinfo='none',
                                  xend=summarizedData[summarizedData$Journal==journal,x],
                                  y=summarizedData[summarizedData$Journal==journal,y],
                                  yend=summarizedData[summarizedData$Journal==journal,y],
                                  name='linear',mode='lines',line=list(color='red',dash='dash'))
    }
      
    fig <- fig %>% add_trace(y=~summarizedData[,x],hoverinfo='none',name='linear',mode='lines',line=list(color='black'))
    
    fig <- fig %>% add_trace(marker=list(color = colorVar,line=list(color = colorVar),opacity=opacityVar))
    
    #fig <- fig %>% add_trace(marker=list(color = 'rgba(17, 157, 255,0.5)',line=list(color = 'rgba(17, 157, 255,0.5)')))
    
        
    fig <- fig %>%
      layout(xaxis=list(showgrid=TRUE,title='Time (days)'),
             yaxis=list(showgrid=TRUE,title='Time (days)'),
             showlegend=FALSE)
    
    fig <- fig %>%
      config(displayModeBar = F)
    
    fig#},globals = c('summarizedData','covidPapers','nonCovidPapers','preCovidPapers')) 
    })


  output$densityPlot <- renderCachedPlot({
    #validate(need(input$search,message=FALSE))
    journal <- input$search
    x <- input$x
    y <- input$y
    
    #future({
    if(journal=='All Journals'){
      if(x!='Covid'){
        x1 <- data.table(a=nonCovidPapers[,'Days from Date Received to Date on PubMed'],by='Non-COVID Articles')
        x1 <- x1[-which(x1[,'a']==max(x1[x1$by=='Non-COVID Articles','a'])),]
      }else{
        x1 <- data.table(a=covidPapers[,'Days from Date Received to Date on PubMed'],by='COVID Articles')
      }

      if(y=='Covid'){
        x2 <- data.table(a=covidPapers[,'Days from Date Received to Date on PubMed'],by='COVID Articles')
      }else{
        x2 <- data.table(a=preCovidPapers[,'Days from Date Received to Date on PubMed'],by='Pre-COVID Articles')
      }
    }else{
      if(x!='Covid'){
        x1 <- data.table(a=nonCovidPapers[nonCovidPapers$Journal==journal,'Days from Date Received to Date on PubMed'],by='Non-COVID Articles')
        x1 <- x1[-which(x1[,'a']==max(x1[x1$by=='Non-COVID Articles','a'])),]
      }else{
        x1 <- data.table(a=covidPapers[covidPapers$Journal==journal,'Days from Date Received to Date on PubMed'],by='COVID Articles')
      }

      if(y=='Covid'){
        x2 <- data.table(a=covidPapers[covidPapers$Journal==journal,'Days from Date Received to Date on PubMed'],by='COVID Articles')
      }else{
        x2 <- data.table(a=preCovidPapers[preCovidPapers$Journal==journal,'Days from Date Received to Date on PubMed'],by='Pre-COVID Articles')
      }
    }

    agg <- rbind(x1,x2)

    if('Covid'%in%c(x,y)){
      agg <- agg %>% mutate(segmenty=if_else(by==unique(agg$by)[1],true = 0,false = -0.0015),
                            segmenty2=if_else(by==unique(agg$by)[2],true = -0.003,false = -0.0015))
    }else{
      agg <- agg %>% mutate(segmenty=if_else(by==unique(agg$by)[1],true = 0,false = -0.0002),
                            segmenty2=if_else(by==unique(agg$by)[2],true = -0.0004,false = -0.0002))
    }

    colors <- c('COVID Articles'='#440154FF','Non-COVID Articles'='#21908CFF','Pre-COVID Articles'='#DCE319FF')

    require(ggplot2)
    fig <- ggplot(agg,aes(x=a,fill=by,colour=by))+geom_density(alpha=0.6,position='identity')
    fig <- fig + geom_segment(mapping=aes(x = a,y=segmenty,yend=segmenty2,xend=a))

    fig <- fig + scale_fill_manual(values=c(colors[unique(agg$by)[1]],colors[unique(agg$by)[2]]))
    fig <- fig + scale_color_manual(values=c(colors[unique(agg$by)[1]],colors[unique(agg$by)[2]]))
    #fig <- fig + scale_fill_manual(values=c("#69b3a2", "#404080"))
    #fig <- fig + scale_color_manual(values=c("#69b3a2", "#404080"))
    fig <- fig + theme(legend.position=c(0.9,0.99),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                       axis.text=element_text(size=14),axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),
                       axis.title=element_text(size=15),legend.text=element_text(size=14))
    fig <- fig + xlab('Time (days)') + ylab('density')

    fig
    #},globals = c('summarizedData','covidPapers','nonCovidPapers','preCovidPapers'))
  }
  ,cacheKeyExpr = {list(input$search,input$x,input$y)}
  )
  
  
  output$table <- renderDataTable({
    #validate(need(input$search,message=FALSE))
    journal <- input$search
    
    #future({
    if(journal=='All Journals'){
      journalData <- covidPapers
      return(datatable(journalData[,-1],rownames= FALSE))
    }
    
    if(!is.null(journal) && is.null(days)){
      journalData <- covidPapers[covidPapers$Journal==journal,]
      return(datatable(journalData[,-1],rownames= FALSE))
    }
      
    #},globals = c('summarizedData','covidPapers','nonCovidPapers','preCovidPapers'))
    })
  
    output$nonCovidTable <- renderDataTable({
      journal <- input$search
      
      if(journal=='All Journals'){
        journalData <- nonCovidPapers
        return(datatable(journalData[,-1],rownames= FALSE))
      }
      
      if(!is.null(journal) && is.null(days)){
        journalData <- nonCovidPapers[nonCovidPapers$Journal==journal,]
        return(datatable(journalData[,-1],rownames= FALSE))
      }
      
    })
  
    output$preCovidTable <- renderDataTable({
      journal <- input$search
      
      if(journal=='All Journals'){
        journalData <- preCovidPapers
        return(datatable(journalData[,-1],rownames= FALSE))
      }
      
      if(!is.null(journal) && is.null(days)){
        journalData <- preCovidPapers[preCovidPapers$Journal==journal,]
        return(datatable(journalData[,-1],rownames= FALSE))
      }
      
    })

})

