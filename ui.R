source('global.R')

fluidPage(
  #tab title
  title="time2pub",
  #titlePanel(h2("Journal-Specific Fast-Track time2pub Visualization",align='center')),   
  
  
  mainPanel(width=12,
            # tags$style(type="text/css",
            #            ".shiny-output-error { visibility: hidden; }",
            #            ".shiny-output-error:before { visibility: hidden; }"
            # ),
            
    fixedRow(
      column(2,br(),a(href='https://www.time2pub.com/',img(src='logo.jpg',height='75px',width='150px'))),
      column(8,offset=0,br(),titlePanel(h3("Journal-Specific Fast-Track time2pub Visualization",align='center')))
    ),
    br(),
    fluidRow(
      column(1,br(),br(),br(),br(),br(),br(),br(),br(),
             radioButtons('y','',choices=c('Covid','Pre-Covid'),selected='Covid',inline=F),
             style='padding:0px;margin-right:-1em'),
      column(5,
        #div(align='left',uiOutput('searchUI')),     
        div(align='left',selectInput('search',label=NULL,choices=c('All Journals',sort(unique(summarizedData$Journal))),selected='All Journals',multiple=FALSE)),     
        
        #div(align='left',style='margin-bottom=-3em',selectInput('search',label=NULL,choices=c('All Journals',sort(unique(summarizedData$Journal))),selected='All Journals',multiple=FALSE)),     
        div(plotlyOutput(outputId='mainPlot',width='100%'),style='margin-left:-2em;margin-top:-1em'),
        div(align = "center", style='margin-top:-2em',br(),radioButtons('x','',choices=c('Covid','Non-Covid'),selected='Non-Covid',inline=T))
      ),
      column(6,br(),br(),br(),
      div(plotOutput(outputId='densityPlot',width='100%'),style='margin-left:2em;')
      )
    ),
    fluidRow(
      br(),h4('PubMed Entry Navigation',style='margin-left:1em;')
    ),
    tabsetPanel(
      tabPanel('COVID',br(),dataTableOutput("table")),
      tabPanel('Non-COVID',br(),dataTableOutput('nonCovidTable')),
      tabPanel('Pre-COVID',br(),dataTableOutput('preCovidTable'))
    ),
    br(),br(),br(),br()
    # br(),br(),
    # HTML(paste(h3('Covid Papers'))),br(),
    # fluidRow(
    #   dataTableOutput("table")
    # ),
    # br(),br(),
    # HTML(paste(h3('Non-Covid Papers'))),br(),
    # fluidRow(
    #   dataTableOutput('nonCovidTable')
    # ),
    # br(),br()

  )
)
