ui <- fluidPage(
  
  headerPanel("PhenoCam ROI Tool"),
  
  sidebarPanel(width = 4,
               selectInput("site", "Site", sites),
               selectInput("vegtype", "Vegetation Type", choices = vegTypes, selected = ''),
               textInput('descr','Description', placeholder = 'Enter a description for the ROI'),
               textInput('owner','Owner', placeholder = 'Enter your name'),
               h4('ROI List filename:'),
               textOutput('roilabel'),
               hr(),
               
               selectInput("masks", "Mask", choices = NULL),
               dateRangeInput(inputId = 'roiDateRange', label = 'ROI Start/End Dates:', start = '2001-01-01', end = '2016-01-01', separator = '-', startview='year'),
               
               fluidRow(
                 column(width = 6, timeInput("starttime", "Start Time:",  seconds = F)),
                 column(width = 6, timeInput("endtime", "End Time:",  seconds = F))
               ),
               actionButton("generate", "Generate ROI List", icon = icon('list-alt'), width = "100%")
  ),
  
  
  
  mainPanel(
    fluidRow(
      column(2, selectInput("year", "Year", '', width = '80px')),
      br(),
      column(10, sliderInput(inputId = "viewDay", label =  NULL, 
                             min = 1, max = 365, ticks = F, animate=F,
                             value = 50, round = T, step = 1, width = '700px'))
    ),
    
    fluidRow( 
      column(1, actionButton("backplay", "", icon = icon('step-backward'), width = '100%', style="border-color: #fff; align:center")),
      column(1, actionButton("back", "", icon = icon('backward'), width = '100%', style="border-color: #fff")),
      column(1, actionButton("pause", "", icon = icon('pause'), width = '100%',  style="border-color: #fff")),
      column(1, actionButton("forw", "", icon = icon('forward'), width = '100%',  style="border-color: #fff")),
      column(1, actionButton("play", "", icon = icon('step-forward'), width = '100%', style="border-color: #fff; align:center"))
    ),
    
    fluidRow(
      column(6, plotOutput("plot", click = "newPoint", width = "350px", height = '260px')),
      column(6,plotOutput("maskplot", width = "350px", height = '260px'))
    ),
    
    fluidRow(
      column(3, colourInput(inputId = 'roicol', label = 'ROI Color',value = '#30aa20', showColour = 'background')),
      column(3, sliderInput(inputId = 'roitrans', label = 'Transparency', min = 0, max = 100, value = 50, ticks = F, width = '100%', pre = '%')),
      br(),
      column(6, actionButton("cancel", "Start over", icon = icon('refresh'), width = "120px"),
             actionButton("undo", "Undo", icon = icon('undo'), width = "120px"),
             actionButton("accept", "Accept", icon = icon('thumbs-up'), width = "120px"))
    ),

    hr(),
    fluidRow(
      column(6, radioButtons('sevenorall', label = 'Time series range:', choices = c('7 days','Entire range'), width = "400px",inline = T)),
      br(),
      column(6, actionButton("extract", "Extract", icon = icon('line-chart'), width = "200px"))
    ),
    sliderInput(inputId = "dateRange", label =  NULL, ticks = F, 
                min = 1, max = 365, 
                value = c(1,365), round = T, step = 1, dragRange = T, width = "100%"),
    plotOutput(outputId = "timeSeries", height = "200px", width = "100%")
    
  )
)



