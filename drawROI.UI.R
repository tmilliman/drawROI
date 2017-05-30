ui <- fluidPage(
  
  # theme = "bootstrap.min.css",
  # Application title
  headerPanel("PhenoCam ROI Tool"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(width = 4,
               selectInput("site", "Site", sites),
               selectInput("year", "Year", ''),
               selectInput("rois", "ROI", choices = NULL),
               fluidRow(
                 column(width = 6, colourInput(inputId = 'roicol', label = 'ROI Color',value = '#30aa20', showColour = 'background')),
                 column(width = 6, sliderInput(inputId = 'roitrans', label = 'Transparency', min = 0, max = 100, value = 50, ticks = F, width = '100%', pre = '%'))
               )
               ,
               radioButtons('sevenorall', label = 'Time series range:', choices = c('First 7 days','Entire time range'), width = "100%",inline = T),
               actionButton("extract", "Extract GCC", icon = icon('line-chart'), width = "100%"),
               br(),
               hr(),
               
               dateRangeInput(inputId = 'roiDateRange', label = 'ROI Start/End Dates:', start = '2001-01-01', end = '2016-01-01', separator = '-', startview='year'),
               
               fluidRow(column(width = 6, timeInput("starttime", "Start Time:",  seconds = F)),
                        column(width = 6, timeInput("endtime", "End Time:",  seconds = F))
               ),
               actionButton("generate", "Generate ROI List", icon = icon('list-alt'), width = "100%"),
               br(),
               br(),
               actionButton("export", "Export ROI List", icon = icon('save'), width = "100%"),
               br(),
               br(),
               actionButton("submit", "Submit ROI List", icon = icon('upload'), width = "100%")
  ),
  
  
  
  mainPanel(
    strong('Daily Image browser'),
    sliderInput(inputId = "viewDay", label =  NULL, 
                min = 1, max = 365, ticks = F, animate=F,
                value = 50, round = T, step = 1, width = '500px'),
    
    fluidRow( 
      column(1, actionButton("play", "", icon = icon('play'), width = '100%', style="border-color: #fff; align:center")),
      column(1, actionButton("pause", "", icon = icon('pause'), width = '100%',  style="border-color: #fff"))
    ),
    
    
    fluidRow( 
      column(1, actionButton("back", "", icon = icon('backward'), width = '100%', style="border-color: #fff")),
      column(7, plotOutput("plot", click = "newPoint", width = "350px", height = '260px')),
      column(1, actionButton("forw", "", icon = icon('forward'), width = '100%',  style="border-color: #fff"))
    ),
    br(),
    actionButton("cancel", "Start over", icon = icon('refresh'), width = "160px"),
    actionButton("undo", "Undo", icon = icon('undo'), width = "160px"),
    actionButton("accept", "Accept", icon = icon('thumbs-up'), width = "160px"),
    br(),
    br(),
    strong('ROI Mask'),
    fluidRow( 
      column(1 ),
      column(7, plotOutput("maskplot", width = "350px", height = '260px')),
      column(1)
    ),
    hr(),
    strong('Extracted Time Series'),
    
    sliderInput(inputId = "dateRange", label =  NULL, ticks = F, 
                min = 1, max = 365, 
                value = c(1,365), round = T, step = 1, dragRange = T, width = "500px"),
    
    plotOutput(outputId = "timeSeries", height = "200px", width = "500px")
    
    
  )
)



