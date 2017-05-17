# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(raster)
library(jpeg)
library(shiny)
library(shinyTime)
library(lubridate)
library(colourpicker)

source('Resources/SwitchButton.R')

source('drawROI.Funcs.R')
source('drawROI.Init.R')

sites <- unique(imgDT$Site)

# ROIs <- list()


ui <- fluidPage(
  
  theme = "button.css",
  # Application title
  headerPanel("PhenoCam ROI Tool"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(width = 4,
               selectInput("site", "Site", sites),
               selectInput("year", "Year", ''),
               selectInput("rois", "ROI", choices = NULL),
               dateRangeInput(inputId = 'roiDateRange', label = 'ROI Start/End Dates:', start = '2001-01-01', end = '2016-01-01', separator = '-', startview='year'),
               
               fluidRow(column(width = 6, timeInput("starttime", "Start Time:",  seconds = F)),
                        column(width = 6, timeInput("endtime", "End Time:",  seconds = F))
               ),
               fluidRow(
                 column(width = 6, colourInput(inputId = 'roicol', label = 'ROI Color',value = '#30aa20', showColour = 'background')),
                 column(width = 6, sliderInput(inputId = 'roitrans', label = 'Transparency', min = 0, max = 100, value = 50, ticks = F, width = '100%', pre = '%'))
               )
               ,
               radioButtons('sevenorall', label = 'Time series range:', choices = c('First 7 days','Entire time range'), width = "100%",inline = T),
               actionButton("extract", "Extract GCC", icon = icon('line-chart'), width = "100%"),
               br(),
               br(),
               actionButton("generate", "Generate ROI List", icon = icon('area-chart'), width = "100%")
  ),
  
  
  
  mainPanel(
    sliderInput(inputId = "viewDay", label =  NULL, 
                min = 1, max = 365, ticks = F, 
                value = 50, round = T, step = 1, width = '500px'),
    shiny::fluidRow( 
      column(1, actionButton("back", "", icon = icon('backward'), width = '100%', style="border-color: #fff")),
      column(7, plotOutput("plot", click = "newPoint", width = "350px", height = '260px')),
      column(1, actionButton("forw", "", icon = icon('forward'), width = '100%',  style="border-color: #fff"))
    ),
    actionButton("cancel", "Start over", icon = icon('refresh'), width = "160px"),
    actionButton("undo", "Undo", icon = icon('undo'), width = "160px"),
    actionButton("accept", "Accept", icon = icon('thumbs-up'), width = "160px"),
    hr(),
    fluidRow(sliderInput(inputId = "dateRange", label =  NULL, ticks = F, 
                         min = 1, max = 365, 
                         value = c(70,90), round = T, step = 1, dragRange = T, width = "500px")),
    
    plotOutput(outputId = "timeSeries", height = "200px", width = "500px")
  )
)







server <- function(input, output, session) {
  
  values <- reactiveValues(centers = matrix(numeric(), 0, 2),
                           ROIs = list())
  
  imgFile <- reactive(imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path][1])
  
  isolate({
    dmin <- imgDT[Site==input$site, min(Date)]
    dmax <- imgDT[Site==input$site, max(Date)]
    updateDateRangeInput(session , inputId = 'roiDateRange', min = dmin, max = dmax)  
  })
  
  observe(
               updateSliderInput(session,
                                 inputId = 'viewDay', 
                                 value = imgDT[Site==input$site&Year==input$year, min(DOY)]))
  observeEvent(input$site, 
               {
                 dmin <- imgDT[Site==input$site, min(Date)]
                 dmax <- imgDT[Site==input$site, max(Date)]
                 updateDateRangeInput(session , inputId = 'roiDateRange',
                                      min = dmin, max = dmax,
                                      start = dmin, end = dmax)  
               })
  
  curROI <- reactive({
    if(length(values$ROIs)==0) return(NULL)
    else values$ROIs[[input$rois]]
  })
  
  
  output$plot <- renderPlot({
    
    
    if(is.na(imgFile())){
      par(mar=c(1,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
      text(0.5,0.5, 'No image for this date was found!', font=2, adj=.5)
    }else{
      par(mar=c(1,0,0,0))
      plotJPEG(imgFile())
      trans <- as.hexmode(floor((1-input$roitrans/100)*255))
      if(nchar(trans)==1) trans <- paste0('0',trans)
      polygon(values$centers, col = paste0(input$roicol, trans), pch = 9)
      # polygon(values$centers, col = 'red', pch = 9)
    }
  })
  observeEvent(input$rois, {
    values$centers <- values$ROIs[[input$rois]]
  })
  
  observeEvent(input$newPoint, {
    newPoint <- matrix(c(input$newPoint[['x']], input$newPoint[['y']]),1, 2)
    values$centers <- rbind(values$centers, newPoint)
  })
  
  observeEvent(input$back,
               updateSliderInput(session, "viewDay", value = input$viewDay-1))
  
  observeEvent(input$forw,
               updateSliderInput(session, "viewDay", value = input$viewDay+1))
  
  observeEvent(input$accept,
               {
                 if (nrow(values$centers)<3) return()
                 tmp <- values$ROIs
                 tmp[[length(tmp)+1]] <-  values$centers
                 names(tmp)[length(tmp)] <- paste('ROI', length(tmp), sep = '.')
                 # updateSelectInput(session, inputId = 'rois', choices = names(tmp))
                 values$ROIs <- tmp
               })
  
  observeEvent(input$cancel, 
               values$centers <- matrix(numeric(), 0, 2))
  
  observeEvent(input$undo, {
    if (nrow(values$centers) > 2)
      values$centers <- values$centers[-nrow(values$centers),]
    else if (nrow(values$centers) == 2)
      values$centers <- matrix(values$centers[1,], 1, 2)
    else if (nrow(values$centers) == 1)
      values$centers <- matrix(numeric(), 0, 2)
  })
  
  observeEvent(input$generate,{
    
  })
  
  observe({
    updateSelectInput(session, inputId = 'rois', choices = names(values$ROIs), selected = names(values$ROIs)[length(values$ROIs)])
  })
  observe({
    x <- imgDT[Site==input$site, unique(Year)]
    if (is.null(x)) x <- character(0)
    
    updateSelectInput(session, "year",
                      label = 'Year',
                      choices = x,
                      selected = tail(x, 1))
  })
  
  ccRange <- reactive({
    if(input$sevenorall=="First 7 days")
      return(input$dateRange[1]:(input$dateRange[1]+7))
    else
      return(input$dateRange[1]:input$dateRange[2])
  })
  
  ccVals <- eventReactive(input$extract,{
    if(is.null(curROI())) return(data.frame(rcc=NA, gcc=NA, bcc=NA))
    pnts <- isolate(curROI())
    paths <- imgDT[Site==input$site&Year==input$year&DOY%in%ccRange()&Hour==12&Minute<30, path]
    extractCCCTimeSeries(pnts, paths )
  })
  
  ccTime <- eventReactive(input$extract,{
    if(is.null(curROI())) return(NA)
    if(input$sevenorall=="First 7 days")
      return(imgDT[Site==input$site&Year==input$year&DOY%in%(input$dateRange[1]:(input$dateRange[1]+7))&Hour==12&Minute<30, DOY])
    else 
      return(imgDT[Site==input$site&Year==input$year&DOY%in%(input$dateRange[1]:input$dateRange[2])&Hour==12&Minute<30, DOY])    
  })
  
  output$timeSeries <- 
    renderPlot({
      par(mar=c(4,4,0,0))
      plot(NA, 
           xlim=c(input$dateRange[1], input$dateRange[2]), ylim = c(0.20,.45),
           type = 'l', xlab='', ylab='')
      mtext(side = 1, text = 'Day of year', font=2, line=2.5)
      mtext(side = 2, text = 'GCC (-)', font=2, line=2.5)
      if(input$extract==0) return()
      # imgFile <- imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path][1]
      # if(is.na(imgFile)|input$accept==0|nrow(values$centers)<3) return()
      # if(is.na(imgFile)|input$accept==0|nrow(values$centers)<3|input$extract==0) return()
      
      lines(ccTime(), ccVals()$rcc,  col='red', lwd=2)
      lines(ccTime(), ccVals()$gcc,  col='green', lwd=2)
      lines(ccTime(), ccVals()$bcc,  col='blue', lwd=2)
    })
}

shinyApp(ui, server)
