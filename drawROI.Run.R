# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(raster)
library(jpeg)
library(shiny)
library(lubridate)

source('Resources/SwitchButton.R')

source('drawROI.Funcs.R')
source('drawROI.Init.R')

sites <- unique(imgDT$Site)

ROIs <- list()


ui <- fluidPage(
  
  theme = "button.css",
  # Application title
  fluidRow(titlePanel("PhenoCam ROI Tool")),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(width = 4,
               selectInput("site", "Site", sites),
               selectInput("year", "Year", ''),
               selectInput("rois", "ROI", names(ROIs)),
               dateRangeInput(inputId = 'roiDateRange', label = 'ROI Start/End Dates:', start = '2001-01-01', end = '2016-01-01'),
               
               fixedRow(
                 column(width = 5,textInput("starttime", "Start Time:", value = "99:99:99" )),
                 column(width = 5, offset = 2,textInput("endtime", "End Time:", value = "99:99:99" ))
               )
  ),
  
  mainPanel(
    sliderInput(inputId = "viewDay", label =  "View day", 
                min = 1, max = 365, 
                value = 50, round = T, step = 1, width = '500px'),
    shiny::fluidRow( 
      column(1, actionButton("back", "", icon = icon('backward'), width = '40px', style="border-color: #fff")),
      column(8, plotOutput("plot", click = "newPoint", width = "420px", height = '300px')),
      column(1, actionButton("forw", "", icon = icon('forward'), width = '40px',  style="border-color: #fff"))
    ),
    actionButton("cancel", "Start over", icon = icon('refresh'), width = "160px"),
    actionButton("undo", "Undo", icon = icon('undo'), width = "160px"),
    actionButton("accept", "Accept", icon = icon('thumbs-up'), width = "160px"),
    br(),
    HTML("<hr>"),
    fluidRow(
      column(radioButtons('sevenorall', label = 'Time series range:', choices = c('First 7 days','Entire time range'), width = "200px",inline = F), width = 4),
      fluidRow(column(actionButton("extract", "Extract GCC", icon = icon('line-chart'), width = "160px"), width = 4),
               br(),
               br(),
               column(actionButton("generate", "Generate", icon = icon('area-chart'), width = "160px"), width = 4 ))
    ),
    br(),
    fluidRow(sliderInput(inputId = "dateRange", label =  "Time series range", 
                         min = 1, max = 365, 
                         value = c(70,90), round = T, step = 1, dragRange = T, width = "500px")),
    plotOutput(outputId = "timeSeries", height = "200px", width = "500px")
  )
)







server <- function(input, output, session) {
  
  values <- reactiveValues(centers = matrix(numeric(), 0, 2))
  # output$ROIs <- render
  
  ROI <- reactive({
    if(length(ROIs)==0) return(NULL)
    else ROIs[[input$rois]]
  })
  
  output$plot <- renderPlot({
    imgFile <- imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path][1]
    
    if(is.na(imgFile)){
      par(mar=c(1,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
      text(0.5,0.5, 'No image for this date was found!', font=2, adj=.5)
    }else{
      par(mar=c(1,0,0,0))
      plotJPEG(imgFile)
      polygon(values$centers, col = '#30aa2080', pch = 9)
    }
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
                 
                 ROIs[[length(ROIs)+1]] <-  values$centers
                 names(ROIs)[length(ROIs)] <- paste('ROI', length(ROIs))
                 updateSelectInput(session, inputId = 'rois', choices = names(ROIs))
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
    if(is.null(ROI())) return(data.frame(rcc=NA, gcc=NA, bcc=NA))
    pnts <- isolate(ROI())
    paths <- imgDT[Site==input$site&Year==input$year&DOY%in%ccRange()&Hour==12&Minute<30, path]
    extractCCCTimeSeries(pnts, paths )
  })
  
  ccTime <- eventReactive(input$extract,{
    if(is.null(ROI())) return(NA)
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
