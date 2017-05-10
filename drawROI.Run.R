# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(raster)
library(jpeg)
library(shiny)
library(lubridate)

source('drawROI.Funcs.R')

ui <- fluidPage(
  
  # Application title
  titlePanel("PhenoCam ROI Tool"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    selectInput("site", "Site", sites),
    selectInput("year", "Year", ''),
    sliderInput(inputId = "dateRange", label =  "time series range", 
                min = 1, max = 365, 
                value = c(50,350), round = T, step = 1, dragRange = T),
    sliderInput(inputId = "viewDay", label =  "View day", 
                min = 1, max = 365, 
                value = 50, round = T, step = 1)
    
    
  ),
  
  mainPanel(
    plotOutput("plot", click = "newCenter", width = "500px"),
    actionButton("accept", "Accept"),
    actionButton("undo", "Undo"),
    actionButton("cancel", "Cancel"),
    plotOutput(outputId = "timeSeries", height = "200px", width = "500px")
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(centers = matrix(numeric(), 0, 2))
  
  
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
  
  observe({
    if (is.null(input$newCenter))
      return()
    isolate({
      newCenter <- matrix(c(input$newCenter[['x']], input$newCenter[['y']]),
                          1, 2)
      values$centers <- rbind(values$centers, newCenter)
    })
  })
  
  observe({
    if (input$accept == 0)
      return()
    if (values$centers <3 )
      return()
    
    #stopApp(clusters())
  })
  
  observe({
    if (input$cancel == 0)
      return()
    values$centers <- matrix(numeric(), 0, 2)
  })
  
  observe({
    if (input$undo == 0)
      return()
    isolate({
      if (nrow(values$centers) > 2)
        values$centers <- values$centers[-nrow(values$centers),]
      else if (nrow(values$centers) == 2)
        values$centers <- matrix(values$centers[1,], 1, 2)
      else if (nrow(values$centers) == 1)
        values$centers <- matrix(numeric(), 0, 2)
    })
  })
  
  
  observe({
    x <- imgDT[Site==input$site, unique(Year)]
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "year",
                      label = 'Year',
                      choices = x,
                      selected = tail(x, 1)
    )
  })
  # observe({
  #   updateSelectInput(session, "viewDay",
  #                     label = 'View day',
  #                     min = input$dateRange[1], max = input$dateRange[2], 
  #                     value = input$dateRange[1], round = T, step = 1
  #   )
  # })
  
  
  output$timeSeries <- renderPlot({
    par(mar=c(4,4,3,1))
    plot(NA, 
         xlim=c(input$dateRange[1], input$dateRange[2]), ylim = c(0,1),
         type = 'l', xlab='', ylab='')
    mtext(side = 1, text = 'Day of year', font=2, line=2.5)
    mtext(side = 2, text = 'GCC (-)', font=2, line=2.5)
    
    imgFile <- imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path][1]
    
    if(is.na(imgFile)|input$accept==0) return()

    # x <- input$dateRange[1]:input$dateRange[2]
    paths <- imgDT[Site==input$site&Year==input$year&DOY%in%(input$dateRange[1]:input$dateRange[2])&Hour==12&Minute<30, path]
    t <- imgDT[Site==input$site&Year==input$year&DOY%in%(input$dateRange[1]:input$dateRange[2])&Hour==12&Minute<30, DOY]
    pnts <- values$centers
    vals <- extractCCCTimeSeries(pnts, paths )
    lines(t, vals$rcc,  col='red')
    lines(t, vals$gcc,  col='green')
    lines(t, vals$bcc,  col='blue')
    input$accept==0
  })
  
}


shinyApp(ui, server)

