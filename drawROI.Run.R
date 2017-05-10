# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(raster)
library(jpeg)
library(shiny)
library(lubridate)

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
                value = 50, round = T, step = 1),
    
    actionButton(inputId = 'go',label = " Extract the time series", icon("refresh")),
    actionButton(inputId = 'draw',label = "Let's draw ROI", icon("pencil"))
  ),
  
  mainPanel(
    plotOutput(outputId = "imagePlot", width = "500px"),
    plotOutput(outputId = "timeSeries", height = "200px", width = "500px")
  )
)

server <- function(input, output, session) {
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
  
  output$imagePlot <- renderPlot({
    tIndex <- input$daterange[1]
    imgFile <- imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path]
    if(length(imgFile)>1) imgFile <- imgFile[1]
    
    if(length(imgFile)==0){
      par(mar=c(0,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
      text(0.5,0.5, 'No image for this date was found!', font=2, adj=.5)
    }else{
      par(mar=c(0,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1),
           xaxt='n', yaxt='n',
           xaxs = 'i', yaxs='i',
           xlab='',ylab='',
           bty='o')
      rasterImage(readJPEG(imgFile), 0,0,1,1)
      x <- draw.polygon()
      input$draw
      text(0.5,0.5, 'TEST', font=2, adj=.5, col='white')
    }
    # plot(NA, xlim=c(0,1), ylim=c(0,1))
    # text(0,0,labels=imgFile)
    
  })
  
  output$timeSeries <- renderPlot({
    par(mar=c(4,4,1,1))
    input$go
    x <- input$dateRange[1]:input$dateRange[2]
    plot(x, cumsum(runif(length(x))),  type = 'l', xlab='', ylab='')
    mtext(side = 1, text = 'Day of year', font=2, line=2.5)
    mtext(side = 2, text = 'GCC (-)', font=2, line=2.5)
  })
  
}


shinyApp(ui, server)

