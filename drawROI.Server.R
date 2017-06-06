# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(sp)
library(raster)
library(jpeg)
library(shiny)
library(shinyTime)
library(lubridate)
library(data.table)
library(colourpicker)

source('Resources/SwitchButton.R')

source('drawROI.Funcs.R')
source('drawROI.Init.R')


source('drawROI.UI.R')

# source('drawROI.Server.R')
server <- function(input, output, session) {
  
  values <- reactiveValues(centers = matrix(numeric(), 0, 2),
                           MASKs = list(),
                           slideShow = 0,
                           contID= 1
  )
  
  dayYearIDTable <- reactive(imgDT[Site==input$site&Hour==12&Minute<30,.(ID=1:.N,Year, DOY)])
  
  roipath <- reactive(paste0('phenocamdata/',input$site,'/ROI/'))
  nroi <- reactive({
    length(dir(roipath(), pattern = '*roi.csv'))+1
  })
  
  output$roilabel <- renderText({
    paste(input$site, 
          input$vegtype, 
          sprintf('%04d',nroi()), 
          'roi.csv',
          sep = '_')
  })
  
  
  sampleImage <- reactive(
    imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path][1]
  )
  
  sampleImageName <- reactive({
    tmp <- unlist(strsplit(sampleImage(), split = '/'))
    tmp[length(tmp)]
  })
  
  output$sampleImagePath <- renderText(
    sampleImageName()
  )
  
  observeEvent(values$contID,{
    tmpid <- dayYearIDTable()[ID==as.numeric(values$contID),ID]
    tmpday <- dayYearIDTable()[ID==as.numeric(values$contID),DOY]
    tmpyear <- dayYearIDTable()[ID==as.numeric(values$contID),Year]
    
    updateSliderInput(session, 'contID', value = tmpid)
    updateSliderInput(session, 'viewDay', value = tmpday)
    updateSelectInput(session, 'year', selected = tmpyear)
  })
  
  observe({
    tmpid <- dayYearIDTable()[Year==as.numeric(input$year)&DOY==as.numeric(input$viewDay), ID]
    # updateSliderInput(session, "contID", value = tmpid)
    values$contID <- tmpid
  })
  
  observeEvent(input$contID, 
               values$contID <- input$contID)
  
  # observeEvent(input$viewDay,{
  observeEvent(values$contID,{
  # observe({
    if(values$slideShow==0) return()
    values$contID <- as.numeric(values$contID) + as.numeric(values$slideShow)
    # print(paste(input$contID,values$slideShow, values$contID))
    # updateSliderInput(session, "contID", value = as.numeric(input$contID) + values$slideShow)
  })
  
  observeEvent(input$pause, values$slideShow <- 0)
  observeEvent(input$play, values$slideShow <- 1)
  observeEvent(input$backplay, values$slideShow <- -1)
  
  observeEvent(input$back,
               updateSliderInput(session, "contID", value = input$contID-1)
               )
  
  observeEvent(input$forw,
               updateSliderInput(session, "contID", value = input$contID+1))
  
  
  observeEvent(input$site, 
               {
                 updateSliderInput(session,
                                   inputId = 'contID',
                                   value = 1,
                                   min= min(dayYearIDTable()$ID),
                                   max= max(dayYearIDTable()$ID)  )
                 
                 dmin <- imgDT[Site==input$site, min(Date)]
                 dmax <- imgDT[Site==input$site, max(Date)]
                 updateDateRangeInput(session ,
                                      inputId = 'roiDateRange',
                                      # min = dmin, 
                                      # max = dmax,
                                      start = dmin,
                                      end = dmax)  
                 
                 x <- imgDT[Site==input$site, unique(Year)]
                 if (is.null(x)) x <- character(0)
                 
                 updateSelectInput(session, "year", choices = x)
                 
               })
  
  curMask <- eventReactive(input$masks,
                           {
                             if(length(values$MASKs)==0) return(NULL)
                             else values$MASKs[[input$masks]]$rasteredMask
                           })
  
  output$plot <- renderPlot({
    if(is.na(sampleImage())){
      par(mar=c(0,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
      text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No image for this date was found!', font=2, adj=.5)
    }else{
      par(mar=c(0,0,0,0))
      plotJPEG(sampleImage())
      trans <- as.hexmode(floor((1-input$roitrans/100)*255))
      if(nchar(trans)==1) trans <- paste0('0',trans)
      polygon(values$centers, col = paste0(input$roicol, trans), pch = 9)
    }
  })
  
  observeEvent(input$masks, {
    tmpmask <- values$MASKs[[input$masks]]
    
    values$centers <- tmpmask$maskpoints
    updateDateRangeInput(session, inputId = 'roiDateRange', start=tmpmask$startdate)
    updateDateRangeInput(session, inputId = 'roiDateRange', end=tmpmask$enddate)
    updateTimeInput(session, inputId = 'starttime', value = tmpmask$starttime)
    updateTimeInput(session, inputId = 'endtime', value = tmpmask$endtime)
    updateSelectInput(session, inputId = 'year', selected = tmpmask$sampleyear)
    updateSelectInput(session, inputId = 'viewDay', selected = tmpmask$sampleday)
  })
  
  observeEvent(input$newPoint, {
    newPoint <- matrix(c(input$newPoint[['x']], input$newPoint[['y']]),1, 2)
    values$centers <- rbind(values$centers, newPoint)
  })
  
  
  observeEvent(input$save,{
    
  })
  
  observeEvent(input$accept,
               {
                 if(is.null(values$centers)) return()
                 if (nrow(values$centers)<3) return()
                 
                 newMask <- list(maskpoints = values$centers, 
                                 startdate = input$roiDateRange[1], 
                                 enddate = input$roiDateRange[2], 
                                 starttime = input$starttime, 
                                 endtime = input$endtime, 
                                 sampleyear = input$year, 
                                 sampleday = input$viewDay,
                                 sampleImage = sampleImageName(),
                                 rasteredMask = createRasteredROI(values$centers, sampleImage()))
                 
                 tmp <- values$MASKs
                 tmp[[length(tmp)+1]] <-  newMask
                 names(tmp)[length(tmp)] <- paste(input$site, input$vegtype, 
                                                  sprintf('%04d',nroi()),
                                                  sprintf('%02d',length(tmp)), sep = '_')
                 updateSelectInput(session, inputId = 'masks', choices = names(tmp))
                 values$MASKs <- tmp
                 # values$msk <- tmp$rasteredMask
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
    if(length(values$MASKs)==0) return()
    systime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    ROIList <- list(siteName = input$site, 
                    vegType = input$vegtype, 
                    ID = nroi(),
                    Owner= input$owner, 
                    Description = input$descr, 
                    createTime = systime, 
                    updateTime = systime, 
                    masks = values$MASKs)
    
    writeROIListFile(ROIList, path = roipath() )
    
    showModal(modalDialog(title = 'Complete',width='300px',
                          "New file for ROI List was generated!",
                          easyClose = T,
                          size = 's',
                          # footer = modalButton('OK')
                          footer = NULL
    ))
  })
  
  observe({
    updateSelectInput(session, inputId = 'masks', choices = names(values$MASKs), selected = names(values$MASKs)[length(values$MASKs)])
  })
  
  tsDayRange <- reactive({
    if(input$sevenorall=="7 days")
      return(input$dateRange[1]:(input$dateRange[1]+7))
    else
      return(input$dateRange[1]:input$dateRange[2])
  })
  
  paths <- reactive(
    imgDT[Site==input$site&Year==input$year&DOY%in%tsDayRange()&Hour==12&Minute<30, .(paths=path[1]),DOY]
  )
  
  ccVals <- eventReactive(input$extract,{
    if(is.null(curMask())|length(paths()$path)==0) return(data.frame(rcc=NA, gcc=NA, bcc=NA))
    extractCCCTimeSeries(isolate(curMask()), paths()$path)
  })
  
  ccTime <- eventReactive(input$extract,{
    if(is.null(curMask())) return(NA)
    if(input$sevenorall=="First 7 days")
      return(paths()[DOY%in%tsDayRange(),DOY])
    else 
      return(paths()$DOY)
  })
  output$timeSeriesPlotly <- 
    renderPlotly({
      tvals <- ccTime()
      cvals <- ccVals()
      cc <- melt(data.frame(red= cvals$rcc, green = cvals$gcc, blue= cvals$bcc), 
                 variable.name='band', value.name='cc', id.vars=NULL)
      
      d <- data.table(time=tvals, cc)
      d <- d[band%in%tolower(input$ccselect)]
      plot_ly(data = d, x=~time, y= ~cc, color = ~band)
    })
  
  output$timeSeries <- 
    renderPlot({
      par(mar=c(4,4,0,0))
      plot(NA, 
           xlim=c(input$dateRange[1], input$dateRange[2]), 
           ylim = input$ccrange,
           type = 'l', xlab='', ylab='')
      mtext(side = 1, text = 'Day of year', font=2, line=2.5)
      mtext(side = 2, text = 'GCC (-)', font=2, line=2.5)
      if(input$extract==0) {
        text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No time series was generated!', font=2, adj=.5)
        return()
      }
      TS <- ccVals()
      if(nrow(TS)==0){
        text(mean(par()$usr[1:2]), mean(par()$usr[3:4]),  'No time series was generated!', font=2, adj=.5)
        return()
      }
      if('Red'%in%input$ccselect) lines(ccTime(), TS$rcc,  col='red', lwd=2)
      if('Green'%in%input$ccselect) lines(ccTime(), TS$gcc,  col='green', lwd=2)
      if('Blue'%in%input$ccselect) lines(ccTime(), TS$bcc,  col='blue', lwd=2)
    })
  
  
  output$maskplot <- 
    renderPlot({
      par(mar=c(0,0,0,0))
      plot(1,
           type='n',
           xaxs='i',yaxs='i',
           xaxt='n',yaxt='n',
           xlab='',ylab='',
           bty='o')

      
      # if(length(values$MASKs)==0){
      #   text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No mask was generated!', font=2, adj=.5)
      #   return()
      # }
      
      if(is.null(curMask())) {
        text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No mask was generated!', font=2, adj=.5)
        
        return()
      }
      mask <- curMask()
      res <- dim(mask)
      plot(NA,xlim=c(1,res[1]),ylim=c(1,res[2]), type='n',
           xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
      plot(mask,legend=F, add=T, col='black')
      # }
    })
}

shinyApp(ui, server)
