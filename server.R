library(shiny)
library(shinyTime)
library(shinyjs)
library(shinyBS)
library(shinydashboard)

library(colourpicker)
library(rjson)
library(stringr)
library(sendmailR)
library(shinyAce)

library(sp)
library(raster)
library(jpeg)
library(tiff)

library(data.table)
library(lubridate)
library(plotly)

source('funcs.R')


midddayListPath <- '/home/bijan/middayList/'
bijanWD <- "/Users/bijan/Projects/drawROI"

if(getwd()==bijanWD) 
  midddayListPath <- 'midddayListPath/'


shinyServer(function(input, output, session) {
  options(warn = -1)
  values <- reactiveValues(centers = matrix(numeric(), 0, 2),
                           MASKs = list(),
                           slideShow = 0,
                           ROIs = vector(),
                           sitesList = vector(),
                           parsedROIList = NULL,
                           phenoSites = fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/')
  )
  
  autoInvalidate <- reactiveTimer(1000)
  
  observe({
    vegTypes <-   list('AG','DB','EB','EN','DN','GR','MX',
                       'NV','RF','SH','TN','UN','WL','XX')
    names(vegTypes) <- c('Agriculture (AG)',
                         'Deciduous Broadleaf (DB)',
                         'Evergreen Broadleaf (EB)',
                         'Evergreen Needleleaf (EN)',
                         'Deciduous Needleleaf (DN)',
                         'Grassland (GR)',
                         'Mixed Forest (MX)',
                         'Non-vegetated (NV)',
                         'Reference Panel (RF)',
                         'Shrub (SH)',
                         'Tundra (TN)',
                         'Understory (UN)',
                         'Wetland (WL)',
                         'Other/Canopy (XX)')
    updateSelectInput(session, 'vegType', choices = vegTypes)    
  })
  
  
  # ----------------------------------------------------------------------
  # imgDT
  # ----------------------------------------------------------------------
  imgDT <- reactive({
    dummy <- 0
    getIMG.DT(input$siteName, midddayListPath)
    
  })
  
  
  
  # ----------------------------------------------------------------------
  # Site
  # ----------------------------------------------------------------------
  observe({
    phenoSitesList <- sapply(values$phenoSites, function(x){x$site})
    names(values$phenoSites) <- phenoSitesList
    phenoSitesList <- phenoSitesList[-which(phenoSitesList=='HF_Vivotek')]
    if(getwd()==bijanWD) 
      phenoSitesList <- c('acadia','dukehw','harvard')
    
    values$sitesList <- phenoSitesList
    
  })
  
  observe({
    updateSelectInput(session, inputId = 'siteName', choices = values$sitesList)
    updateSelectInput(session, inputId = 'errorSite', choices = values$sitesList)
    if(getwd()!=bijanWD) 
      updateSelectInput(session, inputId = 'siteName', selected = 'ahwahnee')
  })
  
  observeEvent(input$siteName, {
    dummy = 0
    updateSelectInput(session, inputId = 'errorSite', selected = input$siteName)
    values$slideShow <- 0
    
    updateSliderInput(session,
                      inputId = 'contID',
                      value = 1,
                      min= min(dayYearIDTable()$ID),
                      max= max(dayYearIDTable()$ID)  )
    
    dmin <- imgDT()[Site==input$siteName, min(Date)]
    dmax <- imgDT()[Site==input$siteName, max(Date)]
    updateDateRangeInput(session ,
                         inputId = 'roiDateRange',
                         start = dmin,
                         end = dmax)  
    
    x <- imgDT()[Site==input$siteName, unique(Year)]
    if (is.null(x)) x <- character(0)
    
    updateSelectInput(session, "year", choices = x)
    updateSelectInput(session, 'roiName', choices = values$ROIs, selected = 'New ROI')
    dummy <- 0
    values$MASKs <- list()
    updateSelectInput(session, inputId = 'maskName', choices = 'New mask')
  })
  
  siteInfo <- reactive({
    dummy <- 0
    values$phenoSites[[input$siteName]]
  })
  
  observeEvent(input$nextSite, {
    dummy <- 0
    w <- which(values$sitesList==input$siteName)
    wNext <- w + 1
    if (w==length(values$sitesList)) wNext= 1
    nextSite <- values$sitesList[wNext]
    updateSelectInput(session, 'siteName', selected = nextSite)
  })
  # ----------------------------------------------------------------------
  # Site info
  # ----------------------------------------------------------------------
  output$tblSiteInfo = renderTable( rownames = T, 
                                    colnames = F, 
                                    striped = T, 
                                    hover = T, 
                                    bordered = F,
                                    spacing = 's',
                                    options = list( lengthChange = FALSE),{
                                      dummy <- 1
                                      dummy <- 1
                                      inf <- siteInfo()
                                      x <- t(data.frame(Site = inf$site, 
                                                        Site.Type = inf$site_type,
                                                        MAT = paste0(inf$MAT_worldclim, ' °C'),
                                                        MAP = paste0(inf$MAP_worldclim, ' mm/year'),
                                                        Koeppen.Class = inf$koeppen_geiger,
                                                        Latitude = paste0(inf$lat, ' °'),
                                                        Longitude = paste0(inf$lon, ' °'),
                                                        Elevation = paste0(inf$elev, ' m'),
                                                        Descriotion = inf$site_description,
                                                        Primary.Vegetation = inf$primary_veg_type
                                      ))
                                      x
                                    })
  
  # ----------------------------------------------------------------------
  # ROIs
  # ----------------------------------------------------------------------
  roipath <- reactive({
    tmp <- (paste0('/data/archive/', input$siteName,'/ROI/'))
    
    if(getwd()==bijanWD) 
      tmp <- (paste0('phenocamdata/data/archive/', input$siteName,'/ROI/'))
    return(tmp)
  }  )
  
  observe(
    values$ROIs <- c(dir(roipath(), pattern = 'roi.csv$'), "New ROI")
  )
  
  observe(
    updateSelectInput(session, 'roiName', choices = values$ROIs, selected = 'New ROI')
  )
  
  
  # ----------------------------------------------------------------------
  # ROI label
  # ----------------------------------------------------------------------
  roiLabel <- reactive({
    dummy = 0 
    label <- paste(input$siteName, 
                   input$vegType, 
                   sprintf('%04d',roiID()), sep = '_')
    label
  }
  )
  
  output$roiFileName <- renderText({
    if(input$roiName=='New ROI')
      paste0(roiLabel(),'_roi.csv')
    else
      input$roiName
  })
  
  
  
  # ----------------------------------------------------------------------
  # Parsed ROI List
  # ----------------------------------------------------------------------
  observeEvent(input$roiName,{
    dummy = 0
    values$slideShow <- 0 
    
    if(input$roiName=='New ROI') {
      shinyjs::enable('vegType')
      dummy =0 
      values$MASKs <- list()
      updateSelectInput(session, inputId = 'maskName', choices = 'New mask')
      
      return()
    }
    shinyjs::disable('vegType')
    dummy=0
    dummy=0
    values$parsedROIList <- parseROI(roifilename=input$roiName,
                                     roipath = roipath())
    
    updateSelectInput(session, inputId = 'vegType', selected =  values$parsedROIList$vegType)
    updateTextInput(session, inputId = 'siteDescription', value = values$parsedROIList$Description)
    updateTextInput(session, inputId = 'roiOwner', value = values$parsedROIList$Owner)
    dummy=0
    
    values$MASKs <- values$parsedROIList$masks
    
    updateSelectInput(session, inputId = 'maskName', choices = c(names(values$MASKs), 'New mask'))
    
    updateSelectInput(session, inputId = 'year', selected = values$parsedROIList$masks[[1]]$sampleyear)
    updateSelectInput(session, inputId = 'viewDay', selected = values$parsedROIList$masks[[1]]$sampleday)
    dummy =0
  })
  
  nroi <- reactive({
    autoInvalidate()
    tmpl <- paste0(input$siteName, '_', input$vegType)
    sum(grepl(tmpl, values$ROIs))+1
  })
  
  
  roiID <- reactive({
    if(input$roiName=='New ROI') {
      return(nroi())
    }
    else {
      dummy = 0 
      values$parsedROIList$ID
    }
  })
  curMask <- reactive({
    if(input$maskName=='New mask') {
      return(NULL)
    }
    values$MASKs[[input$maskName]]$rasteredMask
  })
  
  
  # ----------------------------------------------------------------------
  # MASKs
  # ----------------------------------------------------------------------
  observeEvent(values$MASKs,{
    if(length(values$MASKs)==0) {
      shinyjs::disable("downloadROI")
      shinyjs::disable("emailROI")
    }else{
      shinyjs::enable("downloadROI")
      shinyjs::enable("emailROI")
    }
  })
  
  
  # ----------------------------------------------------------------------
  # VegType
  # ----------------------------------------------------------------------
  observeEvent(input$vegType,{
    values$slideShow <- 0 
    if(length(values$MASKs)==0) return()
    
    maskNames <- names(values$MASKs)
    f <- function(x, y){
      z <- unlist(strsplit(x, '_'))
      paste(c(z[1], y, z[3:4]), collapse = '_')
    }
    
    newmaskNames <- as.vector(sapply(maskNames, f, y = input$vegType))
    if(!is.null(values$MASKs))names(values$MASKs) <- newmaskNames
    updateSelectInput(session, inputId = 'maskName', choices = c(names(values$MASKs), 'New mask'))
  })
  
  
  
  # ----------------------------------------------------------------------
  # Mask start and end time
  # ----------------------------------------------------------------------
  observeEvent(input$maskStartTime, 
               {
                 asText <- input$maskStartTime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'maskStartTime', value = asTextNew)
               })
  
  observeEvent(input$maskEndTime, 
               {
                 asText <- input$maskEndTime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'maskEndTime', value = asTextNew)
               })
  
  
  dayYearIDTable <- reactive({
    dummy <- 0
    imgDT()[Site==input$siteName,.(ID=1:.N,Year, DOY)]
  }    )
  
  
  # ----------------------------------------------------------------------
  # Slideshow
  # ----------------------------------------------------------------------
  observe({
    if(values$slideShow==0) return()
    updateSliderInput(session,
                      inputId = 'contID',
                      value = as.numeric(input$contID) + values$slideShow)
  })
  
  observeEvent(input$pause, {
    values$slideShow <- 0
  })
  
  observeEvent(input$play, {
    values$slideShow <- 1
  })
  
  observeEvent(input$backplay, {
    values$slideShow <- -1
  })
  
  observeEvent(input$back, {
    values$slideShow <- 0 
    updateSliderInput(session, "contID", value = input$contID-1)
  })
  
  observeEvent(input$forw, {
    values$slideShow <- 0 
    updateSliderInput(session, "contID", value = input$contID+1)
  })
  
  
  # ----------------------------------------------------------------------
  # Sample Image
  # ----------------------------------------------------------------------
  sampleImage <- reactive({
    imgDT()[Site==input$siteName&Year==yearID()&DOY==doyID(), path][1]
  }  )
  
  sampleImageSize <- reactive(
    dim(readJPEG(sampleImage()))[1:2]
  )
  
  sampleImageName <- reactive({
    tmp <- unlist(strsplit(sampleImage(), split = '/'))
    tmp[length(tmp)]
  })
  
  output$sampleImagePath <- renderText(
    sampleImageName()
  )
  
  observeEvent(input$matchStart, {
    tmp <- unlist(strsplit(sampleImageName(), '_'))
    startDate <- as.Date(paste(tmp[2:4], collapse = '-'))
    HHMMSS <- gsub(tmp[5], pattern = '.jpg',replacement = '')
    startTime <- paste(substring(HHMMSS, c(1,3,5), c(2,4,6)), collapse = ':')
    
    updateDateRangeInput(session, inputId = 'roiDateRange', start = startDate)
    updateTextInput(session, inputId = 'maskStartTime', value = startTime)
  })
  
  observeEvent(input$matchEnd, {
    tmp <- unlist(strsplit(sampleImageName(), '_'))
    endDate <- as.Date(paste(tmp[2:4], collapse = '-'))
    HHMMSS <- gsub(tmp[5], pattern = '.jpg',replacement = '')
    endTime <- paste(substring(HHMMSS, c(1,3,5), c(2,4,6)), collapse = ':')
    
    updateDateRangeInput(session, inputId = 'roiDateRange', end = endDate)
    updateTextInput(session, inputId = 'maskEndTime', value = endTime)
  })
  
  
  
  doyID <- reactive(
    dayYearIDTable()[ID==as.numeric(input$contID),DOY]
  )
  yearID <- reactive(
    dayYearIDTable()[ID==as.numeric(input$contID),Year]
  )
  output$yearOut <- renderText({
    paste0('    Year:  ', yearID())
  })
  
  output$doyOut <- renderText({
    paste0('    DOY:  ', doyID())
  })
  
  
  
  # ----------------------------------------------------------------------
  # Plot image
  # ----------------------------------------------------------------------
  output$imagePlot <- renderPlot({
    if(is.na(sampleImage())){
      par(mar=c(0,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
      text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No image for this date was found!', font=2, adj=.5)
    }else{
      par(mar=c(0,0,0,0))
      plotJPEG(sampleImage())
      roiColors <- if (input$roiColors=='transparent') '#ffffff00' else paste0(input$roiColors, '80')
      dummy <- 0
      if(is.null(values$centers)) 
        absPoints <- matrix(numeric(), 0, 2)
      else if(nrow(values$centers)==0) 
        absPoints <- matrix(numeric(), 0, 2)
      else if(nrow(values$centers)==1) 
        absPoints <- values$centers*sampleImageSize()
      else 
        absPoints <- t(apply(values$centers, 1, '*', sampleImageSize()))
      dummy <- 0
      polygon(absPoints, col = roiColors, pch = 9, lwd=2)
    }
  })
  
  observeEvent(input$maskName, {
    values$slideShow <- 0 
    if(input$maskName=='New mask') {
      # values$MASKs <- list()
      # values$centers <- matrix(numeric(), 0, 2)
      return()
    }
    tmpmask <- values$MASKs[[input$maskName]]
    
    values$centers <- tmpmask$maskpoints
    updateDateRangeInput(session, inputId = 'roiDateRange', start=tmpmask$startdate)
    updateDateRangeInput(session, inputId = 'roiDateRange', end=tmpmask$enddate)
    updateTextInput(session, inputId = 'maskStartTime', value = tmpmask$starttime)
    updateTextInput(session, inputId = 'maskEndTime', value = tmpmask$endtime)
    
    tmpID <- dayYearIDTable()[Year==as.numeric(yearID())&DOY==as.numeric(doyID()), ID]
    if(length(tmpID)==0) tmpID <- 1
    values$ID <- tmpID[1]
    # updateSelectInput(session, inputId = 'year', selected = tmpmask$sampleyear)
    # updateSelectInput(session, inputId = 'viewDay', selected = tmpmask$sampleday)
  })
  
  observeEvent(input$newPoint, {
    values$slideShow <- 0 
    newPoint <- matrix(c(input$newPoint[['x']], input$newPoint[['y']]),1, 2)
    values$centers <- rbind(values$centers, newPoint/sampleImageSize())
  })
  
  
  
  observeEvent(input$clearCanvas, {
    values$slideShow <- 0 
    values$centers <- matrix(numeric(), 0, 2)
  })
  
  observeEvent(input$undoCanvas, {
    values$slideShow <- 0 
    if (nrow(values$centers) > 2)
      values$centers <- values$centers[-nrow(values$centers),]
    else if (nrow(values$centers) == 2)
      values$centers <- matrix(values$centers[1,], 1, 2)
    else if (nrow(values$centers) == 1)
      values$centers <- matrix(numeric(), 0, 2)
  })
  
  # ----------------------------------------------------------------------
  # Save ROI List
  # ----------------------------------------------------------------------
  observeEvent(input$saveROI,{
    values$slideShow <- 0 
    if(length(values$MASKs)==0) return()
    systime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    ROIList <- list(siteName = input$siteName, 
                    vegType = input$vegType, 
                    ID = roiID(),
                    Owner= input$roiOwner, 
                    Description = input$siteDescription, 
                    createDate = strftime(systime, format = '%Y-%m-%d'),
                    createTime = strftime(systime, format = '%H:%M:%S'),
                    updateDate = strftime(systime, format = '%Y-%m-%d'),
                    updateTime = strftime(systime, format = '%H:%M:%S'),
                    masks = values$MASKs)
    
    
    roifilename <- paste0(roiLabel(),'_roi.csv')
    writeROIListFile(ROIList, path = roipath(),  roifilename)
    
    showModal(modalDialog(title = 'Complete',width='300px',
                          "New file for ROI List was saved in the database!",
                          easyClose = T,
                          size = 's',
                          footer = NULL
    ))
  })
  
  
  
  # ----------------------------------------------------------------------
  # Download ROI List
  # ----------------------------------------------------------------------
  output$downloadROI <- downloadHandler(
    filename = function(){
      make.names(paste0(input$roiOwner, '_',roiLabel(),'_roi.zip'))
    },
    content = function(fname){
      
      wd <- getwd()
      
      setwd(tempdir())
      print(tempdir())
      
      systime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      ROIList <- list(siteName = input$siteName, 
                      vegType = input$vegType, 
                      ID = roiID(),
                      Owner= input$roiOwner, 
                      Description = input$siteDescription, 
                      createDate = strftime(systime, format = '%Y-%m-%d'),
                      createTime = strftime(systime, format = '%H:%M:%S'),
                      updateDate = strftime(systime, format = '%Y-%m-%d'),
                      updateTime = strftime(systime, format = '%H:%M:%S'),
                      masks = values$MASKs)
      
      roifilename <- paste0(roiLabel(),'_roi.csv')
      writeROIListFile(ROIList, path = '',  roifilename)
      fs <- c(roifilename, 
              paste0(names(ROIList$masks), '.tif'),
              paste0(names(ROIList$masks), '_vector.csv'))
      zip(zipfile=fname, files=fs)
      setwd(wd)
    },
    contentType = "application/zip"
  )
  
  
  
  
  # ----------------------------------------------------------------------
  # Email ROI List
  # ----------------------------------------------------------------------
  observeEvent(input$emailROI,{
    values$slideShow <- 0 
    if(length(values$MASKs)==0) return()
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print(tempdir())
    
    systime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    ROIList <- list(siteName = input$siteName, 
                    vegType = input$vegType, 
                    ID = roiID(),
                    Owner= input$roiOwner, 
                    Description = input$siteDescription, 
                    createDate = strftime(systime, format = '%Y-%m-%d'),
                    createTime = strftime(systime, format = '%H:%M:%S'),
                    updateDate = strftime(systime, format = '%Y-%m-%d'),
                    updateTime = strftime(systime, format = '%H:%M:%S'),
                    masks = values$MASKs)
    
    roifilename <- paste0(roiLabel(),'_roi.csv')
    writeROIListFile(ROIList, path = '',  roifilename)
    fs <- c(roifilename, 
            paste0(names(ROIList$masks), '.tif'),
            paste0(names(ROIList$masks), '_vector.csv'))
    fname <- make.names(paste0(input$roiOwner, '_',roiLabel(),'_roi.zip'))
    
    zip(zipfile=fname, files=fs)
    
    msg <- paste0(
      '---------\n',
      'Submit time: \t', as.character(Sys.time()), '\n',
      # 'IP address: \t', as.character(system('ipconfig getifaddr en0', intern=TRUE)),'\n',
      'Site: \t', input$siteName, '\n',
      'Owner: \t', input$roiOwner, '\n',
      '---------\n'
    )
    attachmentObject <- mime_part(x = fname, name = fname)
    bodyWithAttachment <- list(msg, attachmentObject)
    
    
    sendmail(from = 'phenocam.network@gmail.com', 
             to = 'phenocam.network@gmail.com', 
             subject = 'New ROI was just submitted via drawROI!', 
             msg = bodyWithAttachment)
    
    showModal(modalDialog(title = 'ROI was submitted!',width='250px',
                          "The new ROI will be reviewed shortly.",
                          easyClose = T,
                          size = 's',
                          footer = NULL
    ))
    
    
  })
  
  
  # ----------------------------------------------------------------------
  # tsYearDayRange
  # ----------------------------------------------------------------------
  
  
  tsYearDayRange <- reactive({
    if(input$ccRange=="week")
      return(imgDT()[Site==input$siteName&Year==yearID()&DOY%in%(doyID()[1]:(doyID()[1]+7)),YearDOY])
    else if(input$ccRange=="year")
      # return(1:365)
      return(imgDT()[Site==input$siteName&Year==yearID(),YearDOY])
    else if(input$ccRange=="all")
      return(imgDT()[Site==input$siteName,YearDOY])
  })
  
  paths <- reactive(
    imgDT()[Site==input$siteName&YearDOY%in%tsYearDayRange(), .(paths=path[1]),.(conT, Year, DOY)]
  )
  
  ccVals <- eventReactive(input$startExtractCC,{
    if(is.null(curMask())|length(paths()$path)==0) return(data.frame(rcc=NA, gcc=NA, bcc=NA))
    extractCCCTimeSeries(isolate(curMask()), paths()$path)
  })
  
  ccTime <- eventReactive(input$startExtractCC,{
    if(is.null(curMask())) return(NA)
    paths()[,conT]
  })
  
  
  # ----------------------------------------------------------------------
  # Plot timeseries
  # ----------------------------------------------------------------------
  output$timeSeriesPlotly <- 
    renderPlotly({
      fontList <- list(
        family = "Courier New, monospace",
        size = 16,
        color = "#7f7f7f"
      )
      xAxis <- list(
        # title = "Day of year",
        title = "Time (Year)",
        titlefont = fontList
      )
      yAxis <- list(
        title = "CC",
        titlefont = fontList
      )
      
      
      if(input$startExtractCC==0|is.null(isolate(curMask()))){
        tvals <- 1:7
        dummy=0
        
        cvals <- matrix(NA, nrow=length(tvals), ncol = 3)
        colnames(cvals) <- c('rcc','gcc','bcc')
        cvals <- as.data.frame(cvals)
        
        yAxis$range <- c(0,1)
        xAxis$range <- range(tvals)
        dummy=0
        cc <- melt(data.frame(red= cvals$rcc, green = cvals$gcc, blue= cvals$bcc), 
                   variable.name='band', value.name='cc', id.vars=NULL)
        d <- data.table(time=tvals, cc)
        dummy=0
        dummy=0
        ccSel <- as.vector(sapply(input$ccBand, switch, R='red', G='green',  B='blue'))
        d <- d[band%in%ccSel]
        
        p <- plot_ly(data = d, x=~time, y= ~cc,
                     color = ~band, 
                     colors = c('#FF4615','#007D00','#2364B7'),
                     type = 'scatter', mode = 'lines+markers') %>%
          layout(xaxis = xAxis, yaxis = yAxis)
        return(p)
      }
      
      cvals <- ccVals()
      tvals <- ccTime()
      
      shinyjs::enable("downloadTSData")
      dummy=0
      
      cc <- melt(data.frame(red= cvals$rcc, green = cvals$gcc, blue= cvals$bcc), 
                 variable.name='band', value.name='cc', id.vars=NULL)
      d <- data.table(time=tvals, cc)
      ccSel <- as.vector(sapply(input$ccBand, switch, R='red', B='blue', G="green"))
      d <- d[band%in%ccSel]
      
      p <- plot_ly(data = d, x=~time, y= ~cc,
                   color = ~band, 
                   colors = c('#FF4615','#007D00','#2364B7'),
                   type = 'scatter', mode = 'lines+markers') %>%
        layout(xaxis = xAxis, yaxis = yAxis)
      hide_legend(p)
      
    })
  
  
  # ----------------------------------------------------------------------
  # Accept canvas
  # ----------------------------------------------------------------------
  observeEvent(input$acceptCanvas,{
    if(input$maskName=='New mask'){
      values$slideShow <- 0 
      if(is.null(values$centers)) return()
      if (nrow(values$centers)<3) return()
      
      showModal(modalDialog(title = 'Processing',width='300px',
                            "Raster is being produced ...",
                            easyClose = F,
                            size = 's',
                            footer = NULL
      ))
      
      newMask <- list(maskpoints = values$centers, 
                      startdate = input$roiDateRange[1], 
                      enddate = input$roiDateRange[2], 
                      starttime = input$maskStartTime, 
                      endtime = input$maskEndTime, 
                      sampleyear = yearID(), 
                      sampleday = doyID(),
                      sampleImage = sampleImageName(),
                      rasteredMask = createRasteredROI(values$centers, sampleImageSize()))
      
      tmp <- values$MASKs
      tmp[[length(tmp)+1]] <-  newMask
      tmpName <- paste(input$siteName, input$vegType, 
                       sprintf('%04d',roiID()),
                       sprintf('%02d',length(tmp)), sep = '_')
      names(tmp)[length(tmp)] <- tmpName
      values$MASKs <- tmp
      updateSelectInput(session, inputId = 'maskName', choices = c(names(tmp), 'New mask'), selected = tmpName)
      
      removeModal()
    }else{
      values$slideShow <- 0 
      if(is.null(curMask()))return()
      if(is.null(values$centers)) return()
      if (nrow(values$centers)<3) return()
      
      showModal(modalDialog(title = 'Processing',width='300px',
                            "Raster is being updated ...",
                            easyClose = F,
                            size = 's',
                            footer = NULL
      ))
      
      newMASK <- createRasteredROI(values$centers, sampleImageSize())
      tmpMask <- list(maskpoints = values$centers, 
                      startdate = input$roiDateRange[1], 
                      enddate = input$roiDateRange[2], 
                      starttime = input$maskStartTime, 
                      endtime = input$maskEndTime, 
                      sampleyear = yearID(), 
                      sampleday = doyID(),
                      sampleImage = sampleImageName(),
                      rasteredMask = newMASK)
      
      values$MASKs[[input$maskName]] <- tmpMask
      
      removeModal()
      
    }
  })
  
  
  
  # ----------------------------------------------------------------------
  # Plot mask
  # ----------------------------------------------------------------------
  
  output$maskPlot <- 
    renderPlot({
      par(mar=c(0,0,0,0))
      plot(1,
           type='n',
           xaxs='i',yaxs='i',
           xaxt='n',yaxt='n',
           xlab='',ylab='',
           bty='o')
      
      if(is.null(curMask())) {
        text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No mask was generated!', font=2, adj=.5)
        
        return()
      }
      mask <- curMask()
      res <- dim(mask)
      
      wd <- getwd()
      setwd(tmpDir())
      
      plot(NA,xlim=c(1,res[2]),ylim=c(1,res[1]), type='n',
           xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
      dummy=0
      writeTIFF(mask*1, '.tmpraster.tif')
      rmask <- raster('.tmpraster.tif')
      rmask[rmask!=0] <- NA
      
      plot(rmask,legend=F, add=T, col='black')
      file.remove('.tmpraster.tif')
      setwd(wd)
    })
  
  # ----------------------------------------------------------------------
  # Download timeseries
  # ----------------------------------------------------------------------
  
  output$downloadTSData <- downloadHandler(
    filename = function() {
      paste('timeseries-', input$maskName, '-', format(Sys.time(), format = '%Y-%m-%d-%H%M%S'), ".csv", sep="")
    },
    content = function(file) {
      cvals <- ccVals()
      tvals <- paths()[,.(Year, DOY)]
      cc <- data.frame(red= cvals$rcc, green = cvals$gcc, blue= cvals$bcc)
      d <- data.table(tvals, cc)
      write.table(d, file, sep = ',', row.names = F)
    }
  )
  
  observeEvent(input$password,{
    if(input$password==readLines('.key.psw')){
      shinyjs::enable("saveROI")
    }else{
      shinyjs::disable("saveROI")
    }
  })
  
  
  # ----------------------------------------------------------------------
  # Email error
  # ----------------------------------------------------------------------
  
  observeEvent(input$errorSend,{
    msg <- paste0(
      '---------\n',
      'Submit time: \t', as.character(Sys.time()), '\n',
      # 'IP address: \t', as.character(system('ipconfig getifaddr en0', intern=TRUE)),'\n',
      'User: \t', input$errorUser, '\n',
      'Email: \t', input$errorEmail, '\n',
      'OS: \t', input$errorOS, '\n',
      'Browser: \t', input$errorBrowser, '\n',
      'Date: \t', as.character(input$errorDate), '\n',
      'Time: \t', input$errorTime, '\n',
      'Type: \t', input$errorType, '\n',
      '---------\n',
      'Message: \t', input$errorMessage, '\n',
      '---------\n'
    )
    sendmail(from = 'phenocam.network@gmail.com', 
             to = 'phenocam.network@gmail.com', 
             # subject = paste0('drawROI error submitted at ', as.character(Sys.time())), 
             subject = 'a drawROI user just submitted an error report', 
             msg = msg)
    
    showModal(modalDialog(title = 'Message was submitted!',width='250px',
                          "Thank you for helping us to improve the app.",
                          easyClose = T,
                          size = 'm',
                          footer = NULL
    ))
    
    
  })
  
  observeEvent(input$errorTime, 
               {
                 asText <- input$errorTime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'errorTime', value = asTextNew)
               })
  
  shinyjs::disable("downloadTSData")
  shinyjs::disable("saveROI")
  shinyjs::disable("downloadROI")
  shinyjs::disable("emailROI")
  shinyjs::disable("vegType")
  shinyjs::disable("shiftsList")
  shinyjs::disable("gotoShiftFOV")
  
})

