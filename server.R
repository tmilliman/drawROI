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
  
  showModal(strong(
    modalDialog("Please wait for initial configurations ...",
                easyClose = F,
                fade = T,
                size = 's',
                style='background-color:#3b3a35; color:#fce319; ',
                footer = NULL
    )))
  
  observe({
    message(paste(as.character(Sys.time()), 'openEnd observed experssion was called.\t'))
    
    req(input$maskEndDate)
    req(input$maskEndTime)
    if(input$openEnd) {
      shinyjs::disable('maskEndDate')
      shinyjs::disable('maskEndTime')
      updateDateInput(session, 'maskEndDate', value = '9999-12-31')
      updateTextInput(session, 'maskEndTime', value = '23:59:59')
    }else{
      shinyjs::enable('maskEndDate')
      shinyjs::enable('maskEndTime')
    }
  })
  
  options(warn = -1)
  rv <- reactiveValues(centers = matrix(numeric(), 0, 2),
                       MASKs = list(),
                       slideShow = 0,
                       nroi = NULL,
                       ROIs = vector(),
                       sitesList = vector(),
                       parsedROIList = NULL,
                       phenoSites = fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/')
  )
  
  autoInvalidate <- reactiveTimer(1000)
  
  observe({
    message(paste(as.character(Sys.time()), 'vegTypes initial observed experssion was called.\t'))
    
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
    message(paste(as.character(Sys.time()), 'imgDT reactive experssion was called.\t'))
    
    dummy <- 0
    getIMG.DT(input$siteName, midddayListPath)
    
  })
  
  
  
  # ----------------------------------------------------------------------
  # Site
  # ----------------------------------------------------------------------
  observe({
    message(paste(as.character(Sys.time()), 'phenoSitesList observed experssion was called.\t'))
    
    phenoSitesList <- sapply(rv$phenoSites, function(x){x$site})
    names(rv$phenoSites) <- phenoSitesList
    phenoSitesList <- phenoSitesList[-which(phenoSitesList=='HF_Vivotek')]
    if(getwd()==bijanWD) 
      phenoSitesList <- c('dukehw','harvard')
    
    rv$sitesList <- phenoSitesList
    
  })
  
  observe({
    message(paste(as.character(Sys.time()), 'rv$sitesList initial observed experssion was called.\t'))
    
    updateSelectInput(session, inputId = 'siteName', choices = rv$sitesList)
    if(getwd()!=bijanWD) 
      updateSelectInput(session, inputId = 'siteName', selected = 'ahwahnee')
  })
  
  observeEvent(input$siteName, {
    message(paste(as.character(Sys.time()), 'input$siteName was changed to:', '\t',input$siteName, '\t'))
    
    dummy = 0
    updateSelectInput(session, inputId = 'errorSite', selected = input$siteName)
    rv$slideShow <- 0
    
    updateSliderInput(session,
                      inputId = 'contID',
                      value = 1,
                      min= min(dayYearIDTable()$ID),
                      max= max(dayYearIDTable()$ID)  )
    
    tmp <- unlist(strsplit(sampleImageName(), '_'))
    startDate <- as.Date(paste(tmp[2:4], collapse = '-'))
    HHMMSS <- gsub(tmp[5], pattern = '.jpg',replacement = '')
    startTime <- paste(substring(HHMMSS, c(1,3,5), c(2,4,6)), collapse = ':')
    updateDateInput(session, 'maskStartDate', value = startDate)
    updateTextInput(session, inputId = 'maskStartTime', value = startTime)
    
    updateCheckboxInput(session, 'openEnd', value = F)
    
    
    dmin <- imgDT()[Site==input$siteName, min(Date)]
    dmax <- imgDT()[Site==input$siteName, max(Date)]
    updateDateInput(session, 'gotoDate', value = dmin, min = dmin, max = dmax)
    
    # updateDateRangeInput(session ,
    #                      inputId = 'roiDateRange',
    #                      start = dmin,
    #                      end = dmax)  
    # updateDateInput(session, 'maskStartDate', value = dmin)
    # updateDateInput(session, 'maskEndDate', value = dmax)
    
    x <- imgDT()[Site==input$siteName, unique(Year)]
    if (is.null(x)) x <- character(0)
    
    updateSelectInput(session, "year", choices = x)
    updateSelectInput(session, 'roiName', choices = rv$ROIs, selected = 'New ROI')
    updateSelectInput(session, 'roiName', selected = 'New ROI')
    updateSelectInput(session, 'vegType', selected = 'AG')
    updateTextInput(session, 'siteDescription', value = '')
    updateTextInput(session, 'roiOwner', value = '')
    dummy <- 0
    rv$MASKs <- list()
    updateSelectInput(session, inputId = 'maskName', choices = 'New mask')
    rv$centers <- matrix(numeric(), 0, 2)
  })
  
  siteInfo <- reactive({
    message(paste(as.character(Sys.time()), 'siteInfo reactive experssion was called.\t'))
    
    dummy <- 0
    rv$phenoSites[[input$siteName]]
  })
  
  observeEvent(input$nextSite, {
    message(paste(as.character(Sys.time()), 'input$nextSite was changed to:', '\t',input$nextSite, '\t'))
    
    dummy <- 0
    w <- which(rv$sitesList==input$siteName)
    wNext <- w + 1
    if (w==length(rv$sitesList)) wNext= 1
    nextSite <- rv$sitesList[wNext]
    updateSliderInput(session, 'contID', value = 1)
    updateSelectInput(session, 'siteName', selected = nextSite)
    updateSelectInput(session, 'roiName', selected = 'New ROI')
    
  })
  
  observeEvent(input$previousSite, {
    message(paste(as.character(Sys.time()), 'input$previousSite was changed to:', '\t',input$previousSite, '\t'))
    
    dummy <- 0
    w <- which(rv$sitesList==input$siteName)
    wLast <- w - 1
    if (w==1) wLast= length(rv$sitesList)
    previousSite <- rv$sitesList[wLast]
    updateSliderInput(session, 'contID', value = 1)
    updateSelectInput(session, 'siteName', selected = previousSite)
    updateSelectInput(session, 'roiName', selected = 'New ROI')
    
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
                                      wNULL <- which(sapply(inf, is.null))
                                      for(wi in wNULL)inf[wi] <- 'N.A.'
                                      
                                      x <- data.frame(Site = inf$site, 
                                                      Site.Type = inf$site_type,
                                                      MAT = paste0(inf$MAT_worldclim, ' °C'),
                                                      MAP = paste0(inf$MAP_worldclim, ' mm/year'),
                                                      Koeppen.Class = inf$koeppen_geiger,
                                                      Latitude = paste0(inf$lat, ' °'),
                                                      Longitude = paste0(inf$lon, ' °'),
                                                      Elevation = paste0(inf$elev, ' m'),
                                                      Descriotion = inf$site_description,
                                                      Primary.Vegetation = inf$primary_veg_type
                                      )
                                      
                                      tx <- t(x)
                                      
                                      tx
                                    })
  
  # ----------------------------------------------------------------------
  # ROIs
  # ----------------------------------------------------------------------
  roipath <- reactive({
    message(paste(as.character(Sys.time()), 'roipath reactive experssion was called.\t'))
    
    tmp <- (paste0('/data/archive/', input$siteName,'/ROI/'))
    # tmp <- ('/home/shiny/drawROI/ROI/')
    # tmp <- tempdir()
    
    if(getwd()==bijanWD) 
      tmp <- (paste0('phenocamdata/data/archive/', input$siteName,'/ROI/'))
    return(tmp)
  }  )
  
  observe({
    
    autoInvalidate()
    tmp.rv.ROIs <- c(dir(roipath(), pattern = 'roi.csv$'), "New ROI")
    if(!identical(rv$ROIs, tmp.rv.ROIs))    {
      message(paste(as.character(Sys.time()), 'dir roi.csv observed experssion was called.\t'))
      
      rv$ROIs <- tmp.rv.ROIs
    }
  }
  )
  
  observe({
    message(paste(as.character(Sys.time()), 'rv$ROIs observed experssion was called.\t'))
    updateSelectInput(session, 'roiName', choices = rv$ROIs, selected = 'New ROI')
  })
  
  
  # ----------------------------------------------------------------------
  # ROI label
  # ----------------------------------------------------------------------
  roiLabel <- reactive({
    message(paste(as.character(Sys.time()), 'roiLabel reactive experssion was called.\t'))
    
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
    message(paste(as.character(Sys.time()), 'input$roiName was changed to:', '\t',input$roiName, '\t'))
    dummy = 0
    rv$slideShow <- 0 
    
    if(input$roiName=='New ROI') {
      shinyjs::enable('vegType')
      dummy =0 
      rv$MASKs <- list()
      rv$centers <- matrix(numeric(), 0, 2)
      updateSelectInput(session, inputId = 'maskName', choices = 'New mask')
      updateSelectInput(session, inputId = 'vegType', selected = list('Agriculture (AG)'='AG'))
      updateSelectInput(session, inputId = 'siteDescription', selected = '')
      updateTextInput(session, inputId = 'roiOwner', value = '')
      
      return()
    }
    shinyjs::disable('vegType')
    dummy=0
    dummy=0
    rv$parsedROIList <- parseROI(roifilename=input$roiName,
                                 roipath = roipath())
    
    updateSelectInput(session, inputId = 'vegType', selected =  rv$parsedROIList$vegType)
    updateTextInput(session, inputId = 'siteDescription', value = rv$parsedROIList$Description)
    updateTextInput(session, inputId = 'roiOwner', value = rv$parsedROIList$Owner)
    dummy=0
    
    rv$MASKs <- rv$parsedROIList$masks
    
    updateSelectInput(session, inputId = 'maskName', choices = c(names(rv$MASKs), 'New mask'))
    
    updateSelectInput(session, inputId = 'year', selected = rv$parsedROIList$masks[[1]]$sampleyear)
    updateSelectInput(session, inputId = 'viewDay', selected = rv$parsedROIList$masks[[1]]$sampleday)
    dummy =0
  })
  
  observe({
    autoInvalidate()
    dummy <- 0
    template <- paste0(input$siteName, '_', input$vegType)
    sameTemplate <- grepl(template, rv$ROIs)
    if(sum(sameTemplate)==0) 
      n <- 1
    else
      n <- max(
        as.numeric(
          sapply(
            strsplit(
              rv$ROIs[sameTemplate], '_'), 
            function(x)(x[3])
          )
        )
      ) + 1
    n
    
    if(!identical(n , rv$nroi)) {
      message(paste(as.character(Sys.time()), 'rv$nroi observed experssion was called.\t'))
      rv$nroi <- n
    }
  })
  
  
  roiID <- reactive({
    message(paste(as.character(Sys.time()), 'roiID reactive experssion was called.\t'))
    
    if(input$roiName=='New ROI') {
      return(rv$nroi)
    }
    else {
      dummy = 0 
      rv$parsedROIList$ID
    }
  })
  curMask <- reactive({
    message(paste(as.character(Sys.time()), 'curMask reactive experssion was called.\t'))
    
    if(input$maskName=='New mask') {
      return(NULL)
    }
    rv$MASKs[[input$maskName]]$rasteredMask
  })
  
  
  # ----------------------------------------------------------------------
  # MASKs
  # ----------------------------------------------------------------------
  observeEvent(rv$MASKs,{
    message(paste(as.character(Sys.time()), 'input$MASKs was changed to:', '\t','...', '\t'))
    if(length(rv$MASKs)==0) {
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
    message(paste(as.character(Sys.time()), 'input$vegType was changed to:', '\t',input$vegType, '\t'))
    rv$slideShow <- 0 
    if(length(rv$MASKs)==0) return()
    
    maskNames <- names(rv$MASKs)
    f <- function(x, y){
      z <- unlist(strsplit(x, '_'))
      paste(c(z[1], y, z[3:4]), collapse = '_')
    }
    
    newmaskNames <- as.vector(sapply(maskNames, f, y = input$vegType))
    if(!is.null(rv$MASKs))names(rv$MASKs) <- newmaskNames
    updateSelectInput(session, inputId = 'maskName', choices = c(names(rv$MASKs), 'New mask'))
  })
  
  
  
  # ----------------------------------------------------------------------
  # Mask start and end time
  # ----------------------------------------------------------------------
  observeEvent(input$maskStartTime, 
               {
                 message(paste(as.character(Sys.time()), 'input$maskStartTime was changed to:', '\t',input$maskStartTime, '\t'))
                 asText <- input$maskStartTime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'maskStartTime', value = asTextNew)
                 if(input$maskName!='New mask') rv$MASKs[[input$maskName]]$starttime <- asTextNew
               })
  
  observeEvent(input$maskEndTime, 
               {
                 message(paste(as.character(Sys.time()), 'input$maskEndTime was changed to:', '\t',input$maskEndTime, '\t'))
                 asText <- input$maskEndTime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'maskEndTime', value = asTextNew)
                 if(input$maskName!='New mask') rv$MASKs[[input$maskName]]$endtime <- asTextNew
               })
  
  observeEvent(input$maskStartDate,{
    message(paste(as.character(Sys.time()), 'input$maskStartDate was changed to:', '\t',input$maskStartDate, '\t'))
    if(input$maskName!='New mask') rv$MASKs[[input$maskName]]$startdate <- input$maskStartDate
  })
  
  observeEvent(input$maskEndDate,{
    message(paste(as.character(Sys.time()), 'input$maskEndDate was changed to:', '\t',input$maskEndDate, '\t'))
    if(input$maskName!='New mask') rv$MASKs[[input$maskName]]$enddate <- input$maskEndDate
  })
  
  dayYearIDTable <- reactive({
    message(paste(as.character(Sys.time()), 'dayYearIDTable reactive experssion was called.\t'))
    
    dummy <- 0
    imgDT()[Site==input$siteName,.(ID=1:.N,Year, DOY, Date)]
  }    )
  
  
  # ----------------------------------------------------------------------
  # Slideshow
  # ----------------------------------------------------------------------
  observe({
    message(paste(as.character(Sys.time()), 'slideShow observed experssion was called.\t'))
    
    if(rv$slideShow==0) return()
    nextID <- as.numeric(input$contID) + rv$slideShow
    if(nextID > max(dayYearIDTable()$ID)) nextID <- 1
    if(nextID < 1) nextID <- max(dayYearIDTable()$ID)
    
    # print(paste(input$contID, '+', rv$slideShow, 'changed to', nextID))
    dummy <- 0
    updateSliderInput(session,
                      inputId = 'contID',
                      value = nextID)
  })
  
  observeEvent(input$pause, {
    message(paste(as.character(Sys.time()), 'input$pause was changed to:', '\t',input$pause, '\t'))
    rv$slideShow <- 0
  })
  
  observeEvent(input$play, {
    message(paste(as.character(Sys.time()), 'input$play was changed to:', '\t',input$play, '\t'))
    if(rv$slideShow==0) rv$slideShow <- 1
    if(rv$slideShow==-1) rv$slideShow <- 0
  })
  
  observeEvent(input$backplay, {
    message(paste(as.character(Sys.time()), 'input$backplay was changed to:', '\t',input$backplay, '\t'))
    if(rv$slideShow==0) rv$slideShow <- -1
    if(rv$slideShow==1) rv$slideShow <- 0
  })
  
  observeEvent(input$back, {
    message(paste(as.character(Sys.time()), 'input$back was changed to:', '\t',input$back, '\t'))
    rv$slideShow <- 0 
    nextID <- as.numeric(input$contID) - 1
    if(nextID > max(dayYearIDTable()$ID)) nextID <- 1
    if(nextID == 0) nextID <- max(dayYearIDTable()$ID)
    
    updateSliderInput(session, "contID", value = nextID)
  })
  
  observeEvent(input$forw, {
    message(paste(as.character(Sys.time()), 'input$forw was changed to:', '\t',input$forw, '\t'))
    rv$slideShow <- 0 
    nextID <- as.numeric(input$contID) + 1
    if(nextID > max(dayYearIDTable()$ID)) nextID <- 1
    if(nextID == 0) nextID <- max(dayYearIDTable()$ID)
    
    updateSliderInput(session, "contID", value = nextID)
  })
  
  
  # ----------------------------------------------------------------------
  # Sample Image
  # ----------------------------------------------------------------------
  sampleImage <- reactive({
    message(paste(as.character(Sys.time()), 'sampleImage reactive experssion was called.\t'))
    
    imgDT()[Site==input$siteName&Year==yearID()&DOY==doyID(), path][1]
  }  )
  
  sampleImageSize <- reactive({
    message(paste(as.character(Sys.time()), 'sampleImageSize reactive experssion was called.\t'))
    
    dim(readJPEG(sampleImage()))[1:2]
  })
  
  sampleImageName <- reactive({
    message(paste(as.character(Sys.time()), 'sampleImageName reactive experssion was called.\t'))
    
    tmp <- unlist(strsplit(sampleImage(), split = '/'))
    tmp[length(tmp)]
  })
  
  output$sampleImagePath <- renderText(
    sampleImageName()
  )
  
  observeEvent(input$matchStart, {
    message(paste(as.character(Sys.time()), 'input$matchStart was changed to:', '\t',input$matchStart, '\t'))
    tmp <- unlist(strsplit(sampleImageName(), '_'))
    startDate <- as.Date(paste(tmp[2:4], collapse = '-'))
    HHMMSS <- gsub(tmp[5], pattern = '.jpg',replacement = '')
    startTime <- paste(substring(HHMMSS, c(1,3,5), c(2,4,6)), collapse = ':')
    
    # updateDateRangeInput(session, inputId = 'roiDateRange', start = startDate)
    updateDateInput(session, 'maskStartDate', value = startDate)
    
    updateTextInput(session, inputId = 'maskStartTime', value = startTime)
  })
  
  observeEvent(input$matchEnd, {
    message(paste(as.character(Sys.time()), 'input$matchEnd was changed to:', '\t',input$matchEnd, '\t'))
    tmp <- unlist(strsplit(sampleImageName(), '_'))
    endDate <- as.Date(paste(tmp[2:4], collapse = '-'))
    HHMMSS <- gsub(tmp[5], pattern = '.jpg',replacement = '')
    endTime <- paste(substring(HHMMSS, c(1,3,5), c(2,4,6)), collapse = ':')
    
    # updateDateRangeInput(session, inputId = 'roiDateRange', end = endDate)
    updateDateInput(session, 'maskEndDate', value = endDate)
    
    updateTextInput(session, inputId = 'maskEndTime', value = endTime)
  })
  
  
  
  doyID <- reactive({
    message(paste(as.character(Sys.time()), 'doyID reactive experssion was called.\t'))
    
    dummy <- 1
    tmp <- dayYearIDTable()[ID==as.numeric(input$contID),DOY]
    if(length(tmp)== 0) tmp <- dayYearIDTable()[ID==1,DOY]
    tmp
  }
  )
  
  yearID <- reactive({
    message(paste(as.character(Sys.time()), 'yearID reactive experssion was called.\t'))
    
    tmp <- dayYearIDTable()[ID==as.numeric(input$contID),Year]
    if(length(tmp)== 0) tmp <- dayYearIDTable()[ID==1,Year]
    tmp
  }  )
  
  output$yearOut <- renderText({
    paste0('    Year:  ', yearID())
  })
  
  output$doyOut <- renderText({
    paste0('    DOY:  ', doyID())
  })
  
  observeEvent(input$gotoDateButton,{
    message(paste(as.character(Sys.time()), 'input$gotoDateButton was changed to:', '\t',input$gotoDateButton, '\t'))
    dummy <- 1
    tmpDT <- dayYearIDTable()
    tmpDT[, dif:=abs(Date-input$gotoDate)]
    id <- tmpDT[dif==min(dif), ID]
    updateSliderInput(session, inputId = 'contID', value = id)
  })
  
  observeEvent(input$contID,{
    message(paste(as.character(Sys.time()), 'input$contID was changed to:', '\t',input$contID, '\t'))
    tmpDate <- dayYearIDTable()[ID==input$contID, Date]
    updateDateInput(session, 'gotoDate', value = tmpDate)
  })
  # ----------------------------------------------------------------------
  # Plot image
  # ----------------------------------------------------------------------
  output$imagePlot <- renderPlot(
    res=36,
    height = function(){floor(session$clientData$output_imagePlot_width/1.35)},
    {
      if(is.na(sampleImage())){
        par(mar=c(0,0,0,0))
        plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
        text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No image for this date was found!', font=2, adj=.5)
      }else{
        par(mar=c(0,0,0,0))
        plotJPEG(sampleImage())
        roiColors <- if (input$roiColors=='transparent') '#ffffff00' else paste0(input$roiColors, '60')
        dummy <- 0
        if(is.null(rv$centers)) 
          absPoints <- matrix(numeric(), 0, 2)
        else if(nrow(rv$centers)==0) 
          absPoints <- matrix(numeric(), 0, 2)
        else if(nrow(rv$centers)==1) 
          absPoints <- rv$centers*sampleImageSize()
        else 
          absPoints <- t(apply(rv$centers, 1, '*', sampleImageSize()))
        dummy <- 0
        polygon(absPoints, col = roiColors, pch = 9, lwd=2)
      }
    })
  
  observeEvent(input$maskName, {
    message(paste(as.character(Sys.time()), 'input$maskName was changed to:', '\t',input$maskName, '\t'))
    rv$slideShow <- 0 
    if(input$maskName=='New mask') {
      # rv$MASKs <- list()
      # rv$centers <- matrix(numeric(), 0, 2)
      
      updateCheckboxInput(session, 'openEnd', value = T)
      return()
    }
    updateCheckboxInput(session, 'openEnd', value = F)
    tmpmask <- rv$MASKs[[input$maskName]]
    
    rv$centers <- tmpmask$maskpoints
    # updateDateRangeInput(session, inputId = 'roiDateRange', start=tmpmask$startdate)
    # updateDateRangeInput(session, inputId = 'roiDateRange', end=tmpmask$enddate)
    updateDateInput(session, 'maskStartDate', value = tmpmask$startdate)
    updateDateInput(session, 'maskEndDate', value = tmpmask$enddate)
    
    updateTextInput(session, inputId = 'maskStartTime', value = tmpmask$starttime)
    updateTextInput(session, inputId = 'maskEndTime', value = tmpmask$endtime)
    
    tmpID <- dayYearIDTable()[Year==as.numeric(yearID())&DOY==as.numeric(doyID()), ID]
    if(length(tmpID)==0) tmpID <- 1
    rv$ID <- tmpID[1]
    # updateSelectInput(session, inputId = 'year', selected = tmpmask$sampleyear)
    # updateSelectInput(session, inputId = 'viewDay', selected = tmpmask$sampleday)
  })
  
  observeEvent(input$newPoint, {
    if(nrow(input$newPoint)>0)
      message(paste(as.character(Sys.time()), 'input$newPoint was updated with:', '\t',input$newPoint[nrow(input$newPoint), ], '\t'))
    else
      message(paste(as.character(Sys.time()), 'input$newPoint was reset.\t'))
    rv$slideShow <- 0 
    newPoint <- matrix(c(input$newPoint[['x']], input$newPoint[['y']]),1, 2)
    rv$centers <- rbind(rv$centers, newPoint/sampleImageSize())
  })
  
  observeEvent(input$endPoint, {
    message(paste(as.character(Sys.time()), 'input$endPoint was changed to:', '\t',input$endPoint, '\t'))
    rv$slideShow <- 0
    pnts <- rv$centers
    tbl <- as.data.table(na.omit(cbind(pnts,cumsum(is.na(pnts[,1]))+1 )))
    colnames(tbl) <- c('x', 'y', 'g')
    tbln <- table(tbl$g)
    if(tbln[length(tbln)]<3) return()
    newPoint <- matrix(c(NA, NA),1, 2)
    rv$centers <- rbind(rv$centers, newPoint)
  })
  
  
  observeEvent(input$clearCanvas, {
    message(paste(as.character(Sys.time()), 'input$clearCanvas was changed to:', '\t',input$clearCanvas, '\t'))
    rv$slideShow <- 0 
    rv$centers <- matrix(numeric(), 0, 2)
  })
  
  observeEvent(input$undoCanvas, {
    message(paste(as.character(Sys.time()), 'input$undoCanvas was changed to:', '\t',input$undoCanvas, '\t'))
    rv$slideShow <- 0 
    if (nrow(rv$centers) > 2)
      rv$centers <- rv$centers[-nrow(rv$centers),]
    else if (nrow(rv$centers) == 2)
      rv$centers <- matrix(rv$centers[1,], 1, 2)
    else if (nrow(rv$centers) == 1)
      rv$centers <- matrix(numeric(), 0, 2)
  })
  
  # ----------------------------------------------------------------------
  # Save ROI List
  # ----------------------------------------------------------------------
  observeEvent(input$saveROI,{
    message(paste(as.character(Sys.time()), 'input$saveROI was changed to:', '\t',input$saveROI, '\t'))
    rv$slideShow <- 0 
    if(length(rv$MASKs)==0) return()
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
                    masks = rv$MASKs)
    
    if(input$roiName!='New ROI'){
      ROIList$createDate <- rv$parsedROIList$createDate
      ROIList$createTime <- rv$parsedROIList$createTime
    }
    dummy <- 0
    
    roifilename <- paste0(roiLabel(),'_roi.csv')
    
    # writeROIListFile(ROIList, path = roipath(),  roifilename)
    writeROIListFile(ROIList, path = '/tmp/ROI/',  roifilename)
    
    showModal(strong(modalDialog("ROI was saved in the database!",
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 easyClose = T,
                                 size = 's',
                                 footer = NULL
    )))
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
                      masks = rv$MASKs)
      
      if(input$roiName!='New ROI'){
        ROIList$createDate <- rv$parsedROIList$createDate
        ROIList$createTime <- rv$parsedROIList$createTime
      }
      
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
    message(paste(as.character(Sys.time()), 'input$emailROI was changed to:', '\t',input$emailROI, '\t'))
    rv$slideShow <- 0 
    if(length(rv$MASKs)==0) return()
    
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
                    masks = rv$MASKs)
    
    if(input$roiName!='New ROI'){
      ROIList$createDate <- rv$parsedROIList$createDate
      ROIList$createTime <- rv$parsedROIList$createTime
    }
    
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
    
    showModal(strong(modalDialog("The new ROI will be reviewed shortly.",
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 easyClose = T,
                                 size = 's',
                                 footer = NULL
    )))
    
    
  })
  
  
  # ----------------------------------------------------------------------
  # tsYearDayRange
  # ----------------------------------------------------------------------
  
  
  tsYearDayRange <- reactive({
    message(paste(as.character(Sys.time()), 'tsYearDayRange reactive experssion was called.\t'))
    
    if(input$ccRange=="week")
      return(imgDT()[Site==input$siteName&Year==yearID()&DOY%in%(doyID()[1]:(doyID()[1]+7)),YearDOY])
    else if(input$ccRange=="year")
      # return(1:365)
      return(imgDT()[Site==input$siteName&Year==yearID(),YearDOY])
    else if(input$ccRange=="all")
      return(imgDT()[Site==input$siteName,YearDOY])
  })
  
  paths <- reactive({
    message(paste(as.character(Sys.time()), 'paths reactive experssion was called.\t'))
    
    imgDT()[Site==input$siteName&YearDOY%in%tsYearDayRange(), .(paths=path[1]),.(conT, Year, DOY, Date)]
  })
  
  ccVals <- eventReactive(input$startExtractCC,{
    message(paste(as.character(Sys.time()), 'ccVals-startExtractCC eventReactive experssion was called.\t'))
    
    if(is.null(curMask())|length(paths()$path)==0) {
      return(data.frame(rcc=NA, gcc=NA, bcc=NA))
    }
    dummy <- 0
    showModal(strong(modalDialog("Time series data are being extracted ...",
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 easyClose = F,
                                 size = 's',
                                 footer = actionButton(inputId = "stopExtractCC2",
                                                       label =  "Stop", 
                                                       width = '100%',
                                                       # class="btn-danger", 
                                                       icon = icon('stop'),
                                                       style='background-color:#3b3a35; color:#fce319; ',
                                                       onclick="Shiny.onInputChange('stopThis',true)")
    )))
    
    cc <- extractCCCTimeSeries(isolate(curMask()), paths()$path)
    removeModal()
    cc
  })
  
  ccTime <- eventReactive(input$startExtractCC,{
    message(paste(as.character(Sys.time()), 'ccTime-startExtractCC eventReactive experssion was called.\t'))
    
    if(is.null(curMask())) {
      return(NA)
    }
    # paths()[, conT]
    paths()[, Date]
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
        title = "Time",
        titlefont = fontList
      )
      yAxis <- list(
        title = "CC",
        titlefont = fontList
      )
      
      
      if(input$startExtractCC==0|is.null(isolate(curMask()))){
        
        
        if(input$startExtractCC>0)showModal(strong(modalDialog('You first have to create a mask!',
                                                               style='background-color:#3b3a35; color:#fce319; ',
                                                               footer = NULL, easyClose = T, size = 's')))
        
        tvals <- 0:1
        dummy=0
        
        cvals <- matrix(NA, nrow=length(tvals), ncol = 3)
        colnames(cvals) <- c('rcc','gcc','bcc')
        cvals <- as.data.frame(cvals)
        
        yAxis$range <- c(0,1)
        xAxis$range <- c(0,1)
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
      
      wZeros <- (rowSums(cvals)==0)
      cvals[wZeros,] <- c(NA, NA, NA)
      # cvals <- cvals[!wZeros,]
      # tvals <- tvals[!wZeros]
      
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
    message(paste(as.character(Sys.time()), 'input$acceptCanvas was changed to:', '\t',input$acceptCanvas, '\t'))
    rv$slideShow <- 0 
    if(is.null(rv$centers)) {
      showModal(strong(modalDialog('First draw a polgon by clicking on the image!', 
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   footer = NULL, easyClose = T, size = 'm')))
      return()
    }
    if (nrow(rv$centers)<3) {
      showModal(strong(modalDialog('At least 3 points are required to create a polygon!',
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   footer = NULL, easyClose = T, size = 'm')))
      return()
    }
    
    if(input$maskName=='New mask'){
      
      showModal(strong(modalDialog("Raster is being produced ...",
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   easyClose = F,
                                   size = 's',
                                   footer = NULL
      )))
      dummy <- 0
      dummy <- 0
      
      newMask <- list(maskpoints = rv$centers, 
                      # startdate = input$roiDateRange[1], 
                      # enddate = input$roiDateRange[2], 
                      startdate = input$maskStartDate, 
                      enddate = input$maskEndDate, 
                      starttime = input$maskStartTime, 
                      endtime = input$maskEndTime, 
                      sampleyear = yearID(), 
                      sampleday = doyID(),
                      sampleImage = sampleImageName(),
                      rasteredMask = createRasteredROI(rv$centers, sampleImageSize()))
      
      tmp <- rv$MASKs
      tmp[[length(tmp)+1]] <-  newMask
      
      if(length(rv$MASKs)!=0)
        maskID <- max(
          as.numeric(
            sapply(
              strsplit(
                names(rv$MASKs), split = '_'), function(x)(x[length(x)]))
          )
        ) + 1
      else maskID <- 1
      
      tmpName <- paste(input$siteName, input$vegType, 
                       sprintf('%04d',roiID()),
                       sprintf('%02d',maskID), sep = '_')
      names(tmp)[length(tmp)] <- tmpName
      rv$MASKs <- tmp
      updateSelectInput(session, inputId = 'maskName', choices = c(names(tmp), 'New mask'), selected = tmpName)
      
      removeModal()
    }else{
      if(is.null(curMask()))return()
      
      showModal(strong(modalDialog("Raster is being updated ...",
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   easyClose = F,
                                   size = 's',
                                   footer = NULL
      )))
      
      newMASK <- createRasteredROI(rv$centers, sampleImageSize())
      tmpMask <- list(maskpoints = rv$centers, 
                      # startdate = input$roiDateRange[1], 
                      # enddate = input$roiDateRange[2], 
                      startdate = input$maskStartDate, 
                      enddate = input$maskEndDate, 
                      starttime = input$maskStartTime, 
                      endtime = input$maskEndTime, 
                      sampleyear = yearID(), 
                      sampleday = doyID(),
                      sampleImage = sampleImageName(),
                      rasteredMask = newMASK)
      
      rv$MASKs[[input$maskName]] <- tmpMask
      
      removeModal()
      
    }
  })
  
  
  
  # ----------------------------------------------------------------------
  # Plot mask
  # ----------------------------------------------------------------------
  
  output$maskPlot <- 
    renderPlot(
      res=96,
      height = function(){floor(session$clientData$output_maskPlot_width/1.35)},
      {
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
    message(paste(as.character(Sys.time()), 'input$password was changed to:', '\t', rep('*', length(input$password)), '\t'))
    filepass <- '.key.psw2'
    
    if(file.exists(filepass)){
      fcon <- try(file(filepass, 'r'), silent = F)
      tmppass <- readLines(fcon)
      close(fcon)
    }else{
      showModal(strong(modalDialog("Connection to the passfile was failed!",
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   easyClose = T,
                                   size = 's',
                                   footer = NULL
      )))
      return()
    }
    
    if(input$password==tmppass){
      shinyjs::enable("saveROI")
    }else{
      shinyjs::disable("saveROI")
    }
  })
  
  
  # ----------------------------------------------------------------------
  # Email error
  # ----------------------------------------------------------------------
  
  observeEvent(input$errorSend,{
    message(paste(as.character(Sys.time()), 'input$errorSend was changed to:', '\t',input$errorSend, '\t'))
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
    
    showModal(strong(modalDialog("Message was submitted. Thank you for helping us to improve the app.",
                                 style='background-color:#3b3a35; color:#fce319; ',
                                 easyClose = T,
                                 size = 'm',
                                 footer = NULL
    )))
    
    
  })
  
  observeEvent(input$errorTime, 
               {
                 message(paste(as.character(Sys.time()), 'input$errorTime was changed to:', '\t',input$errorTime, '\t'))
                 asText <- input$errorTime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'errorTime', value = asTextNew)
               })
  
  
  ##LOG
  
  message(paste('\n--------------------------------------------------------------------\n', 
                as.character(Sys.time()),'New session just started!',
                '\n--------------------------------------------------------------------\n'))
  observeEvent(input$siteDescription, message(paste(as.character(Sys.time()), 'input$siteDescription was changed to:', '\t',input$siteDescription, '\t')))
  observeEvent(input$roiOwner, message(paste(as.character(Sys.time()), 'input$roiOwner was changed to:', '\t',input$roiOwner, '\t')))
  observeEvent(input$roiColors, message(paste(as.character(Sys.time()), 'input$roiColors was changed to:', '\t',input$roiColors, '\t')))
  observeEvent(input$ccRange, message(paste(as.character(Sys.time()), 'input$ccRange was changed to:', '\t',input$ccRange, '\t')))
  observeEvent(input$ccBand, message(paste(as.character(Sys.time()), 'input$ccBand was changed to:', '\t',input$ccBand, '\t')))
  observeEvent(input$openEnd, message(paste(as.character(Sys.time()), 'input$openEnd was changed to:', '\t',input$openEnd, '\t')))
  observeEvent(input$errorBrowser, message(paste(as.character(Sys.time()), 'input$errorBrowser was changed to:', '\t',input$errorBrowser, '\t')))
  observeEvent(input$errorDate, message(paste(as.character(Sys.time()), 'input$errorDate was changed to:', '\t',as.character(input$errorDate), '\t')))
  observeEvent(input$errorEmail, message(paste(as.character(Sys.time()), 'input$errorEmail was changed to:', '\t',input$errorEmail, '\t')))
  observeEvent(input$errorMessage, message(paste(as.character(Sys.time()), 'input$errorMessage was changed to:', '\t',input$errorMessage, '\t')))
  observeEvent(input$errorOS, message(paste(as.character(Sys.time()), 'input$errorOS was changed to:', '\t',input$errorOS, '\t')))
  observeEvent(input$errorSend, message(paste(as.character(Sys.time()), 'input$errorSend was changed to:', '\t',input$errorSend, '\t')))
  observeEvent(input$errorSite, message(paste(as.character(Sys.time()), 'input$errorSite was changed to:', '\t',input$errorSite, '\t')))
  observeEvent(input$errorTime, message(paste(as.character(Sys.time()), 'input$errorTime was changed to:', '\t',input$errorTime, '\t')))
  observeEvent(input$errorType, message(paste(as.character(Sys.time()), 'input$errorType was changed to:', '\t',input$errorType, '\t')))
  observeEvent(input$errorUser, message(paste(as.character(Sys.time()), 'input$errorUser was changed to:', '\t',input$errorUser, '\t')))
  observeEvent(input$siteInfo, message(paste(as.character(Sys.time()), 'input$siteInfo was changed to:', '\t',input$siteInfo, '\t')))
  observeEvent(input$modalSiteInfo, message(paste(as.character(Sys.time()), 'input$modalSiteInfo was changed to:', '\t',input$modalSiteInfo, '\t')))
  
  
  shinyjs::disable("downloadTSData")
  shinyjs::disable("saveROI")
  shinyjs::disable("downloadROI")
  shinyjs::disable("emailROI")
  shinyjs::disable("vegType")
  shinyjs::disable("shiftsList")
  shinyjs::disable("gotoShiftFOV")
  
  removeModal()
  
  # showModal(strong(
  #   modalDialog(HTML('This is the beta version of PhenoCam ROI app. Thanks for helping us to improve it. <br> 
  #               Please do not share with others.'),
  #               easyClose = T, 
  #               fade = T,
  #               size = 'm',
  #               style='background-color:#3b3a35; color:#fce319; ',
  #               footer = NULL
  #   )))
  
})

