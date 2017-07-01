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

# library(rjson)
# phenoSites <- fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/')
# # phenoSites
# phenoROIs <- fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/roi/roilistinfo/')


midddayListPath <- '/home/bijan/middayList/'
### XXX
if(getwd()=="/Users/bijan/Projects/drawROI") midddayListPath <- 'midddayListPath/'


shinyServer(function(input, output, session) {
  options(warn = -1)
  values <- reactiveValues(centers = matrix(numeric(), 0, 2),
                           MASKs = list(),
                           slideShow = 0,
                           contID= 1, 
                           ROIs = vector(),
                           sitesList = vector(),
                           parsedROIList = NULL,
                           phenoSites = fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/')
  )
  
  autoInvalidate1 <- reactiveTimer(1000)
  autoInvalidate2 <- reactiveTimer(1000)
  
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
    updateSelectInput(session, 'vegtype', choices = vegTypes)    
  })
  
  
  # ----------------------------------------------------------------------
  # imgDT
  # ----------------------------------------------------------------------
  imgDT <- reactive({
    dummy <- 0
    getIMG.DT(input$site, midddayListPath)
  })
  
  # ----------------------------------------------------------------------
  # Site
  # ----------------------------------------------------------------------
  observe({
    # values$sitesList <- imgDT[,unique(Site)]
    
    phenoSitesList <- sapply(values$phenoSites, function(x){x$site})
    names(values$phenoSites) <- phenoSitesList
    ### XXX
    if(getwd()=="/Users/bijan/Projects/drawROI") phenoSitesList <- c('acadia','dukehw','harvard')
    values$sitesList <- phenoSitesList
    
  })
  
  observe({
    updateSelectInput(session, inputId = 'site', choices = values$sitesList)
    updateSelectInput(session, inputId = 'errorSite', choices = values$sitesList)
  })
  
  observeEvent(input$site, {
    dummy = 0
    updateSelectInput(session, inputId = 'errorSite', selected = input$site)
    # values$ROIs <- c(dir(roipath(), pattern = 'roi.csv'), "New ROI")
    # values$MASKs <- NULL
    values$contID <- 1
    values$slideShow <- 0
    
    updateSliderInput(session,
                      inputId = 'contID',
                      value = 1,
                      min= min(dayYearIDTable()$ID),
                      max= max(dayYearIDTable()$ID)  )
    
    dmin <- imgDT()[Site==input$site, min(Date)]
    dmax <- imgDT()[Site==input$site, max(Date)]
    updateDateRangeInput(session ,
                         inputId = 'roiDateRange',
                         start = dmin,
                         end = dmax)  
    
    x <- imgDT()[Site==input$site, unique(Year)]
    if (is.null(x)) x <- character(0)
    
    updateSelectInput(session, "year", choices = x)
    updateSelectInput(session, 'rois', choices = values$ROIs, selected = 'New ROI')
    dummy <- 0
    
  })
  
  siteInfo <- reactive({
    values$phenoSites[[input$site]]
  })
  
  observeEvent(input$nextsite, {
    dummy <- 0
    w <- which(values$sitesList==input$site)
    wNext <- w + 1
    if (w==length(values$sitesList)) wNext= 1
    nextSite <- values$sitesList[wNext]
    updateSelectInput(session, 'site', selected = nextSite)
  })
  # ----------------------------------------------------------------------
  # Site info
  # ----------------------------------------------------------------------
  # observeEvent(input$siteInfo,{
  #   
  # })
  # 
  output$tblSiteInfo = renderTable( rownames = T, 
                                    colnames = F, 
                                    striped = T, 
                                    hover = T, 
                                    bordered = F,
                                    spacing = 's',
                                    options = list( lengthChange = FALSE),{
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
    tmp <- (paste0('/data/archive/', input$site,'/ROI/'))
    ### XXX
    if(getwd()=="/Users/bijan/Projects/drawROI") tmp <- (paste0('phenocamdata/data/archive/', input$site,'/ROI/'))
    return(tmp)
  }  )
  
  observe(
    values$ROIs <- c(dir(roipath(), pattern = 'roi.csv$'), "New ROI")
  )
  
  observe(
    updateSelectInput(session, 'rois', choices = values$ROIs, selected = 'New ROI')
  )
  
  
  # ----------------------------------------------------------------------
  # ROI label
  # ----------------------------------------------------------------------
  roilabel <- reactive({
    dummy = 0 
    # if(input$rois=='New ROI')
      label <- paste(input$site, 
                     input$vegtype, 
                     sprintf('%04d',roiID()), sep = '_')
    # else
    #   label <- input$rois
    label
  }
  )
  
  output$roilabel <- renderText({
    if(input$rois=='New ROI')
      paste0(roilabel(),'_roi.csv')
    else
      input$rois
  })
  
  
  
  # ----------------------------------------------------------------------
  # Parsed ROI List
  # ----------------------------------------------------------------------
  
  
  observeEvent(input$rois,{
    dummy = 0
    values$slideShow <- 0 
    
    if(input$rois=='New ROI') {
      shinyjs::enable('vegtype')
      dummy =0 
      values$MASKs <- NULL
      updateSelectInput(session, inputId = 'masks', choices = '')
      
      return()
    }
    shinyjs::disable('vegtype')
    dummy=0
    dummy=0
    values$parsedROIList <- parseROI(roifilename=input$rois,
                                     roipath = roipath())
    
    updateSelectInput(session, inputId = 'vegtype', selected =  values$parsedROIList$vegType)
    updateTextInput(session, inputId = 'descr', value = values$parsedROIList$Description)
    updateTextInput(session, inputId = 'owner', value = values$parsedROIList$Owner)
    dummy=0
    # updateSelectInput(session, inputId = 'masks', choices = names(values$parsedROIList$masks))
    values$MASKs <- values$parsedROIList$masks
    
    updateSelectInput(session, inputId = 'masks', choices = names(values$MASKs))
    
    updateSelectInput(session, inputId = 'year', selected = values$parsedROIList$masks[[1]]$sampleyear)
    updateSelectInput(session, inputId = 'viewDay', selected = values$parsedROIList$masks[[1]]$sampleday)
    dummy =0
  })
  
  nroi <- reactive({
    autoInvalidate1()
    # roils <- dir(roipath(), pattern = '*roi.csv')
    tmpl <- paste0(input$site, '_', input$vegtype)
    sum(grepl(tmpl, values$ROIs))+1
  })
  
  
  roiID <- reactive({
    if(input$rois=='New ROI') {
      return(nroi())
    }
    else {
      dummy = 0 
      values$parsedROIList$ID
    }
  })
  curMask <- reactive({
    if(length(values$MASKs)==0) return(NULL)
    values$MASKs[[input$masks]]$rasteredMask
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
  # observe({
  #   dummy =0
  #   maskchoice <- names(values$MASKs)
  #   if(is.null(maskchoice)) 
  #     updateSelectInput(session, 'masks', choices = '')
  #   else if(length(maskchoice)==1)
  #     updateSelectInput(session, 'masks', choices = maskchoice)
  #   else
  #     updateSelectInput(session, 'masks', choices = maskchoice, selected = isolate(input$masks))
  #   dummy =0
  #   
  # }
  # )
  # 
  
  # ----------------------------------------------------------------------
  # VegType
  # ----------------------------------------------------------------------
  observeEvent(input$vegtype,{
    values$slideShow <- 0 
    if(length(values$MASKs)==0) return()
    
    maskNames <- names(values$MASKs)
    f <- function(x, y){
      z <- unlist(strsplit(x, '_'))
      paste(c(z[1], y, z[3:4]), collapse = '_')
    }
    
    newmaskNames <- as.vector(sapply(maskNames, f, y = input$vegtype))
    names(values$MASKs) <- newmaskNames
    updateSelectInput(session, inputId = 'masks', choices = names(values$MASKs))
  })
  
  
  
  # ----------------------------------------------------------------------
  # Mask start and end time
  # ----------------------------------------------------------------------
  observeEvent(input$starttime, 
               {
                 asText <- input$starttime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'starttime', value = asTextNew)
               })
  
  observeEvent(input$endtime, 
               {
                 asText <- input$endtime
                 asTextNew <- fixFormatTime(asText)
                 if(asTextNew!=asText) updateTextInput(session, 'endtime', value = asTextNew)
               })
  
  
  dayYearIDTable <- reactive({
    dummy <- 0
    imgDT()[Site==input$site,.(ID=1:.N,Year, DOY)]
  }    )
  
  
  # ----------------------------------------------------------------------
  # Slideshow
  # ----------------------------------------------------------------------
  observe({
    if(values$slideShow==0) return()
    autoInvalidate2()
    values$contID <- isolate(values$contID) + values$slideShow
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
    # if(input$site=='') return(NA)
    imgDT()[Site==input$site&Year==input$year&DOY==input$viewDay, path][1]
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
  
  
  
  observeEvent(values$contID,{
    dummy <- 0
    if(length(values$contID)==0) values$contID <- 1
    tmpid <- dayYearIDTable()[ID==as.numeric(values$contID),ID]
    tmpday <- dayYearIDTable()[ID==as.numeric(values$contID),DOY]
    tmpyear <- dayYearIDTable()[ID==as.numeric(values$contID),Year]
    
    updateSliderInput(session, 'contID', value = tmpid)
    updateSliderInput(session, 'viewDay', value = tmpday)
    updateSelectInput(session, 'year', selected = tmpyear)
  })
  
  observeEvent(input$contID, {
    values$contID <- input$contID
  })
  
  validDOY <- reactive(dayYearIDTable()[Year==as.numeric(input$year), DOY])
  
  observeEvent(input$year, {
    # values$slideShow <- 0 
    dummy <- 0
    if(length(validDOY())==0) return()
    if(!input$viewDay%in%validDOY()) {
      # tmpx <- abs(validDOY())
      newDOY <- validDOY()[which.min(abs(validDOY() - input$viewDay))+1]
      updateSliderInput(session, 'viewDay', value = newDOY)
      tmpid <- dayYearIDTable()[Year==as.numeric(input$year)&DOY==newDOY, ID]
    }else{
      tmpid <- dayYearIDTable()[Year==as.numeric(input$year)&DOY==as.numeric(input$viewDay), ID]
    }
    # if(length(tmpid)==0) tmpid =1
    values$contID <- tmpid
    updateSliderInput(session, 'contID', value = tmpid)
  })
  
  observeEvent(input$viewDay,{
    # values$slideShow <- 0 
    if(length(validDOY())==0) return()
    tmpx <- NULL
    if(!input$viewDay%in%validDOY()) {
      tmpx <- abs(validDOY())
      newDOY <- validDOY()[which.min(abs(validDOY() - input$viewDay))+1]
      updateSliderInput(session, 'viewDay', value = newDOY)
      tmpid <- dayYearIDTable()[Year==as.numeric(input$year)&DOY==newDOY, ID]
    }else{
      tmpid <- dayYearIDTable()[Year==as.numeric(input$year)&DOY==as.numeric(input$viewDay), ID]
    }
    # if(length(tmpid)==0) tmpid =1
    values$contID <- tmpid
    updateSliderInput(session, 'contID', value = tmpid)
  })
  
  
  # ----------------------------------------------------------------------
  # Plot image
  # ----------------------------------------------------------------------
  output$plot <- renderPlot(width = 350, {
    if(is.na(sampleImage())){
      par(mar=c(0,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
      text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No image for this date was found!', font=2, adj=.5)
    }else{
      par(mar=c(0,0,0,0))
      plotJPEG(sampleImage())
      roicol <- if (input$roicol=='transparent') '#ffffff00' else paste0(input$roicol, '80')
      polygon(values$centers, col = roicol, pch = 9, lwd=2)
    }
  })
  
  observeEvent(input$masks, {
    values$slideShow <- 0 
    tmpmask <- values$MASKs[[input$masks]]
    
    values$centers <- tmpmask$maskpoints
    updateDateRangeInput(session, inputId = 'roiDateRange', start=tmpmask$startdate)
    updateDateRangeInput(session, inputId = 'roiDateRange', end=tmpmask$enddate)
    # updateTimeInput(session, inputId = 'starttime', value = tmpmask$starttime)
    # updateTimeInput(session, inputId = 'endtime', value = tmpmask$endtime)
    updateTextInput(session, inputId = 'starttime', value = tmpmask$starttime)
    updateTextInput(session, inputId = 'endtime', value = tmpmask$endtime)
    updateSelectInput(session, inputId = 'year', selected = tmpmask$sampleyear)
    updateSelectInput(session, inputId = 'viewDay', selected = tmpmask$sampleday)
  })
  
  observeEvent(input$newPoint, {
    values$slideShow <- 0 
    newPoint <- matrix(c(input$newPoint[['x']], input$newPoint[['y']]),1, 2)
    values$centers <- rbind(values$centers, newPoint)
  })
  
  
  
  
  
  observeEvent(input$cancel, {
    values$slideShow <- 0 
    values$centers <- matrix(numeric(), 0, 2)
  })
  
  observeEvent(input$undo, {
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
    ROIList <- list(siteName = input$site, 
                    vegType = input$vegtype, 
                    ID = roiID(),
                    Owner= input$owner, 
                    Description = input$descr, 
                    createDate = strftime(systime, format = '%Y-%m-%d'),
                    createTime = strftime(systime, format = '%H:%M:%S'),
                    updateDate = strftime(systime, format = '%Y-%m-%d'),
                    updateTime = strftime(systime, format = '%H:%M:%S'),
                    masks = values$MASKs)
    
    # roifilename <- input$rois
    # if(input$rois=='New ROI')roifilename <- paste(ROIList$siteName, ROIList$vegType, sprintf('%04d', ROIList$ID), 'roi.csv', sep = '_')
    roifilename <- paste0(roilabel(),'_roi.csv')
    writeROIListFile(ROIList, path = roipath(),  roifilename)
    
    showModal(modalDialog(title = 'Complete',width='300px',
                          "New file for ROI List was saved in the database!",
                          easyClose = T,
                          size = 's',
                          # footer = modalButton('OK')
                          footer = NULL
    ))
  })
  
  
  
  # ----------------------------------------------------------------------
  # Download ROI List
  # ----------------------------------------------------------------------
  output$downloadROI <- downloadHandler(
    filename = function(){
      make.names(paste0(input$owner, '_',roilabel(),'_roi.zip'))
    },
    content = function(fname){
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      systime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      ROIList <- list(siteName = input$site, 
                      vegType = input$vegtype, 
                      ID = roiID(),
                      Owner= input$owner, 
                      Description = input$descr, 
                      createDate = strftime(systime, format = '%Y-%m-%d'),
                      createTime = strftime(systime, format = '%H:%M:%S'),
                      updateDate = strftime(systime, format = '%Y-%m-%d'),
                      updateTime = strftime(systime, format = '%H:%M:%S'),
                      masks = values$MASKs)
      
      roifilename <- paste0(roilabel(),'_roi.csv')
      writeROIListFile(ROIList, path = '',  roifilename)
      fs <- c(roifilename, 
              paste0(names(ROIList$masks), '.tif'),
              paste0(names(ROIList$masks), '_vector.csv'))
      zip(zipfile=fname, files=fs)
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
    ROIList <- list(siteName = input$site, 
                    vegType = input$vegtype, 
                    ID = roiID(),
                    Owner= input$owner, 
                    Description = input$descr, 
                    createDate = strftime(systime, format = '%Y-%m-%d'),
                    createTime = strftime(systime, format = '%H:%M:%S'),
                    updateDate = strftime(systime, format = '%Y-%m-%d'),
                    updateTime = strftime(systime, format = '%H:%M:%S'),
                    masks = values$MASKs)
    
    roifilename <- paste0(roilabel(),'_roi.csv')
    writeROIListFile(ROIList, path = '',  roifilename)
    fs <- c(roifilename, 
            paste0(names(ROIList$masks), '.tif'),
            paste0(names(ROIList$masks), '_vector.csv'))
    fname <- make.names(paste0(input$owner, '_',roilabel(),'_roi.zip'))
    
    zip(zipfile=fname, files=fs)
    
    msg <- paste0(
      '---------\n',
      'Submit time: \t', as.character(Sys.time()), '\n',
      # 'IP address: \t', as.character(system('ipconfig getifaddr en0', intern=TRUE)),'\n',
      'Site: \t', input$site, '\n',
      'Owner: \t', input$owner, '\n',
      '---------\n'
    )
    attachmentObject <- mime_part(x = fname, name = fname)
    bodyWithAttachment <- list(msg, attachmentObject)
    
    
    sendmail(from = 'phenocam.network@gmail.com', 
             to = 'phenocam.network@gmail.com', 
             # subject = paste0('drawROI error submitted at ', as.character(Sys.time())), 
             subject = 'New ROI was just submitted via drawROI!', 
             msg = bodyWithAttachment)
    
    showModal(modalDialog(title = 'ROI was submitted!',width='250px',
                          "The new ROI will be reviewed shortly.",
                          easyClose = T,
                          size = 'm',
                          footer = NULL
    ))
    
    
  })
  
  
  # ----------------------------------------------------------------------
  # tsYearDayRange
  # ----------------------------------------------------------------------
  
  
  tsYearDayRange <- reactive({
    if(input$sevenorall=="week")
      # return(input$viewDay[1]:(input$viewDay[1]+7))
      return(imgDT()[Site==input$site&Year==input$year&DOY%in%(input$viewDay[1]:(input$viewDay[1]+7)),YearDOY])
    else if(input$sevenorall=="year")
      # return(1:365)
      return(imgDT()[Site==input$site&Year==input$year,YearDOY])
    else if(input$sevenorall=="all")
      return(imgDT()[Site==input$site,YearDOY])
  })
  
  paths <- reactive(
    imgDT()[Site==input$site&YearDOY%in%tsYearDayRange(), .(paths=path[1]),.(conT, Year, DOY)]
  )
  
  ccVals <- eventReactive(input$extract,{
    if(is.null(curMask())|length(paths()$path)==0) return(data.frame(rcc=NA, gcc=NA, bcc=NA))
    extractCCCTimeSeries(isolate(curMask()), paths()$path)
  })
  
  ccTime <- eventReactive(input$extract,{
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
      
      
      if(input$extract==0|is.null(isolate(curMask()))){
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
        ccSel <- as.vector(sapply(input$ccselect, switch, R='red', G='green',  B='blue'))
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
      ccSel <- as.vector(sapply(input$ccselect, switch, R='red', B='blue', G="green"))
      d <- d[band%in%ccSel]
      
      p <- plot_ly(data = d, x=~time, y= ~cc,
              color = ~band, 
              colors = c('#FF4615','#007D00','#2364B7'),
              type = 'scatter', mode = 'lines+markers') %>%
        layout(xaxis = xAxis, yaxis = yAxis)
      hide_legend(p)
      
    })
  
  
  # ----------------------------------------------------------------------
  # Accept mask
  # ----------------------------------------------------------------------
  
  observeEvent(input$accept,{
    values$slideShow <- 0 
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
                    rasteredMask = createRasteredROI(values$centers, sampleImageSize()))
    
    tmp <- values$MASKs
    tmp[[length(tmp)+1]] <-  newMask
    tmpName <- paste(input$site, input$vegtype, 
                     sprintf('%04d',roiID()),
                     sprintf('%02d',length(tmp)), sep = '_')
    names(tmp)[length(tmp)] <- tmpName
    values$MASKs <- tmp
    updateSelectInput(session, inputId = 'masks', choices = names(tmp), selected = tmpName)
    
    # values$msk <- tmp$rasteredMask
  })
  
  # ----------------------------------------------------------------------
  # Save mask
  # ----------------------------------------------------------------------
  
  observeEvent(input$save,{
    values$slideShow <- 0 
    if(is.null(curMask()))return()
    if(is.null(values$centers)) return()
    if (nrow(values$centers)<3) return()
    
    newMASK <- createRasteredROI(values$centers, sampleImageSize())
    tmpMask <- list(maskpoints = values$centers, 
                    startdate = input$roiDateRange[1], 
                    enddate = input$roiDateRange[2], 
                    starttime = input$starttime, 
                    endtime = input$endtime, 
                    sampleyear = input$year, 
                    sampleday = input$viewDay,
                    sampleImage = sampleImageName(),
                    rasteredMask = newMASK)
    
    values$MASKs[[input$masks]] <- tmpMask
    
    showModal(modalDialog(title = 'Complete',width='300px',
                          "Mask info was saved!",
                          easyClose = T,
                          size = 's',
                          footer = NULL
    ))
    
    
  })
  
  # ----------------------------------------------------------------------
  # Plot mask
  # ----------------------------------------------------------------------
  
  output$maskplot <- 
    renderPlot(width = 350, {
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
      plot(NA,xlim=c(1,res[2]),ylim=c(1,res[1]), type='n',
           xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
      dummy=0
      writeTIFF(mask*1, '.tmpraster.tif')
      rmask <- raster('.tmpraster.tif')
      rmask[rmask!=0] <- NA
      
      plot(rmask,legend=F, add=T, col='black')
      file.remove('.tmpraster.tif')
      # plot(mask,legend=F, add=T)
      # }
    })
  
  # ----------------------------------------------------------------------
  # Download timeseries
  # ----------------------------------------------------------------------
  
  output$downloadTSData <- downloadHandler(
    filename = function() {
      paste('timeseries-', input$masks, '-', format(Sys.time(), format = '%Y-%m-%d-%H%M%S'), ".csv", sep="")
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
  shinyjs::disable("vegtype")
  shinyjs::disable("shiftsList")
  shinyjs::disable("gotoShiftFOV")
  
})

