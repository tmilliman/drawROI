

server <- function(input, output, session) {
  
  values <- reactiveValues(centers = matrix(numeric(), 0, 2),
                           ROIs = list())
  
  imgFile <- reactive(imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path][1])
  
  isolate({
    dmin <- imgDT[Site==input$site, min(Date, na.rm = T)]
    dmax <- imgDT[Site==input$site, max(Date, na.rm = T)]
    updateDateRangeInput(session , inputId = 'roiDateRange', min = dmin, max = dmax)  
  })
  
  observe(
    updateSliderInput(session,
                      inputId = 'viewDay', 
                      value = imgDT[Site==input$site&Year==input$year, min(DOY, na.rm = T)]))
  observeEvent(input$site, 
               {
                 dmin <- imgDT[Site==input$site, min(Date)]
                 dmax <- imgDT[Site==input$site, max(Date)]
                 updateDateRangeInput(session , inputId = 'roiDateRange',
                                      min = dmin, max = dmax,
                                      start = dmin, end = dmax)  
               })
  
  # curROI <- reactive({
  #   if(length(values$ROIs)==0) return(NULL)
  #   else values$ROIs[[input$rois]]
  # })
  
  curROI <- eventReactive(input$rois,
                          {
                            if(length(values$ROIs)==0) return(NULL)
                            else values$ROIs[[input$rois]]
                          })
  
  output$plot <- renderPlot({
    
    
    if(is.na(imgFile())){
      par(mar=c(0,0,0,0))
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
      text(0.5,0.5, 'No image for this date was found!', font=2, adj=.5)
    }else{
      par(mar=c(0,0,0,0))
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
                 updateSelectInput(session, inputId = 'rois', choices = names(tmp))
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
  
  paths <- reactive(
    imgDT[Site==input$site&Year==input$year&DOY%in%ccRange()&Hour==12, .(paths=path[1]),DOY]
    )
  
  ccVals <- eventReactive(input$extract,{
    if(is.null(curROI())|length(paths()$path)==0) return(data.frame(rcc=NA, gcc=NA, bcc=NA))
    pnts <- isolate(curROI())
    # paths <- imgDT[Site==input$site&Year==input$year&DOY%in%ccRange()&Hour==12&Minute<30, path]
    extractCCCTimeSeries(pnts, paths()$path)
  })
  
  ccTime <- eventReactive(input$extract,{
    if(is.null(curROI())) return(NA)
    if(input$sevenorall=="First 7 days")
      return(paths()[DOY%in%ccRange(),DOY])
    else 
      return(paths()$DOY)
  })
  
  output$timeSeries <- 
    renderPlot({
      par(mar=c(4,4,0,0))
      # layout(matrix(c(rep(1,4),2), nrow=1))
      plot(NA, 
           xlim=c(input$dateRange[1], input$dateRange[2]), ylim = c(0.20,.45),
           type = 'l', xlab='', ylab='')
      mtext(side = 1, text = 'Day of year', font=2, line=2.5)
      mtext(side = 2, text = 'GCC (-)', font=2, line=2.5)
      if(input$extract==0) return()
      # imgFile <- imgDT[Site==input$site&Year==input$year&DOY==input$viewDay&Hour==12, path][1]
      # if(is.na(imgFile)|input$accept==0|nrow(values$centers)<3) return()
      # if(is.na(imgFile)|input$accept==0|nrow(values$centers)<3|input$extract==0) return()
      
      TS <- ccVals()$TS
      
      lines(ccTime(), TS$rcc,  col='red', lwd=2)
      lines(ccTime(), TS$gcc,  col='green', lwd=2)
      lines(ccTime(), TS$bcc,  col='blue', lwd=2)
      
      
      
      # mask <- ccVals()$mask
      # # plot(mask, legend=F)
      # res <- dim(mask)
      # plot(NA,xlim=c(1,res[1]),ylim=c(1,res[2]), type='n',
      #      xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
      # plot(mask,legend=F, add=T, col='black')
      
    })
  
  msk <- eventReactive(input$accept,{
    pnts <- values$centers
    if(nrow(pnts)<3) return(NULL)
    # paths <- imgDT[Site==input$site&Year==input$year&DOY%in%ccRange()&Hour==12&Minute<30, path]
    
    createRasteredROI(pnts, imgFile())
  })
  
  output$testplot <- renderPlot({
    par(mar=c(0,0,0,0))
    plot(1:10)
  })
  
  output$maskplot <- 
    renderPlot({
      par(mar=c(0,0,0,0))
      if(is.null(msk())){
        plot(1:10,
             type='n',
             xaxs='i',yaxs='i',
             xaxt='n',yaxt='n',
             xlab='',ylab='',
             bty='o'
        )
      }else{
        
        # mask <- ccVals()$mask
        
        mask <- msk()
        res <- dim(mask)
        par(mar=c(0,0,0,0))
        plot(NA,xlim=c(1,res[1]),ylim=c(1,res[2]), type='n',
             xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
        plot(mask,legend=F, add=T, col='black')
      }
    })
}