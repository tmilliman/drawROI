library(shiny)
library(shinyTime)
library(shinyjs)
library(colourpicker)

library(sp)
library(raster)
library(jpeg)
library(tiff)

library(data.table)
library(lubridate)
library(plotly)



plotJPEG <- function(path, add=FALSE)
{
  # jpgNonNative <-  readJPEG(path, native=F) # read the file
  jpgNonNative <- NULL
  jpgNative <-  readJPEG(path, native=T) # read the file
  res <-  dim(jpgNative)[1:2] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(NA,xlim=c(1,res[1]),ylim=c(1,res[2]), type='n',
         xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
  rasterImage(jpgNative,1,1,res[1],res[2])
  invisible(list(res=res, jpgNonNative=jpgNonNative, jpgNative=jpgNative))
  # invisible(res)
}



draw.polygon <-
  function (col = "#80303080", lty = 1, ...) 
  {
    
    xy <- locator(2)
    lines(xy$x, xy$y, lty = lty)
    
    while(is.list(c1 <- locator(1))) {
      xy$x <- c(xy$x, c1$x)
      xy$y <- c(xy$y, c1$y)
      lines(xy$x, xy$y, lty = lty)
    }
    xy <- data.frame(xy)
    xy <- rbind(xy, xy[1, ])
    polygon(xy$x, xy$y, lty = lty, col = col, ...)
    
    invisible(xy)
  }




# extractCCC.Plus <- function(path, mmsk){
#   jp <- readJPEG(path)
#   
#   jp.m <- apply(jp, 3, '*', mmsk)
#   
#   DT <- data.table(r = jp.m[,1],
#                    g = jp.m[,2],
#                    b = jp.m[,3])
#   
#   DT[,c('rcc', 'gcc', 'bcc'):= .(r/(r+g+b), 
#                                  g/(r+g+b), 
#                                  b/(r+g+b))]
#   
#   list(rcc = DT[,mean(rcc, na.rm=T)],
#        gcc = DT[,mean(gcc, na.rm=T)],
#        bcc = DT[,mean(bcc, na.rm=T)])
# }
# 



extractCCC <- function(path, mmm){
  
  jp <- readJPEG(path)
  dm <- dim(jp)
  printLog(paste(dim(dm), collapse = ' '))
  rgb <- jp
  dim(rgb) <- c(dm[1]*dm[2],3)
  printLog(paste(dim(rgb), collapse = ' '))
  printLog(paste(dim(mmm), collapse = ' '))
  mrgb <- rgb*mmm
  RGB <- colMeans(mrgb, na.rm = T)
  printLog(paste(RGB, collapse = ' '))
  
  RGBTOT <- sum(RGB)
  
  if(RGBTOT==0) {
    rcc <- 0
    gcc <- 0
    bcc <- 0
  }else{
    rcc <- RGB[1]/RGBTOT
    gcc <- RGB[2]/RGBTOT
    bcc <- RGB[3]/RGBTOT
  }
  
  list(rcc = rcc,
       gcc = gcc,
       bcc = bcc)
}



createRasteredROI <- function(pnts, imgSize){
  
  pnts <- t(apply(pnts, 1, '*', imgSize))
  ext <- extent(1, imgSize[1], 1, imgSize[2])
  poly <- as(ext  ,"SpatialPolygons")
  # poly@polygons[[1]]@Polygons[[1]]@coords <- as.matrix(pnts)
  
  tbl <- as.data.table(na.omit(cbind(pnts,cumsum(is.na(pnts[,1]))+1 )))
  colnames(tbl) <- c('x', 'y', 'g')
  ng <- table(tbl$g)
  
  polyList <- list()
  np <- length(ng[which(ng>=3)])
  
  for(gi in 1:np)
    polyList[[gi]] <- as.matrix(tbl[g==gi, .(x,y)])
  
  polys <- SpatialPolygons(
    lapply(1:np,
           function(x){
             p <- slot(poly@polygons[[1]], "Polygons")[[1]]
             slot(p, "coords") <- polyList[[x]]  
             pp <- Polygons(list(p), ID = x)
             return(pp)
           })
  )
  
  r <- rasterize(polys, raster(ext, nrow = imgSize[1], ncol = imgSize[2]))
  r[!is.na(r)] <- 1
  
  m1 <- as.matrix(r)
  m <- m1
  m[m1==0|is.na(m1)] <- 1
  m[m1!=0] <- 0
  m
}



extractCCCTimeSeries <- function(rmsk, paths, PLUS=F, session=shiny::getDefaultReactiveDomain()){
  
  continue = TRUE
  
  mmsk <- 1-as.matrix(rmsk)
  msk <- mmsk
  m <- as.vector(mmsk)
  m[m==0] <- NA
  mmm <- cbind(m, m, m)
  
  n <- length(paths)
  CCCT <- matrix(0, nrow=n, ncol=3)
  
  
  # extractCCCFunc <- extractCCC
  # if(PLUS) extractCCCFunc <- extractCCC.Plus
  
  # if(exists('session'))
  withProgress(value = 0, message = 'Extracting CCs',
               for(i in 1:n){
                 if(isTRUE(session$input$stopThis))break
                 printLog(paths[i])
                 printLog(i)
                 
                 ccc <- extractCCC(paths[i], mmm)
                 CCCT[i,] <- as.numeric((ccc[c("rcc", "gcc", "bcc")]))
                 incProgress(1/n)
                 # Sys.sleep(1)
                 httpuv:::service()
               }
  )
  CCCT <- as.data.table(CCCT)
  colnames(CCCT) <- c('rcc','gcc','bcc')
  CCCT
}




writeROIListFile <- function(ROIList, path='ROI/', roifilename){
  
  updateTime <- Sys.time()
  hdrText <- paste0('#\n# ROI List for ', ROIList$siteName,
                    '\n#',
                    '\n# Site: ', ROIList$siteName,
                    '\n# Veg Type: ', ROIList$vegType, 
                    '\n# ROI ID Number: ', sprintf('%04d', ROIList$ID),
                    '\n# Owner: ', ROIList$Owner,
                    '\n# Creation Date: ', ROIList$createDate,
                    '\n# Creation Time: ', ROIList$createTime,
                    '\n# Update Date: ', strftime(updateTime, format = '%Y-%m-%d'),
                    '\n# Update Time: ', strftime(updateTime, format = '%H:%M:%S'),
                    '\n# Description: ', ROIList$Description,
                    '\n#\n')
  
  
  bdyText <- 'start_date,start_time,end_date,end_time,maskfile,sample_image\n'
  
  for(i in 1:length(ROIList$masks)){
    # m <- as.matrix(ROIList$masks[[i]]$rasteredMask)
    m <- ROIList$masks[[i]]$rasteredMask
    # m[m==1] <- 0
    # m[is.na(m)] <- 1
    # rName <- paste0(ROIList$siteName, '_',
    #                 ROIList$vegType, '_',
    #                 sprintf('%04d', ROIList$ID), '_',
    #                 sprintf('%02d', i)
    # )
    
    rName <- names(ROIList$masks)[i]
    
    writeTIFF(m*1 , where = paste0(path, rName,'.tif'))
    
    maskpoints <- ROIList$masks[[i]]$maskpoints
    maskpoints <- rbind(dim(m), maskpoints)
    if(nrow(maskpoints)>3)
      write.table(maskpoints, file = paste0(path, rName,'_vector.csv'), col.names = F, row.names = F, sep = ',')
    # }
    # for(i in 1:length(ROIList$masks)){
    
    bdyLine <- paste( ROIList$masks[[i]]$startdate,
                      ROIList$masks[[i]]$starttime,
                      ROIList$masks[[i]]$enddate, 
                      ROIList$masks[[i]]$endtime,
                      paste0(rName,'.tif'),
                      ROIList$masks[[i]]$sampleImage, sep = ',')
    
    
    bdyText <- paste0(bdyText, bdyLine, '\n')
  }
  allText <- paste0(hdrText, bdyText)
  writeLines(allText, paste0(path, roifilename))
  # fcon <- file(paste0(path, roifilename))
  # writeLines(paste0(hdrText, bdyText), con = fcon)
  # close(fcon)
}




filePathParse <- function(filenames)
{
  imgDT <- data.table(filenames = filenames)
  imgDT$tmp <- unlist(lapply(filenames, function(x){strsplit(x,split = '/', fixed = T)[[1]][3]}))
  imgDT[,c('Site', 'Year', 'Month','Day','HHMMSS'):=as.data.table(matrix(unlist(strsplit(gsub(tmp,pattern = '.jpg', replacement = ''), split = '_')), ncol=5, byrow = T))]
  imgDT[,Year:=as.numeric(Year)]
  imgDT[,Month:=as.numeric(Month)]
  imgDT[,Day:=as.numeric(Day)]
  imgDT[,HHMMSS:=as.numeric(HHMMSS)]
  imgDT[,Hour:=floor(HHMMSS/10000)]
  imgDT[,Minute:=floor((HHMMSS%%10000)/100)]
  imgDT[,Second:=HHMMSS%%100]
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  imgDT[,Date:=date(ISOdate(Year, Month, Day))]
  imgDT[,DateTime:=ISOdatetime(Year, Month, Day, Hour, Minute, Second)]
  imgDT[,conT:=Year+DOY/(365+(2001%%4==0))]
  imgDT[,YearDOY:=Year+DOY/1000]
  # imgDT <- imgDT[Hour==12&Minute<30,]
  imgDT
}


fixFormatTime <- function(asText){
  asSplit <- unlist(strsplit(asText, ':'))
  for(i in 1:length(asSplit))
    if(any(!unlist(strsplit(asSplit[i], ''))%in%as.character(0:9))) asSplit[i] <- 0
    
    asNum <- as.numeric(asSplit)
    if(length(asNum)>3) asNum <- asNum[1:3]
    if(length(asNum)==0) asNum <- c(0, 0, 0)
    if(length(asNum)==1) asNum <- c(asNum[1], 0,0)
    if(length(asNum)==2) asNum <- c(asNum[1:2], 0)
    
    asNum[1] <- min(23, max(asNum[1], 0))
    asNum[2] <- min(59, max(asNum[2], 0))
    asNum[3] <- min(59, max(asNum[3], 0))
    
    asTextNew <- sapply(asNum, sprintf, fmt='%02d')
    asTextNew <- paste(asTextNew, collapse = ':')
    asTextNew
}





parseROI <- function(roifilename, roipath){
  # fls <- dir(roipath, gsub(pattern = 'roi.csv', '', roifilename))
  fname <- paste0(roipath, roifilename)
  if(!file.exists(fname)) return(NULL)
  
  roilines <- readLines(fname)
  
  wEmptyLine <- roilines%in%c('', ' ',  '  ')
  wCommented <- as.vector(sapply(roilines, grepl,  pattern = '^#'))
  wNotSkip <- !(wEmptyLine|wCommented)
  
  
  parseroiline <- function(roilines, property){
    wProp <- grepl(roilines, pattern = property)
    gsub(roilines[wProp], pattern = paste0('# ', property, ': '), replacement = '')
  }
  
  ROIList <- list(siteName = parseroiline(roilines[wCommented], 'Site'), 
                  vegType = parseroiline(roilines[wCommented], 'Veg Type'), 
                  ID = as.numeric(parseroiline(roilines[wCommented], 'ROI ID Number')), 
                  Owner = parseroiline(roilines[wCommented], 'Owner'), 
                  createDate = parseroiline(roilines[wCommented], 'Creation Date'), 
                  createTime = parseroiline(roilines[wCommented], 'Creation Time'), 
                  updateDate = parseroiline(roilines[wCommented], 'Update Date'), 
                  updateTime = parseroiline(roilines[wCommented], 'Update Time'), 
                  Description = parseroiline(roilines[wCommented], 'Description'), 
                  masks = NULL)
  
  
  parsedMasks <- read.table(textConnection(roilines[which(wNotSkip)]), sep = ',', header = T)
  # parsedMasks <- read.csv(paste0(roipath, roifilename), skip = nskips)
  
  # tifls <- fls[grepl('.tif', fls)]
  # vecls <- fls[grepl('vector.csv', fls)]
  masksList <- list()
  for(i in 1:nrow(parsedMasks)){
    maskpath <- paste0(roipath, parsedMasks$maskfile[i])
    maskpointspath <- gsub(maskpath, pattern = '.tif', replacement = '_vector.csv')
    if(file.exists(maskpointspath)) {
      dummy=0
      maskpoints <- as.matrix(read.csv(maskpointspath, header = F, skip = 1))
    }else{
      maskpoints <- NULL
      # r <- raster(maskpath)
      # maskpoints <- maskRaster2Vector(r)
      # maskpointsTmp <- rbind(dim(r), maskpoints)
      # write.table(maskpointsTmp, file =maskpointspath, col.names = F, row.names = F, sep = ',')
    }
    dummy=0
    tmpMask <- list(maskpoints = maskpoints, 
                    startdate = as.character(parsedMasks$start_date[i]), 
                    enddate = as.character(parsedMasks$end_date[i]), 
                    starttime = as.character(parsedMasks$start_time[i]), 
                    endtime = as.character(parsedMasks$end_time[i]), 
                    sampleyear = NULL, 
                    sampleday = NULL,
                    sampleImage = as.character(parsedMasks$sample_image[i]),
                    rasteredMask = as.matrix(raster(maskpath)))
    tmpMask$rasteredMask[(!is.na(tmpMask$rasteredMask))&tmpMask$rasteredMask!=0] <- 1
    
    
    sampleYMD <- strsplit(tmpMask$sampleImage, split = '_')[[1]][2:4]
    tmpMask$sampleyear <- as.numeric(sampleYMD)[1]
    tmpMask$sampleday <- yday(paste(sampleYMD, collapse = '-'))
    
    masksList[[length(masksList)+1]] <- tmpMask
    
  }
  names(masksList) <- gsub(parsedMasks$maskfile, pattern = '.tif', replacement = '')
  ROIList$masks <- masksList
  
  ROIList
}





maskRaster2Vector <- function(r){
  r[r==0] <- 1
  r[r==255] <- NA
  
  p <- rasterToPolygons(r, dissolve = T)
  c <- p@polygons[[1]]@Polygons[[1]]@coords
  c
}


parseIMG.DT <- function(imgDT){
  
  imgDT[,c('Site', 'Year', 'Month','Day','HHMMSS'):=as.data.table(matrix(unlist(strsplit(gsub(filenames, pattern = '.jpg', replacement = ''), split = '_')), ncol=5, byrow = T))]
  imgDT[,Year:=as.numeric(Year)]
  imgDT[,Month:=as.numeric(Month)]
  imgDT[,Day:=as.numeric(Day)]
  imgDT[,HHMMSS:=as.numeric(HHMMSS)]
  imgDT[,Hour:=floor(HHMMSS/10000)]
  imgDT[,Minute:=floor((HHMMSS%%10000)/100)]
  imgDT[,Second:=HHMMSS%%100]
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  imgDT[,Date:=date(ISOdate(Year, Month, Day))]
  imgDT[,DateTime:=ISOdatetime(Year, Month, Day, Hour, Minute, Second)]
  imgDT[,conT:=Year+DOY/(365+(2001%%4==0))]
  imgDT[,YearDOY:=Year+DOY/1000]
  # imgDT <- imgDT[Hour==12&Minute<30,]
  imgDT
}



getIMG.DT <- function(sites, midddayListPath){
  imgDT <- data.table()
  
  for(site in sites){
    tbl <- read.table(paste0(midddayListPath, site), header = F, colClasses = 'character', col.names = 'path')
    imgDT.tmp <- as.data.table(tbl)
    imgDT <- rbind(imgDT, imgDT.tmp)
  }
  
  
  splt <- imgDT[, tstrsplit(path, split = '/')]
  
  colnames(splt) <- c('empty','data','archive','site','year','month','filenames') 
  splt[, newpath:=paste(empty, data, archive, site, 'originals', year, month, filenames, sep='/')]
  
  imgDT$filenames <- splt$filenames
  imgDT$newpath <- splt$newpath
  
  #imgDT[grepl(pattern = 'NEON', filenames), path:=newpath]
  imgDT$newpath <- NULL
  
  imgDT <- imgDT[str_count(filenames, pattern = '_')==4, ]
  imgDT <- parseIMG.DT(imgDT)
  imgDT
}



printLog <- function(msg){
  message(paste(as.character(Sys.time()), msg, '\t'))
}