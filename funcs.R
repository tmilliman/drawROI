library(lubridate)
library(data.table)
library(raster)
library(shiny)
library(tiff)
library(jpeg)

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


extractCCC.Plus <- function(path, mmsk){
  jp <- readJPEG(path)
  
  jp.m <- apply(jp, 3, '*', mmsk)
  
  DT <- data.table(r = jp.m[,1],
                   g = jp.m[,2],
                   b = jp.m[,3])
  
  DT[,c('rcc', 'gcc', 'bcc'):= .(r/(r+g+b), 
                                 g/(r+g+b), 
                                 b/(r+g+b))]
  
  list(rcc = DT[,mean(rcc, na.rm=T)],
       gcc = DT[,mean(gcc, na.rm=T)],
       bcc = DT[,mean(bcc, na.rm=T)])
}


extractCCC <- function(path, mmsk){
  jp <- readJPEG(path)
  msk <- mmsk
  # msk[is.na(msk)] <- 0
  DT <- data.table(r = as.vector(jp[,,1]),
                   g = as.vector(jp[,,2]),
                   b = as.vector(jp[,,3]),
                   m = as.vector(msk))
  DT[,rcc:= r/(r+g+b)]
  DT[,gcc:= g/(r+g+b)]
  DT[,bcc:= b/(r+g+b)]
  
  DT[r+g+b==0,rcc:= 0]
  DT[r+g+b==0,gcc:= 0]
  DT[r+g+b==0,bcc:= 0]
  
  list(DT = DT,
       rcc = DT[,mean(m*rcc, na.rm=T)],
       gcc = DT[,mean(m*gcc, na.rm=T)],
       bcc = DT[,mean(m*bcc, na.rm=T)])
}

createRasteredROI <- function(pnts, path){
  jp <- readJPEG(path)
  res <- dim(jp)[1:2]
  ext <- extent(1,res[1], 1, res[2])
  poly <- as(ext  ,"SpatialPolygons")
  poly@polygons[[1]]@Polygons[[1]]@coords <- as.matrix(pnts)
  r <- rasterize(poly, raster(ext, nrow = res[1], ncol = res[2]))
  r
}

extractCCCTimeSeries <- function(rmsk, paths, PLUS=F){
  # rmsk <- createRasteredROI(pnts, paths[1])
  mmsk <- as.matrix(rmsk)
  
  n <- length(paths)
  CCCT <- as.data.table(matrix(0, nrow=n, ncol=3))
  colnames(CCCT) <- c('rcc','gcc','bcc')
  
  extractCCCFunc <- extractCCC
  if(PLUS) extractCCCFunc <- extractCCC.Plus
  
  # if(exists('session'))
  withProgress(value = 0, message = 'Extracting CCs',
               for(i in 1:n){
                 ccc <- extractCCCFunc(paths[i], mmsk)
                 CCCT[i,] <- as.data.table(ccc[c("rcc", "gcc", "bcc")])
                 incProgress(1/n)
               }
  )
  # else
  #   for(i in 1:n){
  #     ccc <- extractCCCFunc(paths[i], mmsk)
  #     CCCT[i,] <- as.data.table(ccc[c("rcc", "gcc", "bcc")])
  #   }
  
  # list(TS = CCCT, mask= rmsk)
  CCCT
}



writeROIListFile <- function(ROIList, path='ROI/'){
  
  filename <- paste(ROIList$siteName, ROIList$vegType, sprintf('%04d', ROIList$ID), 'roi.csv', sep = '_')
  
  updateTime <- Sys.time()
  hdrText <- paste0('#\n# ROI List for ', ROIList$siteName,
                    '\n#',
                    '\n# Site: ', ROIList$siteName,
                    '\n# Veg Type: ', ROIList$vegType, 
                    '\n# ROI ID Number: ', sprintf('%04d', ROIList$ID),
                    '\n# Owner: ', ROIList$Owner,
                    '\n# Creation Date: ', strftime(ROIList$createTime, format = '%Y-%m-%d'),
                    '\n# Creation Time: ', strftime(ROIList$createTime, format = '%H:%M:%S'),
                    '\n# Update Date: ', strftime(updateTime, format = '%Y-%m-%d'),
                    '\n# Update Time: ', strftime(updateTime, format = '%H:%M:%S'),
                    '\n# Description: ', ROIList$Description,
                    '\n#\n')
  
  bdyText <- 'start_date,start_time,end_date,end_time,maskfile,sample_image\n'
  
  for(i in 1:length(ROIList$masks)){
    m <- as.matrix(ROIList$masks[[i]]$rasteredMask)
    m[m==1] <- 0
    m[is.na(m)] <- 1
    rName <- paste0(ROIList$siteName, '_',
                    ROIList$vegType, '_',
                    sprintf('%04d', ROIList$ID), '_',
                    sprintf('%02d', i)
    )
    
    
    
    writeTIFF(m , where = paste0(path, rName,'.tif'))
    
    
    maskpoints <- ROIList$masks[[i]]$maskpoints
    maskpoints <- rbind(dim(m), maskpoints)
    write.table(maskpoints, file = paste0(path, rName,'_vector.csv'), col.names = F, row.names = F, sep = ',')
    
    bdyLine <- paste( ROIList$masks[[i]]$startdate,
                      ROIList$masks[[i]]$starttime,
                      ROIList$masks[[i]]$enddate, 
                      ROIList$masks[[i]]$endtime,
                      rName,
                      ROIList$masks[[i]]$sampleImage, sep = ',')
    
    
    bdyText <- paste0(bdyText, bdyLine, '\n')
  }
  fcon <- file(paste0(path,filename))
  writeLines(paste0(hdrText, bdyText), con = fcon)
  close(fcon)
}

readROIFolder <- function(){
  
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
  fls <- dir(roipath, gsub(pattern = 'roi.csv', '', roifilename))
  
  
  roilines <- readLines(paste0(roipath, roifilename))
  roilinesParsed <- sapply(roilines[4:12], strsplit, ': ')
  roilinesParsed <- as.vector(unlist(sapply(roilinesParsed, '[',2)))
  
  masks <- read.csv(paste0(roipath, roifilename), skip = 13)
  
  tifls <- fls[grepl('.tif', fls)]
  vecls <- fls[grepl('vector.csv', fls)]
}

