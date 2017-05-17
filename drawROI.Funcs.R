library(data.table)
library(raster)
library(shiny)
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


extractCCC <- function(path, rmsk){
  jp <- readJPEG(path)
  msk <- rmsk
  # msk[is.na(msk)] <- 0
  DT <- data.table(r = as.vector(jp[,,1]),
                   g = as.vector(jp[,,2]),
                   b = as.vector(jp[,,3]),
                   m=as.vector(msk))
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
  ext <- extent(1,res[2], 1, res[1])
  poly <- as(ext  ,"SpatialPolygons")
  poly@polygons[[1]]@Polygons[[1]]@coords <- as.matrix(pnts)
  # plot(poly, add=T)
  r <- rasterize(poly, raster(ext, ncol = res[2], nrow = res[1]))
  r
}

extractCCCTimeSeries <- function(pnts, paths){
  rmsk <- createRasteredROI(pnts, paths[1])
  
  n <- length(paths)
  CCCT <- as.data.table(matrix(0, nrow=n, ncol=3))
  colnames(CCCT) <- c('rcc','gcc','bcc')
  
  withProgress(value = 0, message = 'Extracting CCs',
  for(i in 1:n){
    ccc <- extractCCC(paths[i], rmsk = rmsk)
    CCCT[i,] <- as.data.table(ccc[c("rcc", "gcc", "bcc")])
    incProgress(1/n)
  }
  )
  list(TS = CCCT, mask= rmsk)
}


