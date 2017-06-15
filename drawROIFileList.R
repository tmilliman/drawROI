library(data.table)
library(rjson)
library(lubridate)

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


download.file('https://phenocam.sr.unh.edu/webcam/network/siteinfo/', '/tmp/~siteinfo.json', method='curl', quiet = T)
phenoSites <- fromJSON(file = '/tmp/~siteinfo.json')
file.remove('/tmp/~siteinfo.json')

for (i in 1:length(phenoSites)){
  
  siteName <- phenoSites[[i]]$site
  print(siteName)
  
  siteDir <- paste0('/data/archive/', siteName)
  if(grepl('NEON', siteName)) siteDir <- paste0(siteDir, '/originals')
  filenames <- dir(siteDir, recursive = T, full.names = F, pattern = '*.jpg')
  wNotToday <- grepl('/',filenames, fixed = T)
  filenames <- filenames[wNotToday]
  wIR <- grepl('_IR_',filenames, fixed = T)
  filenames <- filenames[!wIR]
  
  wError <- grepl('.jpg.',filenames, fixed = T)
  filenames <- filenames[!wError]
  imgDT <- filePathParse(filenames)

    write.table(imgDT[Hour==12&Minute<30,filenames], 
              file = paste0('/home/bijan/middaylist/', siteName, '.txt'), 
              row.names = F, col.names = F, quote = F)
}


