library(data.table)
library(lubridate)
source('drawROI.Funcs.R')

# library(rjson)
# phenoSites <- fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/')
# # phenoSites
# phenoROIs <- fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/roi/roilistinfo/')
# # phenoROIs
# unique(sapply(phenoROIs, function(x){x$site}))
# sapply(phenoSites, function(x){x$site})
# phenoSiteGeos <- fromJSON(file = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/?format=geojson')

dataPath <- 'phenocamdata'

imgDT <- data.table(path = dir(dataPath, recursive = T, full.names = T, pattern = '*.jpg'),
                       tmp =  dir(dataPath, recursive = T, full.names = F, pattern = '*.jpg'))
imgDT$tmp <- unlist(lapply(imgDT$tmp, function(x){strsplit(x,split = '/', fixed = T)[[1]][4]}))
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
sites <- unique(imgDT$Site)


