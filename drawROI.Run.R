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
source('drawROI.Server.R')


shinyApp(ui, server)
