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
bijanWD <- "/Users/bijan/Projects/drawROI"


fluidPage(
  shinyjs::useShinyjs(),  
  tabsetPanel(
    tabPanel('ROI Tool',
             
             headerPanel("PhenoCam ROI Tool"),
             sidebarPanel(width = 4,
                          fluidRow(
                            column(10, 
                                   selectInput("siteName", "Site", choices = if(getwd()==bijanWD)'acadia'else'ahwahnee')
                            ),
                            br(),
                            column(2, strong(actionButton('siteInfo', label = NULL, icon = icon('info'), width = '100%', style="border-color: #f5f5f5; align:center; background-color:#f5f5f5; color:#337ab7; font-size: 200%")),
                                   bsModal("modalSiteInfo", "Site Info", "siteInfo", 
                                           size = "medium",
                                           footer = NULL, 
                                           tableOutput("tblSiteInfo")
                                           )
                                   )
                          ),
                          selectInput("roiName", "ROI", 'New ROI'),
                          selectInput("vegType", "Vegetation Type", choices = list('Agriculture (AG)'='AG')),
                          textInput('siteDescription','Description', placeholder = 'Enter a description for the ROI'),
                          textInput('roiOwner','Owner', placeholder = 'Enter your name'),
                          hr(),
                          strong(textOutput('roiFileName')),
                          br(),
                          selectInput("maskName", label = 'Mask', choices = 'New mask'),
                          # textOutput('maskfilename'),
                          strong('Sample Image:'),
                          textOutput('sampleImagePath'),
                          br(),
                          fluidRow(
                            column(6, actionButton( 'matchStart', 'Match start', width = '100%', style='background-color:#666; color:#fff')),
                            column(6, actionButton( 'matchEnd', 'Match end', width = '100%', style='background-color:#666; color:#fff'))
                          ),
                          br(),
                          
                          fluidRow(
                            column(1, strong('from', style='font-size:70%')),
                            column(5, dateInput('maskStartDate', label = NULL, value =  '2001-01-01', startview = 'day')),
                            column(4, textInput('maskStartTime', label = NULL, value = '00:08:00')),
                            column(1, '')
                          ),
                          fluidRow(
                            column(1, strong('to', style='font-size:70%')),
                            column(5, dateInput('maskEndDate', label = NULL, value =  '2099-01-01', startview = 'day')),
                            column(4, textInput('maskEndTime', label = NULL, value = '00:20:00')),
                            column(1, checkboxInput('openEnd', label = '', value = F))
                          ),
                          
                          
                          br(),
                          fluidRow(
                            column(6, downloadButton("downloadROI", "Download ROI files")),
                            column(6, actionButton("emailROI", "Submit for review", icon = icon('send'), width = "100%"))
                          ),
                          br(),
                          br(),
                          passwordInput("password", label = NULL, placeholder = 'Password to save in the database'),
                          actionButton("saveROI", "Save ROI files in the database", icon = icon('list-alt'), width = "100%")
                          
             ),
             
             
             
             
             
             
             mainPanel(
               fluidRow( 
                 column(3, 
                        fluidRow( 
                          column(8, strong(dateInput('gotoDate', label = ''), style='font-size:40%')),
                          column(4, actionButton('gotoDateButton', label = NULL, icon = icon('refresh'), width = '100%', style="border-color: #fff; align:center; font-size: 200%"))
                        )
                 ),
                 column(9, sliderInput(inputId = "contID",
                                       label =  NULL,
                                       min = 1, max = 1,
                                       ticks = F,
                                       animate=F,
                                       value = 1,
                                       step = 1,
                                       width = '100%'))
               ),
               
               
               fluidRow(
                 column(1, strong()),
                 column(1, actionButton("back", "", icon = icon('minus'), width = '100%', style="border-color: #fff")),
                 column(1, actionButton("backplay", "", icon = icon('backward'), width = '100%', style="border-color: #fff; align:center")),
                 column(1, actionButton("pause", "", icon = icon('stop'), width = '100%',  style="border-color: #fff")),
                 column(1, actionButton("play", "", icon = icon('forward'), width = '100%', style="border-color: #fff; align:center")),
                 column(1, actionButton("forw", "", icon = icon('plus'), width = '100%',  style="border-color: #fff")),
                 column(5, selectInput('shiftsList', label = NULL, choices = 'List of shifts in FOV', width = '100%')),
                 column(1, strong())
                 
               ),
               fluidRow(
                 column(1, actionButton('previousSite', label = NULL, icon = icon('arrow-circle-left'), width = '100%',  style="border-color: #fff; font-size: 175%")),
                 column(5, plotOutput("imagePlot", click = "newPoint", dblclick = 'endPoint', width = "300px", height = '222px')),
                 column(5, plotOutput("maskPlot", width = "300px", height = '222px')),
                 column(1, actionButton('nextSite', label = NULL, icon = icon('arrow-circle-right'), width = '100%',  style="border-color: #fff; font-size: 175%"))
               ),
               br(),
               fluidRow(
                 column(1, strong()),
                 column(5, 
                        fluidRow(
                          column(4, actionButton("clearCanvas", "Erase", icon = icon('eraser'), class="btn-primary", width = "100%")),
                          column(4, actionButton("undoCanvas", "Undo", icon = icon('undo'), class="btn-primary", width = "100%")),
                          column(4, actionButton("acceptCanvas", "Accept", icon = icon('thumbs-up'), class="btn-primary", width = "100%"))
                        )),
                 column(5,
                        fluidRow(
                          column( 5, colourpicker::colourInput(inputId = 'roiColors', allowTransparent=T, transparentText = 'clear', label = NULL,value = '#ab5222', showColour = 'background')),
                          column( 7, p())
                        )
                 ),
                 column(1, strong())
               ),
               
               hr(),
               
               fluidRow(
                 column(3, radioButtons('ccRange', label = NULL, choices = c('week', 'year', 'all'), width = "330px",inline = T)),
                 column(3, actionButton("startExtractCC", "Extract time series", class="btn-primary", icon = icon('line-chart'), onclick="Shiny.onInputChange('stopThis',false)", width = "100%")),
                 column(3, checkboxGroupInput('ccBand', label = NULL, choices = c('R','G','B'), selected = c('R','G','B'), width = '100%', inline = T)),
                 column(3, downloadButton("downloadTSData", "Download"))
               ),
               
               plotlyOutput(outputId = "timeSeriesPlotly", height = "500px", width = "100%")
             )
    ),
    
    tabPanel('Report Errors', 
             fluidPage(
               headerPanel('Report an error'),
               br(),
               sidebarPanel(    
                 textInput('errorUser', label = 'Your contact info', placeholder = 'Name', width = '100%'),
                 textInput('errorEmail', label = NULL, placeholder = 'Email', width = '100%'),
                 selectInput('errorSite', label = 'Site', choices = ''),
                 selectizeInput('errorOS', label = 'System info', choices = c('Windows','Linux','MacOS', 'Android', 'iOS')),
                 selectizeInput('errorBrowser', label = NULL, choices = c('Firefox', 'Google Chrome', 'Internet Explorer', 'Opera','Safari','Others')),
                 dateInput("errorDate", label = "Error date/time", value = Sys.Date()),
                 textInput("errorTime", label = NULL, value = strftime(Sys.time(), format = '%H:%M:%S'), placeholder = 'What time did the error occure?'),
                 selectInput('errorType', label = 'It crashed?', choices = c('Yes, it crashed.', 
                                                                             'No, it just returned an error message.',
                                                                             'No, but the output does not make sense.',
                                                                             'No, I have some suggestions.'), selected = ''),
                 actionButton("errorSend", "Submit", width = '100%', icon = icon('send'), class="btn-primary")
               ),
               
               mainPanel( 
                 textAreaInput('errorMessage', label = 'Explain the error please.', cols = 200, rows = 20) 
               )               
             )
    )
  )
)



