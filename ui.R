library(shiny)
library(shinyTime)
library(shinyjs)
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


fluidPage(
  shinyjs::useShinyjs(),  
  tabsetPanel(
    tabPanel('ROI Tool',
             headerPanel("PhenoCam ROI Tool"),
             sidebarPanel(width = 4,
                          # XXX
                          # selectInput("site", "Site", choices = 'dukehw'),
                          selectInput("site", "Site", choices = 'acadia'),
                          selectInput("rois", "ROIs", 'New ROI'),
                          selectInput("vegtype", "Vegetation Type", choices = ''),
                          textInput('descr','Description', placeholder = 'Enter a description for the ROI'),
                          textInput('owner','Owner', placeholder = 'Enter your name'),
                          hr(),
                          
                          strong('ROI List filename:'),
                          textOutput('roilabel'),
                          br(),
                          selectInput("masks", "Mask", choices = NULL),
                          strong('Sample Image:'),
                          textOutput('sampleImagePath'),
                          br(),
                          
                          dateRangeInput(inputId = 'roiDateRange', label = 'ROI Start/End Dates:', start = '2001-01-01', end = '2016-01-01', separator = '-', startview='day'),
                          fluidRow(
                            column(width = 6, textInput('starttime', label = 'Start Time:', width = '80px', value = '00:08:00')),
                            column(width = 6, textInput('endtime', label = 'End Time:', width = '80px', value = '00:20:00'))
                          ),
                          
                          br(),
                          passwordInput("password", label = NULL, placeholder = 'Password to generate ROI files'),
                          actionButton("generate", "Generate ROI List", icon = icon('list-alt'), width = "100%")
             ),
             
             mainPanel(
               sliderInput(inputId = "contID",
                           label =  NULL,
                           min = 1, max = 1,
                           ticks = F,
                           animate=F,
                           value = 1,
                           step = 1,
                           width = '100%'),
               
               fluidRow(
                 column(2, selectInput("year", "Year", '', width = '80px')),
                 br(),
                 column(10, sliderInput(inputId = "viewDay", label =  NULL,
                                        min = 1, max = 365, ticks = F, animate=F,
                                        value = 1, round = T, step = 1, width = '700px'))
               ),
               
               fluidRow(
                 column(1, actionButton("backplay", "", icon = icon('step-backward'), width = '100%', style="border-color: #fff; align:center")),
                 column(1, actionButton("back", "", icon = icon('backward'), width = '100%', style="border-color: #fff")),
                 column(1, actionButton("pause", "", icon = icon('pause'), width = '100%',  style="border-color: #fff")),
                 column(1, actionButton("forw", "", icon = icon('forward'), width = '100%',  style="border-color: #fff")),
                 column(1, actionButton("play", "", icon = icon('step-forward'), width = '100%', style="border-color: #fff; align:center"))
               ),
               
               fluidRow(
                 column(6, plotOutput("plot", click = "newPoint", width = "350px", height = '260px')),
                 column(6, plotOutput("maskplot", width = "350px", height = '260px'))
               ),
               
               fluidRow(
                 br(),
                 
                 column(6,
                        column( 4, colourpicker::colourInput(inputId = 'roicol', allowTransparent=T, transparentText = 'none', label = NULL,value = '#ab5222', showColour = 'background')),
                        
                        column( 5, selectInput('shiftsList', label = NULL, choices = '', width = '100%')),
                        column( 3, actionButton( 'gotoShiftFOV', label = 'Go', width = '100%', class="btn-success"))
                 ),
                 
                 column(6, actionButton("cancel", "Clear", icon = icon('refresh'), class="btn-primary", width = "85px"),
                        actionButton("undo", "Undo", icon = icon('undo'), class="btn-primary", width = "85px"),
                        actionButton("save", "Edit", icon = icon('edit'), class="btn-danger", width = "85px"),
                        actionButton("accept", "Add", icon = icon('save'), class="btn-danger", width = "85px"))
               ),
               
               hr(),
               
               fluidRow(
                 column(3, radioButtons('sevenorall', label = 'Time series range:', choices = c('week', 'year', 'all'), width = "330px",inline = T)),
                 br(),
                 
                 column(2, actionButton("extract", "Extract", class="btn-primary", icon = icon('line-chart'), onclick="Shiny.onInputChange('stopThis',false)", width = "100%")),
                 column(2, actionButton("stopExtract", "Stop", class="btn-danger", icon = icon('stop'), onclick="Shiny.onInputChange('stopThis',true)", width = "100%")),
                 column(3, checkboxGroupInput('ccselect', label = NULL, choices = c('R','G','B'), selected = c('R','G','B'), width = '100%', inline = T)),
                 column(2, downloadButton("downloadTSData", "Download"))
               ),
               
               plotlyOutput(outputId = "timeSeriesPlotly", height = "300px", width = "100%")
             )
    ),
    
    tabPanel('Report Errors', 
             fluidPage(
               headerPanel('Report an error'),
               br(),
               sidebarPanel(    
                 textInput('errorUser', label = 'Your info:', placeholder = 'Please enter your name (optional).', width = '100%'),
                 textInput('errorEmail', label = NULL, placeholder = 'Please enter your email address (optional).', width = '100%'),
                 selectizeInput('errorOS', label = 'System info', choices = c('Windows','Linux','MacOS', 'Android', 'iOS')),
                 selectizeInput('errorBrowser', label = NULL, choices = c('Firefox', 'Google Chrome', 'Internet Explorer', 'Opera','Safari','Others')),
                 dateInput("errorDate", label = "Error date/time:", value = Sys.Date()),
                 textInput("errorTime", label = NULL, value = strftime(Sys.time(), format = '%H:%M:%S'), placeholder = 'What time did the error occure?'),
                 selectInput('errorType', label = 'It crashed?', choices = c('Yes, it crashed.', 
                                                                             'No, it just returned an error message.',
                                                                             'No, but the outpu does not make sense.'), selected = ''),
                 actionButton("errorSend", "Submit", width = '100%', icon = icon('send'), class="btn-primary")
               ),
               
               mainPanel( 
                 # tags$textarea(id="errorMessage", rows=10, cols=40, "Explain the error please.")
                 textAreaInput('errorMessage', label = 'Explain the error please.', cols = 200, rows = 20) 
                 # aceEditor("errorMessage", value="Explain the error please.", mode = 'text', theme = 'eclipse')
               )               
             )
    )
  )
)



