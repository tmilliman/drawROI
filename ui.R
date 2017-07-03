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


fluidPage(
  shinyjs::useShinyjs(),  
  tabsetPanel(
    tabPanel('ROI Tool',
             
             headerPanel("PhenoCam ROI Tool"),
             sidebarPanel(width = 4,
                          selectInput("siteName", "Site", choices = 'acadia'),
                          selectInput("roiName", "ROIs", 'New ROI'),
                          selectInput("vegType", "Vegetation Type", choices = ''),
                          textInput('siteDescription','Description', placeholder = 'Enter a description for the ROI'),
                          textInput('roiOwner','Owner', placeholder = 'Enter your name'),
                          hr(),
                          strong(textOutput('roiFileName')),
                          selectInput("maskName", "Mask", choices = 'New mask'),
                          # textOutput('maskfilename'),
                          strong('Sample Image:'),
                          textOutput('sampleImagePath'),
                          br(),
                          fluidRow(
                            column(6, actionButton( 'matchStart', 'Match start', width = '100%', style='background-color:#666; color:#fff')),
                            column(6, actionButton( 'matchEnd', 'Match end', width = '100%', style='background-color:#666; color:#fff'))
                          ),
                          br(),
                          dateRangeInput(inputId = 'roiDateRange', label = 'ROI Start/End Dates:', start = '2001-01-01', end = '2016-01-01', separator = '-', startview='day'),
                          fluidRow(
                            column(width = 6, textInput('maskStartTime', label = 'Start Time:', width = '80px', value = '00:08:00')),
                            column(width = 6, textInput('maskEndTime', label = 'End Time:', width = '80px', value = '00:20:00'))
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
                 column(2, actionButton('nextSite', label = 'Next Site', width = '80px',class="btn-primary")),
                 column(10, sliderInput(inputId = "contID",
                                        label =  NULL,
                                        min = 1, max = 1,
                                        ticks = F,
                                        animate=F,
                                        value = 1,
                                        step = 1,
                                        width = '100%'))),
               
               fluidRow(
                 column(2, selectInput("year", "Year", '', width = '80px')),
                 br(),
                 column(10, sliderInput(inputId = "viewDay", label =  NULL,
                                        min = 1, max = 365, ticks = F, animate=F,
                                        value = 1, round = T, step = 1, width = '700px'))
               ),
               
               fluidRow(
                 column(1, actionButton('siteInfo', label = NULL, icon = icon('info'), width = '100%', style="border-color: #fff; align:center; color:#FF0000"),
                        bsModal("modalSiteInfo", "Site Info", "siteInfo", size = "large",footer = NULL, 
                                tableOutput("tblSiteInfo"))
                 ),
                 column(1, actionButton("backplay", "", icon = icon('step-backward'), width = '100%', style="border-color: #fff; align:center")),
                 column(1, actionButton("back", "", icon = icon('backward'), width = '100%', style="border-color: #fff")),
                 column(1, actionButton("pause", "", icon = icon('pause'), width = '100%',  style="border-color: #fff")),
                 column(1, actionButton("forw", "", icon = icon('forward'), width = '100%',  style="border-color: #fff")),
                 column(1, actionButton("play", "", icon = icon('step-forward'), width = '100%', style="border-color: #fff; align:center"))
               ),
               
               fluidRow(
                 column(6, plotOutput("imagePlot", click = "newPoint", width = "350px", height = '260px')),
                 column(6, plotOutput("maskPlot", width = "350px", height = '260px'))
               ),
               br(),
               
               fluidRow(
                 
                 column(6,
                        column( 4, colourpicker::colourInput(inputId = 'roiColors', allowTransparent=T, transparentText = 'clear', label = NULL,value = '#ab5222', showColour = 'background')),
                        
                        column( 8, selectInput('shiftsList', label = NULL, choices = 'Possible shifts in FOV', width = '100%'))
                        # column( 2, actionButton( 'gotoShiftFOV', label = 'Go', width = '100%', class="btn-success"))
                 ),
                 
                 column(6, actionButton("clearCanvas", "Clear", icon = icon('refresh'), class="btn-primary", width = "113px"),
                        actionButton("undoCanvas", "Undo", icon = icon('undo'), class="btn-primary", width = "113px"),
                        # actionButton("save", "Edit", icon = icon('edit'), class="btn-danger", width = "85px"),
                        # actionButton("accept", "Add", icon = icon('save'), class="btn-danger", width = "85px"),
                        actionButton("acceptCanvas", "Save", icon = icon('save'), class="btn-danger", width = "113px"))
               ),
               
               hr(),
               
               fluidRow(
                 column(3, radioButtons('ccRange', label = 'Time series range:', choices = c('week', 'year', 'all'), width = "330px",inline = T)),
                 br(),
                 
                 column(2, actionButton("startExtractCC", "Extract", class="btn-primary", icon = icon('line-chart'), onclick="Shiny.onInputChange('stopThis',false)", width = "100%")),
                 column(2, actionButton("stopExtractCC", "Stop", class="btn-danger", icon = icon('stop'), onclick="Shiny.onInputChange('stopThis',true)", width = "100%")),
                 column(3, checkboxGroupInput('ccBand', label = NULL, choices = c('R','G','B'), selected = c('R','G','B'), width = '100%', inline = T)),
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
                 # tags$textarea(id="errorMessage", rows=10, cols=40, "Explain the error please.")
                 textAreaInput('errorMessage', label = 'Explain the error please.', cols = 200, rows = 20) 
                 # aceEditor("errorMessage", value="Explain the error please.", mode = 'text', theme = 'eclipse')
               )               
             )
    )
  )
)



