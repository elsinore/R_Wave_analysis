#### required packages and functions ####
library(shiny)
library(ggplot2)
library(biwavelet)
library(data.table)
library(stringr)
library(ape)
library(DT)
library(shinyFiles)
library(shinyjs)
source("functions.R")
  ###=== end of packages and functions loading ===###
#### UI Function ####
navbarPage("Wave Analysis",
  #### Single Sample ####
  tabPanel("Single Sample Explore",
           fluidPage(
             useShinyjs(),
             #### sidbar input and layout ####
             sidebarLayout(
               sidebarPanel(
                 #### 01.data View ####
                 conditionalPanel(
                   'input.dataset === "Data View"',
                   h4("Data Overview"),
                   # file input 
                   fileInput('datafile', 'Choose CSV file', accept=c('csv', 'comma-separated-values','.csv')),
                   checkboxInput('header', 'Header', TRUE),
                   radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
                   radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), ''),
                   withBusyIndicatorUI(
                     actionButton(
                       "analyze",
                       "Analyze",
                       class = "btn-primary"
                     )
                   ),
                   #= end of file inpur
                   # select column
                   checkboxInput('selall01', 'Select All Cells', TRUE),
                   selectizeInput("check01", "Select Cells", NULL, multiple = TRUE)
                   #= end of select column
                 ),
                 #=== end.01 ===#
                 
                 #### 02.wave features ####
                 conditionalPanel(
                   'input.dataset === "Wave Features"',
                   h4("Wave Features"),
                   fluidRow(
                     column(6,radioButtons("xaxisGrp","Y-Axis:", c("NULL"="NULL"))),
                     column(6,radioButtons("yaxisGrp","X-axis:", c("NULL"="NULL")))
                   ),
                   checkboxInput('resSlect', 'Select All Cells', TRUE),
                   selectizeInput('c2', 'Select Cells', choices = NULL, multiple = TRUE),
                   checkboxGroupInput('c2.2', 'Select features', choices = NULL),
                   withMathJax.local(
                     helpText("Details of each feature:"),
                     helpText("Here \\(N\\) denotes the length of the signal and \\(x_n\\) represents the wave in a segment"),
                     helpText("01. Integrated (Int): $$Int = \\sum_{n=1}^N |x_n|$$"),
                     tags$hr(),
                     helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N w_n|x_n|$$"),
                     tags$hr(),
                     helpText("03. Variance (VAR): $$VAR = \\frac{1}{N-1} \\sum_{n=1}^N x_n^2 $$"),
                     tags$hr(),
                     helpText("04. Root Mean Square (RMS): $$RMS = \\sqrt{\\frac{1}{N}\\sum_{n=1}^N x_n^2}$$"),
                     tags$hr(),
                     helpText("05. Waveform Length (WL): $$WL = sum_{n=1}^N \\vert x_n+1 - x_n \\vert$$"),
                     tags$hr(),
                     helpText(HTML("06. Main Period (MP):  the period obtained by the <b>Maximal Wavelet Variance </b>")),
                     tags$hr(),
                     helpText("07. Maximal Amplitude (MA): $$\\max x_n - \\min x_n$$"),
                     tags$hr(),
                     helpText("08. Mean Power Frequency (MPF): 
                              $$MPF = \\frac{\\sum_{n=1}^N x_n P_n}{\\sum_{n=1}^N P_n}$$ Here \\(P\\) represents the power spectrum at the frequency segment \\(n\\)")
                   )
                 ),
                 #=== end.02 ===#
                 
                 #### 03.graphic results ####
                 conditionalPanel(
                   'input.dataset === "Graphic View"',
                   h4("Single Data Overview"),
                   selectInput("sel03", label = "Select a Single Cell", choices = NULL, multiple = FALSE),
                   tags$hr(),
                   withMathJax(
                     helpText("Details of each feature:"),
                     helpText("Here \\(N\\) denotes the length of the signal and \\(x_n\\) represents the wave in a segment"),
                     helpText("01. Integrated (Int): $$Int = \\sum_{n=1}^N |x_n|$$"),
                     tags$hr(),
                     helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N w_n|x_n|$$"),
                     tags$hr(),
                     helpText("03. Variance (VAR): $$VAR = \\frac{1}{N-1} \\sum_{n=1}^N x_n^2 $$"),
                     tags$hr(),
                     helpText("04. Root Mean Square (RMS): $$RMS = \\sqrt{\\frac{1}{N}\\sum_{n=1}^N x_n^2}$$"),
                     tags$hr(),
                     helpText("05. Waveform Length (WL): $$WL = sum_{n=1}^N \\vert x_n+1 - x_n \\vert$$"),
                     tags$hr(),
                     helpText(HTML("06. Main Period (MP):  the period obtained by the <b>Maximal Wavelet Variance </b>")),
                     tags$hr(),
                     helpText("07. Maximal Amplitude (MA): $$\\max x_n - \\min x_n$$"),
                     tags$hr(),
                     helpText("08. Mean Power Frequency (MPF): $$MPF = \\frac{\\sum_{n=1}^N x_n P_n}{\\sum_{n=1}^N P_n}$$ Here \\(P\\) represents the power spectrum at the frequency segment \\(n\\)")
                   )
                 ),
                 #=== end.03 ===#
                 
                 #### 04.spatial features ####
                 conditionalPanel(
                   'input.dataset === "Spatial Features"',
                   h4("Spatial Features"),
                   fluidRow(
                     column(6,
                            fileInput('dataSpa04', 'Choose Spatial file', accept=c('csv', 'comma-separated-values','.csv')),
                            checkboxInput('header04', 'Header', TRUE),
                            radioButtons('sep04', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
                            radioButtons('quote04', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '')
                     ),
                     column(6,
                            fileInput('dataLoc04', 'Choose Location file', accept=c('csv', 'comma-separated-values','.csv')),
                            checkboxInput('header04.01', 'Header', TRUE),
                            radioButtons('sep04.01', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
                            radioButtons('quote04.01', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '')
                     )
                   ),
                   tags$hr(),
                   fluidRow(
                     column(
                       6, checkboxInput("label04", "Label", TRUE),
                       withMathJax(
                         helpText("Details of each feature:"),
                         helpText("Here \\(N\\) denotes the length of the signal and \\(x_n\\) represents the wave in a segment"),
                         helpText("01. Integrated (Int): $$Int = \\sum_{n=1}^N |x_n|$$"),
                         tags$hr(),
                         helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N w_n|x_n|$$"),
                         tags$hr(),
                         helpText("03. Variance (VAR): $$VAR = \\frac{1}{N-1} \\sum_{n=1}^N x_n^2 $$"),
                         tags$hr(),
                         helpText("04. Root Mean Square (RMS): $$RMS = \\sqrt{\\frac{1}{N}\\sum_{n=1}^N x_n^2}$$"),
                         tags$hr(),
                         helpText("05. Waveform Length (WL): $$WL = sum_{n=1}^N \\vert x_n+1 - x_n \\vert$$"),
                         tags$hr(),
                         helpText(HTML("06. Main Period (MP):  the period obtained by the <b>Maximal Wavelet Variance </b>")),
                         tags$hr(),
                         helpText("07. Maximal Amplitude (MA): $$\\max x_n - \\min x_n$$"),
                         tags$hr(),
                         helpText("08. Mean Power Frequency (MPF): $$MPF = \\frac{\\sum_{n=1}^N x_n P_n}{\\sum_{n=1}^N P_n}$$ Here \\(P\\) represents the power spectrum at the frequency segment \\(n\\)")
                       )
                     ),
                     column(6, radioButtons("col04","Choose Method", c("NULL"="NULL")))
                   )
                   
                 )
                 #=== end.03 ===#
                 
               ),
               ###=== end of sidbar input and layout ===###
               
               #### the result windows ####
               mainPanel(
                 tabsetPanel(
                   id = 'dataset',
                   #### 01.data View ####
                   tabPanel("Data View", 
                            verticalLayout(
                              plotOutput("plot"),
                              tags$hr(),
                              plotOutput("plot01.02"),
                              tags$hr(),
                              tableOutput("table")
                            )
                   ),
                   #=== end.01 ===#
                   
                   #### 02.basic results ####
                   tabPanel("Wave Features", 
                            verticalLayout(
                              plotOutput("plot02.01"),
                              tags$hr(),
                              splitLayout(
                                plotOutput("plot02.02"),
                                plotOutput("plot02.03")
                              ),
                              tags$hr(),
                              tags$hr(),
                              DT::dataTableOutput('myresults')
                            )
                   ),
                   #=== end.02 ===#
                   
                   #### 03.graphic results ####
                   tabPanel("Graphic View",
                            verticalLayout(
                              tableOutput("table03.01"),
                              splitLayout(
                                plotOutput("plot03.02"),
                                plotOutput("plot03.01")
                              ),
                              splitLayout(
                                plotOutput("plot03.03"),
                                plotOutput("plot03.04")
                              )
                            )
                   ),
                   #===end.03 ===#
                   
                   #### 04.spatial features ####
                   tabPanel("Spatial Features",
                            splitLayout(
                              plotOutput("plot04.01"),
                              plotOutput("plot04.02")
                            ),
                            tableOutput("table04.01")
                   )
                   #=== end.04 ===#
                 )
               )
               ###=== result windows end ===###
             )
           )
  ),
  #### Batching Processing ####
  tabPanel("Batching Processing",
           #### sidbar input and layout ####
           sidebarPanel(
             #### 01. Data Input ####
             conditionalPanel(
               'input.dataset2 === "Data Input"',
               h4("Data Input"),
               verticalLayout(
                 textInput("postfix", "File format", ".csv"),
                 shinyDirButton('directory', 'Folder select', 'Please select a folder'),
                 textInput("pat01", "Prefix mark", "TIF"),
                 textInput("pat01_01", "Image file number", "00"),
                 textInput("pat02", "Location file number", "02"),
                 withBusyIndicatorUI(
                   actionButton(
                     "anB01",
                     "Analyze",
                     class = "btn-primary"
                   )
                 ),
                 tags$hr(),
                 fileInput('tableB01.00', 'Choose CSV file for table 1', accept=c('csv', '.csv')),
                 fileInput('tableB01.01', 'Choose CSV file for table 2', accept=c('csv', '.csv')),
                 fileInput('tableB01.02', 'Choose CSV file for table 3', accept=c('csv', '.csv')),
                 fileInput('tableB01.03', 'Choose CSV file for table 4', accept=c('csv', '.csv'))
               )
             ),
             #### 02. Statistical Analysis ####
             conditionalPanel(
               'input.dataset2 == "Statistical Analysis"',
               h4("Statistical Analysis"),
                 numericInput("GroupMarkB02", "Select Group and Set Number", value = 1, min = 0),
                 actionButton("AssigB02", "Assign"),
                 actionButton("updateB02", "Initialize"),
               tags$hr(),
                 withBusyIndicatorUI(
                   actionButton(
                     "staB02",
                     "Statistical Analysis",
                     class = "btn-primary"
                   )
                 )
             )
             ),
           #### The Result Window ####
           mainPanel(
             tabsetPanel(
               id = 'dataset2',
               #### 01.Data Input ####
               tabPanel("Data Input",
                        verticalLayout(
                          DT::dataTableOutput("tableB01.00"),
                          tags$hr(),
                          DT::dataTableOutput('tableB01.01'),
                          tags$hr(),
                          DT::dataTableOutput('tableB01.02'),
                          tags$hr(),
                          DT::dataTableOutput('tableB01.03')
                        )
               ),
               tabPanel("Statistical Analysis",
                        verticalLayout(
                          tags$hr("Group Setting"),
                          DT::dataTableOutput('tableB02.00')
                        )

               )
             )
           )
  )
  
)
