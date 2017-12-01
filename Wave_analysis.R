# +++00. Required Packages and Functions -----------------------------------------
list.of.packages <- c("shiny", "ggplot2", "biwavelet", "data.table", "stringr", "ape", "DT", "shinyFiles", "shinyjs", "psych", "ggsignif", "grid", "dunn.test")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(shiny)
library(ggplot2)
library(biwavelet)
library(data.table)
library(stringr)
library(ape)
library(DT)
library(shinyFiles)
library(shinyjs)
library(psych)
library(ggsignif)
library(grid)
library(dunn.test)
source("functions.R")
  ###=== end of packages and functions loading ===###
# +++01. UI Function -------------------------------------------------------------
ui<-navbarPage("Wave Analysis",
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
                            verticalLayout(
                              fileInput('datafile', 'Choose CSV file', accept=c('csv', 'comma-separated-values','.csv')),
                              splitLayout(
                                column(12, 
                                       checkboxInput("calibration01", "Calibrated by Mean", TRUE),
                                       checkboxInput("colname01", "Change column name", TRUE),
                                       textInput("FirstCol01", "First column name", "Time"),
                                       textInput("SecondCol01", "Second column name", "Region"),
                                       textInput("ThirdCol01", "Rest column names", "Cell")
                                ),
                                column(12,
                                       checkboxInput('header', 'Header', TRUE),
                                       radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
                                       radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '')
                                )
                              ),
                              tags$hr(),
                              withBusyIndicatorUI(
                                actionButton(
                                  "analyze",
                                  "Analyze",
                                  class = "btn-primary"
                                )
                              ),
                              #= end of file inpur
                              # select column
                              splitLayout(
                                checkboxInput('selall01', 'Select All Cells', TRUE),
                                radioButtons('checkGroup01', 'Select Group', c('All Groups' = 0, 'Group 1' = 1, 'Group 2' = 2))
                              ),
                              selectizeInput("check01", "Select Cells", NULL, multiple = TRUE)
                            )
                            #= end of select column
                          ),
                          #=== end.01 ===#
                          #### 02.wave features ####
                          conditionalPanel(
                            'input.dataset === "Wave Features"',
                            h4("Wave Features"),
                            fluidRow(
                              downloadButton('downloadAnaRes02', 'Download the wave feature result'),
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
                              helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N |x_n|$$"),
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
                              helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N |x_n|$$"),
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
                                  helpText("02. Mean Absolute Value (MAV): $$MAV = \\frac{1}{N} \\sum_{n=1}^N |x_n|$$"),
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
                                       plotOutput("plot01.03"),
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
                                       ),
                                       plotOutput("plot03.05")
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
                        textInput("postfix", "File format", ".csv"),
                        splitLayout(
                          shinyDirButton('directory', 'Folder select', 'Please select a folder'),
                          checkboxInput("calibrationB01", "Calibrated by Mean", TRUE)
                        ),
                        textInput("pat01", "Prefix mark", "TIF"),
                        textInput("pat01_01", "Image file number", "00"),
                        textInput("pat02", "Location file number", "02.Location"),
                        textInput("FirstColB01", "First column Name", "Time (s)"),
                        textInput("SecondColB01", "Second column Name", "Region"),
                        textInput("ThirdColB01", "Rest column Names", "Cell"),
                        withBusyIndicatorUI(
                          actionButton(
                            "anB01",
                            "Analyze",
                            class = "btn-primary"
                          )
                        ),
                        tags$hr(),
                        verticalLayout(
                          shinyDirButton('uploadAnaResB01', 'Upload a analysis result', 'Please select a folder'),
                          downloadButton('downloadAnaResB01', 'Download the analysis result'),
                          helpText(HTML('<p align="justify">Please download those files <b>immediately</b> after hitting the "<b>Analyze</b>" botton</p>'))
                        )
                      ),
                      #### 02. Statistical Analysis ####
                      conditionalPanel(
                        'input.dataset2 == "Statistical Analysis"',
                        h4("Statistical Analysis"),
                          splitLayout(
                            numericInput("GroupMarkB02", "Select rows and Number", value = 0, min = 0),
                            textInput("LabelB02", "Set Group Name", placeholder = "Type a group label")
                          ),
                        actionButton("AssigB02", "Assign"),
                        actionButton("updateB02", "Initialize"),
                        downloadButton('downloadGroupSet', 'Download Group Setting'),
                        tags$hr(),
                        fileInput('GroupSetB02', 'Upload Group Set', accept=c('csv', '.csv')),
                        tags$hr(),
                        withBusyIndicatorUI(
                          actionButton(
                            "staB02",
                            "Statistical Analysis",
                            class = "btn-primary"
                          )
                        )
                      ),
                      #### 03. Results ####
                      conditionalPanel(
                        'input.dataset2 == "Results"',
                        h4("Statistical Results"),
                        downloadButton('downloadStaResB03', 'Download the statistic result')
                      ),
                      #### 04. Plot Output ####
                      conditionalPanel(
                        'input.dataset2 == "Plot Output"',
                        h4("Plot Output"),
                        checkboxInput("ViolinB04", "Create histogram and mean value", TRUE),
                        selectInput("levelB04", "Select the view level", 
                          c("Cell Level", "Region Level", "Moran Index", "Significance of Moran Index", "Comparsion among Groups", "Histogram")
                        ),
                        uiOutput("uiB04.side")
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
                        #### 02.Statistical Analysis ####
                        tabPanel("Statistical Analysis",
                                 verticalLayout(
                                   DT::dataTableOutput('tableB02.00'),
                                   DT::dataTableOutput('tableRegionB02'),
                                   DT::dataTableOutput('tableWavefeatureB02'),
                                   DT::dataTableOutput('tableDistributionB02'),
                                   DT::dataTableOutput('tableMoranPB02')
                                 )
                        ),
                        #### 03.Results ####
                        tabPanel("Results",
                          DT::dataTableOutput('tableWaveB03'),
                          DT::dataTableOutput('tableRegionB03'),
                          DT::dataTableOutput('tableMoranIndexB03'),
                          DT::dataTableOutput('tableMoranPB03'),
                          DT::dataTableOutput('tableGroupComparB03')
                        ),
                        #### 04.Plot Output ####
                        tabPanel("Plot Output",
                          uiOutput("uiB04")
                        )
                      )
                    )
           )
)
# +++02. Service Function --------------------------------------------------------
server<-function(input, output, session) {
  # Global variables ####
  volumes <- c('root'=path.expand('~'))
  shinyDirChoose(input, 'directory', roots=volumes, session = session)
  shinyDirChoose(input, 'uploadAnaResB01', roots=volumes, session = session)
  shinyDirChoose(input, 'ChooseDirB03', roots = volumes, session = session)
  shinyDirChoose(input, 'Select02', roots = volumes, session = session)
  values<-reactiveValues()
  values$colnames<-NULL
  values$sampleSize <- NULL
  values$Row_names <- NULL
  values$rawCells <- NULL
  values$rawRegion <- NULL
###=== Single Sample Explore ===####
  #### 00.data manipulation ####  
  dataframe<-eventReactive(input$analyze, {
    if (is.null(input$datafile))
      return(NULL)                
    data<-read.csv(input$datafile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    ncol<-NCOL(data)
    label<-c()
    if(input$calibration01 == TRUE){
      for(l in 2:ncol){
        data[, l] <- data[, l]-mean(data[, l])
      }
    }
    if(input$colname01 == TRUE){
      for(i in 1:ncol){
        if(i == 1){
          label[1] <- input$FirstCol01
        } else if (i == 2){
          label[i] <- input$SecondCol01
        } else {
          label[i] <- str_c(input$ThirdCol01," ", i-2)
        }
      }
      colnames(data)<-label
    }
    data
  })
  waveClust<-eventReactive(input$analyze, {
    if (is.null(input$datafile))
      return(NULL)    
    similarity<-dataframe()
    withBusyIndicatorServer("analyze", {
      similarity<-WZY.Wavelet.clust(similarity)
    })
    similarity
  })
  res<-eventReactive(input$analyze, {
    if (is.null(input$datafile))
      return(NULL)
    resin<-dataframe()
    ## Basic feasture
    resin<-WZY.EMG.F.MPDF(resin)
    resin <- resin$results
    ## Wavelet Cluster
    resclust<-waveClust()
    resclu<-as.matrix(resclust)
    resclu<-resclu[,1] #the dissimilarity of each cell to region
    fit <- hclust(resclust, method = "ward.D")
    groups <- cutree(fit, k = 2)
    ## Final combination
    resin <- cbind(resin, Dissimilarity = resclu, Group = groups)
    resin
  })
  dataSpa<-reactive({
    if (is.null(input$dataSpa04))
      return(NULL) 
    else
    data04<-read.csv(input$dataSpa04$datapath, header=input$header04, sep=input$sep04, quote=input$quote04)
    return(data04)
  })
  dataLoc<-reactive({
    if (is.null(input$dataLoc04))
      return(NULL) 
    else
      data04<-read.csv(input$dataLoc04$datapath, header=input$header04.01, sep=input$sep04.01, quote=input$quote04.01)
    return(data04)
  })
    ###=== 00.end ===###
  #### 01.data View ####
  #=== manipulation part ===#
  plot1<-reactive({ 
    df <- dataframe()
    gp <- NULL
    wzy<-colnames(df)
    if (!is.null(df)){
      xv <- wzy[1]
      yv <- input$check01
      if (!is.null(xv) & !is.null(yv)){
        if (sum(xv %in% names(df))>0){ # supress error when changing files
          mdf <- melt(df,id.vars=xv,measure.vars=yv)
          gp <- ggplot(data=mdf) + 
            geom_line(aes_string(x=xv,y="value",color="variable")) +
            labs(x = input$FirstColB01, y = "Fouresence Intensity (Gray Value)") +
            theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))
        }
      }
    }
    gp
  })# Line plot
  plot01.02<-reactive({
    df<-waveClust()
    gp<-NULL
    if(!is.null(df)){
      gp<-plot(hclust(df, method = "ward.D"), sub = " ", main = "Wave Cluster", ylab = "Dissimilarity to reference", xlab = "", hang = -1)
    }
    gp
  })# wave clust
  plot01.03<-reactive({
    df<-dataframe()
    df<-df[, -2]
    res<-res()
    res<-res[-1, ]
    G1 <- as.vector(rownames(res[res$Group == 1, ]))
    G2 <- as.vector(rownames(res[res$Group == 2, ]))
    Group1<- rowMeans(df[, G1])
    Group2<- rowMeans(df[, G2])
    SD1 <- SD(t(df)[G1,])/sqrt(length(rownames(res[res$Group == 1, ])))
    SD2 <- SD(t(df)[G2,])/sqrt(length(rownames(res[res$Group == 2, ])))
    SD <- c(SD1, SD2)
    Time <- df[, 1]
    data03 <- data.frame(Time = Time, Group1 = Group1, Group2 = Group2)
    mdf03 <- melt(data03, id.vars = "Time", measure.vars = colnames(data03[,-1]))
    gp03<-ggplot(data = mdf03, aes_string(x="Time", y = "value", color = "variable")) + geom_line() + 
      geom_errorbar(aes(ymin=mdf03$value - SD, ymax=mdf03$value + SD), position = position_dodge(0.9))
    gp03
  })
  #=== output part ===#
  output$plot <-renderPlot(plot1())
  output$plot01.02 <-renderPlot(plot01.02())
  output$plot01.03 <-renderPlot(plot01.03())
  output$table<-renderTable({
    tablein01 <- dataframe()
    tablein01 <- as.data.frame(tablein01)
    timein01 <- colnames(tablein01)
    timein01 <- timein01[1]
    if (is.null(input$check01)){
      return(NULL) 
    } 
      rownames = TRUE
      digits = 3
      tablein01[, c(timein01, input$check01)]
    })
  #=== input update part ===#
  observe({ # monitor and update the ui input
    obin01 <- res()
    if(is.null(input$datafile)){
      colna01 <- NULL
    } else if (input$checkGroup01 == 0) {
      colna01 <- rownames(res())
    } else if (input$checkGroup01 == 1) {
      colna01<-rownames(obin01[obin01$Group == 1, ])
    } else if (input$checkGroup01 == 2) {
      colna01<-rownames(obin01[obin01$Group == 2, ])
    }
    if (input$selall01 == TRUE){
      updateSelectizeInput(session, "check01",
                           label = "Select Cells",
                           choices = colna01,
                           selected = colna01)
    } else if(input$selall01 == FALSE){
      updateSelectizeInput(session, "check01",
                           label = "Select Cells",
                           choices = colna01,
                           selected = "")
    }
  })
    ###=== 01.end ===###
  #### 02.wave features ####
  #=== manipulation part ===#
  plot02.01<-reactive({ # plot among different features
    df <- res()
    gp <- NULL
    wzy<-colnames(df)
    if (!is.null(df)){
      xv <- input$xaxisGrp
      yv <- input$yaxisGrp
      if (!is.null(xv) & !is.null(yv)){
        if (sum(xv %in% names(df))>0){ # supress error when changing files
          mdf <- melt(df,id.vars=yv,measure.vars=xv)
          gp <- ggplot(data=mdf) + 
            geom_point(aes_string(x=yv,y="value")) +
            labs(x = yv, y = xv) +
            theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))
        }
      }
    }
    gp
  })
  plot02.02<-reactive({# histogram of each feature
    df<-res()
    gp<-NULL
    gp <- ggplot(df, aes_string(input$xaxisGrp))+
      geom_histogram()+labs(title = "Frequency Histogram of Y variable")+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))
    gp
  })
  plot02.03<-reactive({# histogram of each feature
    df<-res()
    gp<-NULL
    gp <- ggplot(df, aes_string(input$yaxisGrp))+
      geom_histogram()+labs(title = "Frequency Histogram of X variable")+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))
    gp
  })
  #=== output part ===#
  output$myresults <- DT::renderDataTable({
    f <- res()
    f <- round(f, digits = 3)
    if ((is.null(input$resSlect)) || (is.null(input$c2.2))){
      return(NULL) 
    } 
    else
    DT::datatable(f[input$c2, input$c2.2])
  })
  output$plot02.01 <- renderPlot(plot02.01())
  output$plot02.02 <- renderPlot(plot02.02())
  output$plot02.03 <- renderPlot(plot02.03())
  output$downloadAnaRes02 <- downloadHandler(
    filename = "WaveFeature.csv",
    content = function(file) {
      write.csv(res(), file, sep = ",")
    }
  )
  #=== input update part ===#
  observe({ # monitor and update the ui input
    obin02 <- res()
    dsnames <- row.names(obin02)
    colna02 <- colnames(obin02)
    colna02 <- c(colna02, "Dissimilarity", "Group")
    cb_options <- list()
    cb_options[ colna02] <- colna02
    updateRadioButtons(session, "yaxisGrp",
                       label = "X-Axis",
                       choices = cb_options,
                       selected = cb_options[1])
    updateRadioButtons(session, "xaxisGrp",
                       label = "Y-Axis",
                       choices = cb_options,
                       selected = cb_options[1])
    updateCheckboxGroupInput(session, "c2.2",
                         label = "Select features",
                         choices = cb_options,
                         selected = cb_options)

  })
  observe({
    obin02 <- res()
    dsnames <- row.names(obin02)
    if (input$resSlect == TRUE){
      updateSelectizeInput(session, "c2",
                           label = "Select a Single Cell",
                           choices = dsnames,
                           selected = dsnames)
    }
    else if(input$resSlect == FALSE){
      updateSelectizeInput(session, "c2",
                           label = "Select a Single Cell",
                           choices = dsnames,
                           selected = "")
    }
    
  })
      ###=== 02.end ===###
  #### 03.graphic results ####
  #=== manipulation part ===#
  waveletTransform<- reactive({
    df03.00 <- dataframe()
    df03.00 <- round(df03.00, digits = 5)
    wt03.00 <- wt(cbind(df03.00[, 1], df03.00[, input$sel03]), do.sig = FALSE)
    wt03.00
  })
  waveletSpectrum <- reactive({
    df03.01<-waveletTransform()
    res01.01<-dataframe()
    yv<-c(1:10)
    yl<-2^c(1:10)*(res01.01[3,1]-res01.01[2,1])
    gp03.01<-NULL
    if(!is.null(df03.01)){
      gp03.01<-plot(df03.01, type="power.corr.norm", main="Wavelet Spectrum")
    }
    return(gp03.01)
  })
  timeSeriesGraph <- reactive({
    df03.02<-dataframe()
    gp03.02<-NULL
    x<-df03.02[, 1]
    y<-df03.02[, input$sel03]
    if(!is.null(df03.02)){
      gp03.02<-ggplot(df03.02)+
        geom_line(aes(x = x, y = y))+
        labs(x = input$FirstColB01, y = "Fouresence Intensity (Gray Value)", title = input$sel03) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))
    }
    return(gp03.02)
  })
  PowerSpectrum <- reactive({
    df03.03<-dataframe()
    gp03.03<-NULL
    if(!is.null(df03.03)){
      fft03.03<-fft(df03.03[, input$sel03])
      sampleSize<-length(fft03.03)
      timeStep<-df03.03[3,1]-df03.03[2,1]
      gp03.03<-wzy.plot.frequency.spectrum(fft03.03, sampleSize = sampleSize, timeStep = timeStep)
    }
    return(gp03.03)
  })
  waveletVariance <- reactive({
    df03.04<-dataframe()
    gp03.04<-NULL
    wavelet<-waveletTransform()
    if(!is.null(df03.04)){
      y<-rowSums(abs(wavelet$wave)^2)
      x<-wavelet$period
      gp<-plot(cbind(x,y), type="l", main = "Wavelet Variance", xlab = "Period (s)", ylab = "Variance" )
    }
    return(gp)
  })
  PowerSpectralDensity <- reactive({
    df03.03<-dataframe()
    gp03.03<-NULL
    if(!is.null(df03.03)){
      fft03.03<-fft(df03.03[, input$sel03])
      sampleSize<-length(fft03.03)
      timeStep<-df03.03[3,1]-df03.03[2,1]
      gp03.03<-wzy.plot.frequency.spectrum.density(fft03.03, sampleSize = sampleSize, timeStep = timeStep)
    }
    return(gp03.03)
  })
  #=== output part ===#
  output$plot03.01 <- renderPlot(waveletSpectrum())
  output$plot03.02 <- renderPlot(timeSeriesGraph())
  output$plot03.03 <- renderPlot(PowerSpectrum())
  output$plot03.04 <- renderPlot(waveletVariance())
  output$plot03.05 <- renderPlot(PowerSpectralDensity())
  output$table03.01 <- renderTable({
    f <- res()
    f <- round(f, digits = 3)
    if ((is.null(input$resSlect)) || (is.null(input$c2.2))){
      return(NULL) 
    } 
    else
      f[input$sel03, ]
  })
  #=== input update part ===#
  observe({
    obin03 <- dataframe()
    colna03 <-colnames(obin03)
    colna03 <- colna03[-1]
    updateSelectInput(session, "sel03",
                      label = "Select Cells",
                      choices = colna03,
                      selected = colna03[1])
  })
    ###=== 03.end ===###
  #### 04.spatial features ####
  #=== manipulation part ===#
  plot04.01<-reactive({
    gp04<-NULL
    if ((!is.null(input$dataSpa04)) && (!is.null(input$dataLoc04))){
      loc04<-dataLoc()
      loc04<-loc04[-1, ]
      colnames(loc04)<-c("id", "x", "y")
      res04<-res()
      spa04<-dataSpa()
      spa04o<-spa04
      colnames(spa04)<-c("id", "x", "y")
      col04.res<-colnames(res04)
      col04.spa<-colnames(spa04)
      final04<-c()
      aribitrary04<-c()
      ln04<-length(res04[, 1])
      for(i in 1:ln04){
        times04 <- 0
        times04 <- length(spa04[spa04$id == i-1, "x"])
        aribitrary04 <- rep(res04[i, input$col04], times04)
        final04 <- c(final04, aribitrary04)
      }
      n<-cbind(spa04, final04)
      colnames(n)<-c("id", "x", "y", input$col04)
      n<-as.data.frame(n)
      if(input$label04 == TRUE){
        gp04 <- ggplot(n, aes(x=x, y=y))+geom_polygon(aes_string(x="x", y="y", group = "id", fill = input$col04), colour = "black")+
          geom_text(data = loc04, aes(x = x, y = y, label = id), alpha = 1, color = "black")+scale_y_reverse()
      } else if(input$label04 == FALSE) {
        gp04 <- ggplot(n, aes(x=x, y=y))+geom_polygon(aes_string(x="x", y="y", group = "id", fill = input$col04), colour = "black")+
          scale_y_reverse()
      }
      gp04<- gp04 + coord_fixed(ratio = 1, xlim = c(0, max(spa04o[, 2])), ylim =c(0, max(spa04o[, 2]))) + 
        theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.text = element_blank(), axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
              panel.background = element_blank()) + 
        scale_fill_gradientn(colours = heat.colors(10))
    }
    return(gp04);
  })
  plot04.02<-reactive({
    gp04<-NULL
    if ((!is.null(input$dataSpa04)) && (!is.null(input$dataLoc04))){
      loc04<-dataLoc()
      loc04<-loc04[-1, ]
      colnames(loc04)<-c("id", "x", "y")
      res04<-res()
      spa04<-dataSpa()
      spa04o<-spa04
      colnames(spa04)<-c("id", "x", "y")
      col04.res<-colnames(res04)
      col04.spa<-colnames(spa04)
      final04<-c()
      aribitrary04<-c()
      res04<-res04[-1, ]
      spa04<-spa04[spa04$id > 0, ]
      ln04<-length(res04[, 1])
      for(i in 1:ln04){
        times04 <- 0
        times04 <- length(spa04[spa04$id == i, "x"])
        aribitrary04 <- rep(res04[i, input$col04], times04)
        final04 <- c(final04, aribitrary04)
      }
      n<-cbind(spa04, final04)
      colnames(n)<-c("id", "x", "y", input$col04)
      n<-as.data.frame(n)
      if(input$label04 == TRUE){
        gp04 <- ggplot(n, aes(x=x, y=y))+geom_polygon(aes_string(x="x", y="y", group = "id", fill = input$col04), colour = "black")+
          geom_text(data = loc04, aes(x = x, y = y, label = id), alpha = 1, color = "black") + scale_y_reverse()
      } else if(input$label04 == FALSE){
        gp04 <- ggplot(n, aes(x=x, y=y))+geom_polygon(aes_string(x="x", y="y", group = "id", fill = input$col04), colour = "black")+
          scale_y_reverse()
      }
      border <- array(NA, dim=c(5, 2))
      border[, 1]<- c(0, 0, max(spa04o[, 2]), max(spa04o[, 2]), 0)
      border[, 2]<- c(0, max(spa04o[, 2]), max(spa04o[, 2]), 0, 0)
      border <- as.data.frame(border)
      gp04 <- gp04 + coord_fixed(ratio = 1, xlim = c(0, max(spa04o[, 2])), ylim =c(0, max(spa04o[, 2])) ) + 
        theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              axis.text = element_blank(), axis.line = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
              panel.border = element_blank(), panel.background = element_blank()) + geom_path(data=border, aes(x=border[, 1], y=border[, 2])) +
        scale_fill_gradientn(colours = heat.colors(10))
    }
    return(gp04);
  })
  table04.01 <- reactive({
    res04.01 <- NULL
    if ((!is.null(input$dataSpa04)) && (!is.null(input$dataLoc04))){
      ozone <- dataLoc()
      res04t <- res()
      ncol04 <- ncol(res04t)
      res04t<-res04t[-1,] #remove the data from region
      ozone<-ozone[-1,] #remove the data from region
      rownames04 <- colnames(res04t)
      ozone.dists<- as.matrix(dist(cbind(ozone[, 2], ozone[, 3])))
      ozone.dists.inv <- 1/ozone.dists
      diag(ozone.dists.inv) <- 0
      P.value <- c()
      Moran.I <- c()
      for(resin04 in 1:ncol04){
        moran <- Moran.I(res04t[, resin04], ozone.dists.inv)
        Moran.I <- c(Moran.I, moran$observed)
        P.value <- c(P.value, moran$p.value)
      }
      Moran.I <- as.numeric(Moran.I)
      P.value <- as.numeric(P.value)
      res04.01<-cbind(rownames04, Moran.I, P.value)
      colnames(res04.01) <- c("Feature", "Moran index", "P value")
      res04.01
    }
  })
  #=== output part ===#
  output$plot04.01<-renderPlot(plot04.01())
  output$plot04.02<-renderPlot(plot04.02())
  output$table04.01<-renderTable({
    if ((!is.null(input$dataSpa04)) && (!is.null(input$dataLoc04))){
      table04.01()
    } else
      return(NULL)
  })
  #=== input update part ===#
  observe({
    obin04<-res()
    options04 <- colnames(obin04)
    updateRadioButtons(session, "col04",
                       label = "Choose Method",
                       choices = options04,
                       selected = options04[1])
  }) #col04
    ###=== 04.end ===###
####=== Batching Processing ===####
  #### 01. data input ####
  #=== manipulation part ===#
  resB01<-eventReactive(input$anB01, {
    if(is.null(input$directory)) {
      stop("Please choose a directory")
    }
    Dir<-parseDirPath(volumes, input$directory)
    fl<-fileList()
    pat01<-input$pat01
    pat01_01<-input$pat01_01
    pat02<-input$pat02
    prefix_position<-unlist(gregexpr(pattern = pat01, fl))
    prefix<-substr(fl, 1, prefix_position-1)
    res<-data.frame()
    ids<-c()
    withBusyIndicatorServer("anB01",{
      id<-c()
      InUseName<-paste(prefix[1], pat01, pat01_01, ".csv", sep = "")
      InUseName02<-paste(prefix[1], pat01, pat02, ".csv", sep = "")
      file01<-read.csv(paste(Dir, "/", InUseName, sep = ""), header = TRUE, sep = ",")
      ncol<-NCOL(file01)
      label<-c()
      for(i in 1:ncol){
        if(i == 1){
          label[1] <- input$FirstColB01
        } else if (i == 2){
          label[i] <- str_c("S", prefix[1], input$SecondColB01)
        } else {
          label[i] <- str_c("S", prefix[1], input$ThirdColB01, " ", i-2)
        }
      }
      colnames(file01)<-label
      file02<-read.csv(paste(Dir, "/", InUseName02, sep=""), header = TRUE, sep = ",")
      rawBatchData<-file01
      rawBatchData.cells<-file01[, -2]
      rawBatchData.region<-file01[, 1:2]
      
      
      if(input$calibrationB01 == TRUE){
        res00<-wzy.batch2(wzy = file01, loc = file02)
      } else if(input$calibrationB01 == FALSE){
        res00<-wzy.batch(wzy = file01, loc = file02)
      }
      
      
      label <- c(label, str_c("S", prefix[1], "Moran Index"), str_c("S", prefix[1], "P value"))
      rownames(res00) <- label[-1]
      isolate(values$Row_names <- c(values$Row_names, label[-1]))
      ids<-c(ids, prefix[1])
      res00<-cbind(res00, id = prefix[1])
      res<-rbind(res, res00)
      prefix<-prefix[! prefix %in% prefix[1]]
      while(length(prefix) > 1){
        id<-c()
        InUseName<-paste(prefix[1], pat01, pat01_01, ".csv", sep = "")
        InUseName02<-paste(prefix[1], pat01, pat02, ".csv", sep = "")
        file01<-read.csv(paste(Dir, "/", InUseName, sep = ""), header = TRUE, sep = ",")
        ncol<-NCOL(file01)
        label<-c()
        for(i in 1:ncol){
          if(i == 1){
            label[1] <- input$FirstColB01
          } else if (i == 2){
            label[i] <- str_c("S", prefix[1], input$SecondColB01) 
          } else
            label[i] <- str_c("S", prefix[1], input$ThirdColB01, " ", i-2)
        }
        colnames(file01)<-label
        file02<-read.csv(paste(Dir, "/", InUseName02, sep=""), header = TRUE, sep = ",")
        rawBatchData<-cbind(rawBatchData, file01[,-1])
        rawBatchData.cells<-cbind(rawBatchData.cells, file01[, -c(1:2)])
        rawBatchData.region<-cbind(rawBatchData.region, file01[, 2])
        if(input$calibrationB01 == TRUE){
          res00<-wzy.batch2(wzy = file01, loc = file02)
        } else if(input$calibrationB01 == FALSE){
          res00<-wzy.batch(wzy = file01, loc = file02)
        }
        label <- c(label, str_c("S", prefix[1], "Moran Index"), str_c("S", prefix[1], "P value"))
        rownames(res00) <- label[-1]
        isolate(values$Row_names <- c(values$Row_names, label[-1]))
        ids<-c(ids, prefix[1])
        res00<-cbind(res00, id = prefix[1])
        res<-rbind(res, res00)
        prefix<-prefix[! prefix %in% prefix[1]]
      }
    })
    isolate(values$sampleSize <- as.numeric(length(ids)))
    Row_names<-values$Row_names
    rownames(res)<-NULL
    isolate(values$cells <- rawBatchData.cells)
    isolate(values$region <- rawBatchData.region)
    res<-cbind(Row_names, res)
    res<-list(res, ids, rawBatchData)
    isolate(values$colnames <- colnames(rawBatchData))
    return(res)
  })
  fileList<-reactive({
    if(is.null(input$directory)){
      return(NULL)
    }
    fileDir<-parseDirPath(volumes, input$directory)
    pat = input$postfix
    fl<-list.files(path = fileDir, all.files = FALSE)
    fl <- grep(pat, fl, value = TRUE)
    fl<-sort(fl)
    fl
  })
  #=== output part ===#
  output$tableB01.00<-DT::renderDataTable(
      values$tableB01.00, 
      caption = 'Table 1: Files List',
      rownames = FALSE,
      options = list(
        searching = FALSE
      )
  )
  output$tableB01.01<-DT::renderDataTable(
    values$tableB01.01,
    caption = 'Table 2: Wave Feature',
    rownames = FALSE
  )
  output$tableB01.02<-DT::renderDataTable(
    tableB01.02 <- wzy.force.format.colname(x = values$tableB01.02, colname = values$colnames),
    caption = 'Table 3: Raw Data',
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$tableB01.03<-DT::renderDataTable(
    values$tableB01.03, 
    caption = 'Table 4: Samples ID',
    rownames = FALSE,
    options = list(
      searching = FALSE
    )
  )
  output$downloadAnaResB01 <- downloadHandler(
    filename = "Table_1.csv",
    content = function(file) {
      dir.create(paste(parseDirPath(volumes, input$directory), "/AnalysisResults/", sep = ""), showWarnings = FALSE)
      write.csv(values$tableB01.00, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_1.csv", sep = ""), row.names = FALSE)
      write.csv(values$tableB01.01, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_2.csv", sep = ""), row.names = FALSE)
      write.table(values$tableB01.02, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_3.csv", sep = ""), row.names = FALSE, col.names = values$colnames, sep = ",")
      write.csv(values$cells, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_3.cells.csv", sep = ""), row.names = FALSE)
      write.csv(values$region, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_3.region.csv", sep = ""), row.names = FALSE)
      write.csv(values$tableB01.03, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_4.csv", sep = ""), row.names = FALSE)
    }
  )
  #=== input update part ===#
  values$tableB01.00<-data.frame("Table 1"=NA)
  values$tableB01.01<-data.frame("Table 2"=NA)
  values$tableB01.02<-data.frame("Table 3"=NA)
  values$tableB01.03<-data.frame("Table 4"=NA)
  observe({
    if(is.null(input$uploadAnaResB01) && ! is.null(input$directory)){
      tableB01.00<-fileList()
      No.<-c(1:length(tableB01.00))
      tableB01.00<-cbind(No., data = tableB01.00)
    } else if(! is.null(input$uploadAnaResB01)){
      tableB01.00<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_1.csv", sep = ""), header=TRUE, sep=",")
    } else {tableB01.00 <- data.frame("Table 1"=NA)}
    isolate(values$tableB01.00<-tableB01.00)
  }) #TableB01.00 | Table 1
  observe({
    if(is.null(input$uploadAnaResB01) && ! is.null(input$directory)){
      res<-resB01()
      tableB01.01<-as.data.frame((res[1]))
    } else if(! is.null(input$uploadAnaResB01)){
      tableB01.01<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_2.csv", sep = ""), header=TRUE, sep=",")
    } else {tableB01.01 <- data.frame("Table 2"=NA)}
    isolate(values$tableB01.01<-tableB01.01)
  }) #TableB01.01 | Table 2
  observe({
    if(is.null(input$uploadAnaResB01) && ! is.null(input$directory)){
      res<-resB01()
      tableB01.02<-as.data.frame((res[3]))
    } else if(! is.null(input$uploadAnaResB01)){
      tableB01.02<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_3.csv", sep = ""), header=FALSE, sep=",")
      isolate(values$colnames <- as.vector(unlist(tableB01.02[1,])))
      tableB01.02<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_3.csv", sep = ""), header=TRUE, sep=",")
    } else {tableB01.02 <- data.frame("Table 3"=NA)}
    isolate(values$tableB01.02<-tableB01.02)
  }) #TableB01.02 | Table 3
  observe({
    if(is.null(input$uploadAnaResB01) && ! is.null(input$directory)){
      res<-resB01()
      tableB01.03<-as.vector(unlist(res[2]))
      tableB01.03<-data.frame(ID = tableB01.03, Group = NA, Label = NA)
    } else if(! is.null(input$uploadAnaResB01)){
      tableB01.03<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_4.csv", sep = ""), header=TRUE, sep=",")
    } else {tableB01.03 <- data.frame("Table 4"=NA)}
    isolate(values$tableB01.03<-tableB01.03)
    isolate(values$sampleSize <- length(values$tableB01.03[, 1]))
  }) #TableB01.03 | Table 4
    #=== 01.end ===#
  #### 02. Statistical Analysis ####
  #=== manipulation part ===#
  values$wavefeatureB02 <- data.frame("NoData" = NA)
  values$distributionB02 <- data.frame("NoData" = NA)#MoranI
  values$MoranPB02 <- data.frame("NoData" = NA)#MoranI P-value
  values$regionB02 <- data.frame("NoData" =NA)
  values$GlobalClustB02 <- NULL
  observeEvent(input$staB02, {
    if(is.null(values$tableB01.01)) {
      return(NULL)
    }
    withBusyIndicatorServer("staB02", {
      isolate(values$tableB01.01$id <- as.character(values$tableB01.01$id))
      isolate(values$tableB01.01$Row_names <- as.character(values$tableB01.01$Row_names))
      isolate(colnames(values$tableB01.02) <- values$colnames)
      tag<-c()
      for(i in 1:dim(values$tableB02)[1]) {
        tag<-c(tag, as.vector(rep(values$tableB02$Label[i], length(values$tableB01.01$id[values$tableB01.01$id == values$tableB02$ID[i]]))))
        isolate(values$tableB01.01$id[values$tableB01.01$id == values$tableB02$ID[i]] <- as.character(values$tableB02$Group[i]))
        isolate(colnames(values$tableB01.02) <- str_replace_all(colnames(values$tableB01.02), pattern = str_c("S", as.character(values$tableB02$ID[i]), sep = ""), 
                                                                str_c("G", as.character(values$tableB02$Group[i]), "_",as.character(values$tableB02$ID[i]), sep = "")))
      } #Get tag from group setting
      if(is.null(colnames(values$tableB01.02))) {
        stop("Please download the analysis result after analysis.")
      }
      isolate(colnames(values$tableB01.01)[colnames(values$tableB01.01) == "id"] <- "Label")
      isolate(values$tableB01.01 <- cbind(values$tableB01.01, Tag = tag))
      isolate(rownames(values$tableB01.01)<-values$tableB01.01$Row_names)
      isolate(values$distributionB02 <- values$tableB01.01[grep("Moran Index", values$tableB01.01$Row_names, value = TRUE), ])
      isolate(values$distributionB02 <- values$distributionB02[order(values$distributionB02$Label), ])
      isolate(values$tableB01.01 <- values$tableB01.01[! rownames(values$tableB01.01) %in% grep("Moran Index", rownames(values$tableB01.01), value = TRUE), ])
      isolate(values$MoranPB02 <- values$tableB01.01[grep("P value", values$tableB01.01$Row_names, value = TRUE), ])
      isolate(values$MoranPB02 <- values$MoranPB02[order(values$MoranPB02$Label),])
      isolate(values$tableB01.01 <- values$tableB01.01[! rownames(values$tableB01.01) %in% grep("P value", rownames(values$tableB01.01), value = TRUE), ])
      isolate(values$wavefeatureB02 <- values$tableB01.01[grep(input$ThirdColB01, values$tableB01.01$Row_names, value = TRUE), ])
      isolate(values$wavefeatureB02 <- values$wavefeatureB02[order(values$wavefeatureB02$Label),])
      isolate(values$wavefeatureB02 <- values$wavefeatureB02 <- values$wavefeatureB02[, -12])
      isolate(values$regionB02 <- values$tableB01.01[grep(input$SecondColB01, values$tableB01.01$Row_names, value = TRUE), ])
      isolate(values$regionB02 <- values$regionB02[order(values$regionB02$Label),])
      isolate(values$regionB02$Ratio[values$regionB02$Label == 0]<- 100 - values$regionB02$Ratio[values$regionB02$Label == 0])
    })
  })
  #=== output part ===#
  output$tableB02.00<-DT::renderDataTable(
    values$tableB02,
    caption = 'Group Setting',
    rownames = FALSE,
    options = list(
      lengthChange = FALSE,
      pageLength = values$sampleSize,
      searching = FALSE
    )
  )
  output$downloadGroupSet<-downloadHandler(
    filename = "Group_Set.csv",
    content = function(file){
      write.csv(values$tableB02, file, row.names = FALSE)
    }
  )
  output$tableRegionB02<-DT::renderDataTable(
    values$regionB02,
    caption = 'Raw Results of each entire sample check',
    rownames = FALSE
  )
  output$tableWavefeatureB02<-DT::renderDataTable(
    values$wavefeatureB02,
    caption = 'Raw Results of Wave feature check',
    rownames = FALSE
  )
  output$tableDistributionB02<-DT::renderDataTable(
    values$distributionB02,
    caption = 'Raw Results of distribution check',
    rownames = FALSE
  )
  output$tableMoranPB02<-DT::renderDataTable(
    values$MoranPB02,
    caption = 'Raw Results of P value check',
    rownames = FALSE
  )
  #=== input updata part ===#
  values$tableB02 <- data.frame()
  observe({
    if(is.null(input$GroupSetB02)){
      isolate(values$tableB02 <- data.frame(ID = NA, Group = NA, Label = NA))
    } else {
      GroupSetB02<-read.csv(input$GroupSetB02$datapath, header = TRUE, sep = ",")
      isolate(values$tableB02 <- GroupSetB02)
    }
  })
  observeEvent(input$AssigB02,{
    seRowB02<-input$tableB02.00_rows_selected
    isolate(values$tableB02[seRowB02,2]<-as.numeric(input$GroupMarkB02))
    isolate(values$tableB02[seRowB02,3]<-input$LabelB02)
  })
  observeEvent(input$updateB02, {
    isolate(values$tableB02<-values$tableB01.03)
  })
    #=== 02.end ===#
  #### 03. Statistical Results ####
    #=== maniupaltion part ===#
  values$GroupComparB03 <- NULL
  SummaryWaveB03 <- eventReactive(input$staB02, {
    P.value <- c()
    Statistic <- c()
    for(i in 2:11){
      test <- wilcox.test(values$wavefeatureB02[, i] ~ values$wavefeatureB02$Label) #independent 2-gtoup Mann-Whitney U test see here: https://www.statmethods.net/stats/nonparametric.html
      P.value <-c(P.value, test$p.value) 
      Statistic <- c(Statistic, test$statistic)
    }
    group.n <- as.numeric(max(values$tableB02$Group))+1
    summary <- describeBy(values$wavefeatureB02[, 2:11], values$wavefeatureB02$Tag)
    out <- data.frame()
    for(i in 1:group.n) {
      out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
    }
    out<-rbind(out, P.value = P.value, Statistic = Statistic)
    return(out)
  })
  SummaryRegionB03 <- eventReactive(input$staB02, {
    P.value <- c()
    Statistic <- c()
    for(i in c(2:9, 12)){
      test <- wilcox.test(values$regionB02[, i] ~ values$regionB02$Label)
      P.value <-c(P.value, test$p.value) 
      Statistic <- c(Statistic, test$statistic)
    }
    group.n <- as.numeric(max(values$tableB02$Group))+1
    summary <- describeBy(values$regionB02[, c(2:9, 12)], values$regionB02$Tag)
    out <- data.frame()
    for(i in 1:group.n) {
      out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
    }
    out<-rbind(out, P.value = P.value, Statistic = Statistic)
    return(out)
  })
  SummaryMoranIndexB03 <- eventReactive(input$staB02, {
    P.value <- c()
    Statistic <- c()
    for(i in 2:11){
      test <- wilcox.test(values$distributionB02[, i] ~ values$distributionB02$Label)
      P.value <-c(P.value, test$p.value) 
      Statistic <- c(Statistic, test$statistic)
    }
    group.n <- as.numeric(max(values$tableB02$Group))+1
    summary <- describeBy(values$distributionB02[, 2:11], values$distributionB02$Tag) #General Statistical Describe
    out <- data.frame()
    for(i in 1:group.n) {
      out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
    }
    out<-rbind(out, P.value = P.value, Statistic = Statistic)
    return(out)
  })
  SummaryMoranPB03 <- eventReactive(input$staB02, {
    P.value <- c()
    Statistic <- c()
    x <- values$MoranPB02
    for(i in 2:11){
      groupChisqB04<-data.frame(Group1 = c(0,0), Group2 = c(0,0))
      rownames(groupChisqB04) <- c(x$Tag[x$Label == 0][1],
                                   x$Tag[x$Label == 1][1])
      groupChisqB04[1,1] <- length(x$Label[x[, i] < 0.05][x$Label[x[, i] < 0.05]==0])
      groupChisqB04[1,2] <- length(x$Label[x[, i] < 0.05][x$Label[x[, i] < 0.05]==1])
      groupChisqB04[2,1] <- length(x$Label[x[, i] >= 0.05][x$Label[x[, i] >= 0.051]==0])
      groupChisqB04[2,2] <- length(x$Label[x[, i] >= 0.05][x$Label[x[, i] >= 0.05]==1])
      test <- chisq.test(groupChisqB04, correct = FALSE)
      if(test$p.value == "NaN") {
        P.value <-c(P.value, 1)
      } else {
        P.value <-c(P.value, test$p.value) 
      }
      if(test$statistic == "NaN") {
        Statistic <- c(Statistic, 0)
      } else {
        Statistic <- c(Statistic, test$statistic)
      }
    }
    group.n <- as.numeric(max(values$tableB02$Group))+1
    summary <- describeBy(values$MoranPB02[, 2:11], values$MoranPB02$Tag)
    out <- data.frame()
    for(i in 1:group.n) {
      out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
    }
    out<-rbind(out, P.value = P.value, Statistic = Statistic)
    return(out)
  })
  SummaryGroupComparB03 <- eventReactive(input$staB02, {
    x<-values$wavefeatureB02
    df<-x[1:10]
    Row_names <- c(rep(paste(x$Tag[x$Label == 0][1], "Group1", sep = "."), length(x[, 1][x$Label == 0][x$Group[x$Label == 0]==1])),
                   rep(paste(x$Tag[x$Label == 0][1], "Group2", sep = "."), length(x[, 1][x$Label == 0][x$Group[x$Label == 0]==2])),
                   rep(paste(x$Tag[x$Label == 1][1], "Group1", sep = "."), length(x[, 1][x$Label == 1][x$Group[x$Label == 1]==1])),
                   rep(paste(x$Tag[x$Label == 1][1], "Group2", sep = "."), length(x[, 1][x$Label == 1][x$Group[x$Label == 1]==2])))
    for (i in 2:10) {
      data.group<-c(x[, i][x$Label == 0][x$Group[x$Label == 0]==1],
                    x[, i][x$Label == 0][x$Group[x$Label == 0]==2],
                    x[, i][x$Label == 1][x$Group[x$Label == 1]==1],
                    x[, i][x$Label == 1][x$Group[x$Label == 1]==2])
      df[, i]<-data.group
    }
    df$Row_names <- Row_names
    isolate(values$GroupComparB03<-df)
    summary <- describeBy(df[, 2:10], df$Row_names)
    p.adjusted<-data.frame()
    for (i in 2:10) {
      test<-dunn.test(df[,i], df[,1], kw = FALSE, method = "by") #Dunn's test of multiple comparisons
      if (i == 2) {
        p.adjusted<-data.frame(test$comparisons, test$P.adjusted)
      } else {
        p.adjusted<-cbind(p.adjusted, test$P.adjusted)
      }
    }
    out <- data.frame()
    for(i in 1:4) {
      out <- rbind(out, as.data.frame(t(as.data.frame(summary[i]))))
    }
    out<-cbind(Row_names = rownames(out), out)
    colnames(p.adjusted) <- colnames(df)
    out<-rbind(out, p.adjusted)
    return(out)
  }) #multiple comparison
    #=== output part ===#
  output$downloadStaResB03 <- downloadHandler(
    filename = "Result_1.csv",
    content = function(file) {
      dir.create(paste(parseDirPath(volumes, input$directory), "/Statistic_Results/", sep = ""), showWarnings = FALSE)
      write.csv(values$wavefeatureB02, file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/SplitData_1.csv", sep = ""), row.names = FALSE)
      write.csv(values$regionB02, file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/SplitData_2.csv", sep = ""), row.names = FALSE)
      write.csv(values$distributionB02, file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/SplitData_3.csv", sep = ""), row.names = FALSE)
      write.csv(values$MoranPB02, file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/SplitData_4.csv", sep = ""), row.names = FALSE)
      write.csv(SummaryWaveB03(), file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/Result_1.csv", sep = ""), row.names = TRUE)
      write.csv(SummaryRegionB03(), file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/Result_2.csv", sep = ""), row.names = TRUE)
      write.csv(SummaryMoranIndexB03(), file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/Result_3.csv", sep = ""), row.names = TRUE)
      write.csv(SummaryMoranPB03(), file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/Result_4.csv", sep = ""), row.names = TRUE)
      write.csv(SummaryGroupComparB03(), file = paste(parseDirPath(volumes, input$directory), "/Statistic_Results/Result_5.csv", sep = ""), row.names = TRUE)
    }
  )
  output$plotClustB03<-renderPlot(plotClustB03())
  output$tableWaveB03<-DT::renderDataTable(
    format(SummaryWaveB03(), digits = 2, scientific = FALSE),
    caption = 'Results 1: wave feature for each cell',
    options = list(
      pageLength = length(SummaryWaveB03()[,1]),
      lengthChange = FALSE
    )
  )
  output$tableRegionB03<-DT::renderDataTable(
    format(SummaryRegionB03(), digits = 2, na.encode = TRUE, scientific = FALSE),
    caption = 'Results 2: wave feature for region',
    options = list(
      pageLength = length(SummaryRegionB03()[,1]),
      lengthChange = FALSE
    )
  )
  output$tableMoranIndexB03<-DT::renderDataTable(
    format(SummaryMoranIndexB03(), digits = 2, scientific = FALSE),
    caption = 'Results 3: Moran index for each wave feature',
    options = list(
      pageLength = length(SummaryMoranIndexB03()[,1]),
      lengthChange = FALSE
    )
  )
  output$tableMoranPB03<-DT::renderDataTable(
    format(SummaryMoranPB03(), digits = 2, scientific = FALSE),
    caption = 'Results 4: P value of Moran index for each wave feature',
    options = list(
      pageLength = length(SummaryMoranPB03()[,1]),
      lengthChange = FALSE
    )
  )
  output$tableGroupComparB03<-DT::renderDataTable(
    format(SummaryGroupComparB03(), digits = 2, scientific = FALSE),
    caption = 'Results 5: Wave Feature among Groups',
    options = list(
      pageLength = length(SummaryGroupComparB03()[,1]),
      lengthChange = FALSE
    )
  )
    #=== 03.end ===#
  #### 04. Plot Output ####
    #=== manipulation part ===#
  plotBoxB04.00<-reactive({
    o<-values$wavefeatureB02 #input data
    start<-2
    end<-10
    GroupLabel<-"Label" #to sort
    GroupTag<-"Tag" #to identify
    Pvalue<-round(as.numeric(SummaryWaveB03()["P.value", ]), 10)
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", input$FirstColB01, "Gray Level", "Hz", "Arbitrary Unit")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Dissimilarity to Region")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    q<-c(start:end)
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.99"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], q[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                  y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                  tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"), legend.position="none")+ylim(NA, max(df$y100)*1.1)
      if(input$ViolinB04 == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, q[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }) 
  plotBoxB04.01<-reactive({
    o<-values$regionB02 #input data
    start<-2
    end<-9
    GroupLabel<-"Label" #to sort
    GroupTag<-"Tag" #to identify
    Pvalue<-round(as.numeric(SummaryRegionB03()["P.value", ]), 10)
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", input$FirstColB01, "Gray Level", "Hz", "Percentage (%)")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Percentage of Group 1")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    q<-c(start:end)
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.999"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], q[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                          y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                          tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"))+ylim(NA, max(df$y100)*1.1)
      if(input$ViolinB04 == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, q[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
    }
  }) 
  plotBoxB04.02<-reactive({
    o<-values$distributionB02 #input data
    start<-2
    end<-9
    GroupLabel<-"Label" #to sort
    GroupTag<-"Tag" #to identify
    Pvalue<-round(as.numeric(SummaryMoranIndexB03()["P.value", ]), 10)
    cols <- 3
    YaxisNames <- c("Moran Index", "Moran Index", "Moran Index", "Moran Index", "Moran Index", "Moran Index", "Moran Index", "Moran Index", "Moran Index", "Moran Index")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Dissimilarity to Region", "Group")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    q<-c(start:end)
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.999"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], q[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                          y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                          tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"))+ylim(NA, max(df$y100)*1.10)
      if(input$ViolinB04 == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, q[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
    }
  }) 
  plotBoxB04.03<-reactive({
    o<-values$MoranPB02 #input data
    start<-2
    end<-9
    GroupLabel<-"Label" #to sort
    GroupTag<-"Tag" #to identify
    Pvalue<-round(as.numeric(SummaryMoranPB03()["P.value", ]), 10)
    cols <- 3
    YaxisNames <- c("P value of Moran Index", "P value of Moran Index", "P value of Moran Index", "P value of Moran Index", "P value of Moran Index", "P value of Moran Index", "P value of Moran Index", "P value of Moran Index", "P value of Moran Index", "P value of Moran Index")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Dissimilarity to Region", "Group")
    dfx<-c()
    for(i in 1: length(o[, GroupTag])) {
      if(i == 1) {
        j<-1
        dfx<-c(dfx, as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTag][i]), dfx)))) {
        j<-j+1
        dfx[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTag][i])
      }
    }
    q<-c(start:end)
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      anno <- if(Pvalue[i]>=0.9999){
        "P>0.999"
      } else if(Pvalue[i]<0.9999 && Pvalue[i]>0.05){
        paste("P>", format(floor(Pvalue[i]*1000)/1000, digits = 3), sep = "")
      } else if(Pvalue[i]<0.05 && Pvalue[i]>0.01){
        "*"
      } else if(Pvalue[i]<0.01 && Pvalue[i]>0.001){
        "**"
      } else if(Pvalue[i]<0.001 && Pvalue[i]>0.0001){
        "***"
      } else if(Pvalue[i]<0.0001) {"****"}
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfx)) {
        temp<-as.numeric(o[o[, GroupTag] == dfx[j], q[i]])
        y0<-c(y0, min(temp))
        y25<-c(y25, quantile(temp, 0.25))
        y50<-c(y50, median(temp))
        y75<-c(y75, quantile(temp, 0.75))
        y100<-c(y100, max(temp))
        ymean<-c(ymean, mean(temp))
      }
      df<-data.frame(x=dfx, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      signif<-geom_signif(annotation=c(anno),
                          y_position=max(df$y100) * 1.05, xmin=1, xmax=2, textsize = 7, 
                          tip_length = c(0,0))
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
                     stat = "identity", position="dodge") +signif+
        labs(y = YaxisNames[i], title = TitleNames[i] ) + geom_hline( yintercept = 0.05, color = "Red") +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"))+ylim(NA, max(df$y100)*1.10)
      if(input$ViolinB04 == TRUE) {
        gp<-gp+geom_violin(data = o, aes(x=o[, GroupTag], y= o[, q[i]], alpha = 0.3))+
          geom_point(colour = "red", aes(y=ymean))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
    }
  }) 
  plotBoxB04.04<-reactive({
    RES<-SummaryGroupComparB03()
    position<-c(1.17, 1.29, 1.17, 1.41, 1.05, 1.29)
    xmin<-c(0.75, 1.77, 1.27, 0.75, 1.3, 0.75)
    xmax<-c(1.23, 2.25, 2.25, 2.25, 1.7, 1.73)
    o<-values$wavefeatureB02 #input data
    start<-2
    end<-10
    GroupLabel<-"Label" #to sort
    GroupTagL1<-"Tag" #to identify at level 1
    GroupTagL2<-"Group" #to identify at level 2
    cols <- 3
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", input$FirstColB01, "Gray Level", "Hz", "Arbitrary Unit")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Dissimilarity to Region")
    dfxL1<-c()
    for(i in 1: length(o[, GroupTagL1])) {
      if(i == 1) {
        j<-1
        dfxL1<-c(dfxL1, as.character(o[order(o[, GroupLabel]), ][, GroupTagL1][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTagL1][i]), dfxL1)))) {
        j<-j+1
        dfxL1[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTagL1][i])
      }
    }
    dfxL2<-c()
    for(i in 1: length(o[, GroupTagL2])) {
      if(i == 1) {
        j<-1
        dfxL2<-c(dfxL2, as.character(o[order(o[, GroupLabel]), ][, GroupTagL2][i]))
      } else if(is.na(c(match(as.character(o[order(o[, GroupLabel]), ][, GroupTagL2][i]), dfxL2)))) {
        j<-j+1
        dfxL2[j] <-as.character(o[order(o[, GroupLabel]), ][, GroupTagL2][i])
      }
    }
    q<-c(start:end)
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      signif<-c()
      for(n in 53:58) {
        anno6 <-if(RES[n,q[i]]>=0.9999){
          "P>0.999"
        } else if(RES[n,q[i]]>0.025){
          paste("P>", format(floor(RES[n, q[i]]*1000)/1000, digits = 3), sep = "") 
        } else if(RES[n,q[i]]<0.025 && RES[n,q[i]]>0.001){
          "*"
        } else if(RES[n,q[i]]<0.001 && RES[n,q[i]]>0.0001){
          "**"
        }else if(RES[n,q[i]]<0.0001 && RES[n,q[i]]>0.00001){
          "***"
        } else {"****"}
        anno<- geom_signif(annotation=formatC(anno6, digits=2),
                             y_position=max(o[, q[i]])*position[n-52], xmin=xmin[n-52], xmax=xmax[n-52], textsize = 5, 
                             tip_length = c(0,0))
        signif<-c(signif, anno)
      }
      x3<-c(0.75, 1.25, 1.75, 2.25)
      y0<-c()
      y25<-c()
      y50<-c()
      y75<-c()
      y100<-c()
      ymean<-c()
      for(j in 1:length(dfxL1)) {
        for(h in 1:length(dfxL2)) {
          temp<-as.numeric(o[o[GroupTagL1] == dfxL1[j], ][o[o[GroupTagL1] == dfxL1[j], ][GroupTagL2] == dfxL2[h], q[i]])
          y0<-c(y0, min(temp))
          y25<-c(y25, quantile(temp, 0.25))
          y50<-c(y50, median(temp))
          y75<-c(y75, quantile(temp, 0.75))
          y100<-c(y100, max(temp))
          ymean<-c(ymean, mean(temp))
        }
      }
      df<-data.frame(x=rep(dfxL1, each=2), x2=rep(dfxL2, 2), x3=x3, y0=y0, y25=y25, y50=y50, y75=y75, y100=y100)
      gp <- ggplot(df, aes(x=x, y=y100)) +
        geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100, colour = factor(df$x2)),
                     stat = "identity", position=position_dodge(width = 1.01), varwidth = TRUE) +signif[1:6]+
        labs(y = YaxisNames[i], title = TitleNames[i] , colour = "Group by\n Wavelet Clust") +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.title.x=element_blank(), axis.text=element_text(size=14), 
              axis.title=element_text(size=14,face="bold"))+ylim(NA, max(df$y100)*1.43)
      if(input$ViolinB04 == TRUE) {
        gp<-gp+geom_violin(data = o, position=position_dodge(width = 1.01),
                           aes(x=o[, GroupTagL1], y= o[, q[i]], alpha = 0.3, colour = factor(o[, GroupTagL2])))+
          geom_point(aes(x=x3, y=ymean, colour = factor(x2)))+scale_alpha(guide = "none")
      }
      print(gp, vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
    }
  }) 
  plotBoxB04.05<-reactive({
    o <- values$wavefeatureB02
    start<-2
    end<-9
    cols<-2
    q<-c(start:end)
    YaxisNames <- c("Gray Level", "Gray Level", "Gray Level", "Gray Level", "Gray Level", input$FirstColB01, "Gray Level", "Hz", "Arbitrary Unit")
    TitleNames <- c("Integrated", "Mean Absolute value", "Variance", "Root Mean Square", "Waveform Length", "Main Period", "Maximal Amplitude", "Mean Power Frequency", "Dissimilarity to Region")
    numPlots <- end - start + 1 #Plots numbers
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    #Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = nrow(layout), ncol = ncol(layout))))
    for(i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx<-as.data.frame(which(layout == i, arr.ind = TRUE))
      gp<-ggplot(o, aes(x = o[, q[i]], fill = o$Tag))+geom_histogram(position="dodge")+
        labs(y = "Count", x = YaxisNames[i], title = TitleNames[i])+
        guides(fill=guide_legend(title="Group"))+
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
              axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))
      print(gp, vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))
    }
  })
    #=== output part ===#
  output$plotBoxB04.00<-renderPlot(plotBoxB04.00()) #Cell level
  output$plotBoxB04.01<-renderPlot(plotBoxB04.01()) #Region level
  output$plotBoxB04.02<-renderPlot(plotBoxB04.02()) #Moran index for each wave feature
  output$plotBoxB04.03<-renderPlot(plotBoxB04.03()) #P value of Moran index for each wave
  output$plotBoxB04.04<-renderPlot(plotBoxB04.04()) #Comparsion among groups
  output$plotBoxB04.05<-renderPlot(plotBoxB04.05()) #Histogram
  output$uiB04.side <- renderUI({
    switch(input$levelB04,
           "Cell Level" = NULL,
           "Region Level" = NULL,
           "Moran Index" = NULL,
           "Significance of Moran Index" = NULL,
           "Comparsion among Groups" = NULL,
           "Histogram" = NULL
    )
  })
  output$uiB04 <- renderUI({
    switch(input$levelB04,
      "Cell Level" = list(
        tags$h4("Wave feature for each cell"),
        tags$hr(),
        plotOutput("plotBoxB04.00", width = "800px", height = "800px")
      ),
      "Region Level" = list(
        tags$h4("Wave feature for Region"),
        tags$hr(),
        plotOutput("plotBoxB04.01", width = "800px", height = "800px")
      ),
      "Moran Index" = list(
        tags$h4("Moran index for each wave feature"),
        tags$hr(),
        plotOutput("plotBoxB04.02", width = "800px", height = "1067px")
      ),
      "Significance of Moran Index" = list(
        tags$h4("P value of Moran index for each wave feature"),
        tags$hr(),
        plotOutput("plotBoxB04.03", width = "800px", height = "1067px")
      ),
      "Comparsion among Groups" = list(
        tags$h4("Comparsion among Groups"),
        tags$hr(),
        plotOutput("plotBoxB04.04", width = "1200px", height = "1300px")
      ),
      "Histogram" = list(
        tags$h4("Histogram"),
        tags$hr(),
        plotOutput("plotBoxB04.05", width = "1200px", height = "1300px")
      )
    )
  })
    #=== input update part ===#
  #### Global Setting ####
  options(scipen = 5)
  session$onSessionEnded(stopApp)
}
shinyApp(ui = ui, server = server)