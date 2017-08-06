# +++00. Required Packages and Functions -----------------------------------------
list.of.packages <- c("shiny", "ggplot2", "biwavelet", "data.table", "stringr", "ape", "DT", "shinyFiles", "shinyjs", "psych", "ggsignif", "grid")
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
                          verticalLayout(
                              shinyDirButton('uploadAnaResB01', 'Upload a analysis result', 'Please select a folder'),
                              downloadButton('downloadAnaResB01', 'Download the analysis result')
                          )
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
                        shinyDirButton('ChooseDirB03', 'Select a folder to save the results', 'Please select a folder'),
                        downloadButton('downloadStaResB03', 'Download the statistic result')
                      ),
                      #### 04. Plot Output ####
                      conditionalPanel(
                        'input.dataset2 == "Plot Output"',
                        h4("Plot Output"),
                        selectInput("levelB04", "Select the view level", 
                          c("Cell Level", "Region Level", "Moran Index", "Significance of Moran Index")
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
                          plotOutput("plotClustB03", width = "3000px", height = "800px"),
                          DT::dataTableOutput('tableWaveB03'),
                          DT::dataTableOutput('tableRegionB03'),
                          DT::dataTableOutput('tableMoranIndexB03'),
                          DT::dataTableOutput('tableMoranPB03')
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
  values<-reactiveValues()
###=== Single Sample Explore ===###
  #### 00.data manipulation ####  
  dataframe<-eventReactive(input$analyze, {
    if (is.null(input$datafile))
      return(NULL)                
    data<-read.csv(input$datafile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    ncol<-NCOL(data)
    label<-c()
    for(i in 1:ncol){
      if(i == 1){
        label[1] <- "Time"
      } else if (i == 2){
        label[i] <- "Region"
      } else
        label[i] <- str_c("Cell"," ", i-2)
    }
    colnames(data)<-label
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
    resin<-WZY.EMG.F(resin)
    resin <- resin$results
    ## Wavelet Cluster
    resclust<-waveClust()
    resclu<-as.matrix(resclust)
    resclu<-resclu[,1] #the dissimilarity of each cell to region
    fit <- hclust(resclust, method = "ward.D")
    groups <- cutree(fit, k = 2)
    ## Final combination
    resin <- data.frame(resin, Dissimilarity = resclu, Group = groups)
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
            labs(x = "Time (s)", y = "Fouresence Intensity (Gray Value)") +
            theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))
        }
      }
    }
    gp
  })# scatter plot
  plot01.02<-reactive({
    df<-waveClust()
    gp<-NULL
    if(!is.null(df)){
      gp<-plot(hclust(df, method = "ward.D"), sub = " ", main = "Wave Cluster", ylab = "Dissimilarity to reference", xlab = "", hang = -1)
    }
    gp
  })# wave clust
  #=== output part ===#
  output$plot <-renderPlot(plot1())
  output$plot01.02 <-renderPlot(plot01.02())
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
    obin01 <- dataframe()
    colna01 <- colnames(obin01)
    colna01 <- colna01[-1]
    if (input$selall01 == TRUE){
      updateSelectizeInput(session, "check01",
                           label = "Select Cells",
                           choices = colna01,
                           selected = colna01)
    }
    else if(input$selall01 == FALSE){
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
    df03.00<-dataframe()
    wt03.00<-wt(cbind(df03.00[, 1], df03.00[, input$sel03]))
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
        labs(x = "Time (s)", y = "Fouresence Intensity (Gray Value)", title = input$sel03) +
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
  #=== output part ===#
  output$plot03.01<-renderPlot(waveletSpectrum())
  output$plot03.02<-renderPlot(timeSeriesGraph())
  output$plot03.03<-renderPlot(PowerSpectrum())
  output$plot03.04<-renderPlot(waveletVariance())
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
    f04.01<-table04.01()
    if ((!is.null(input$dataSpa04)) && (!is.null(input$dataLoc04))){
      f04.01 <- cbind(f04.01[, 1], 
                      format(
                        round(as.numeric(f04.01[, 2]), 
                              digits = 4),
                        nsmall = 4
                      ),
                      format(
                        round(as.numeric(f04.01[, 3]), 
                              digits = 4),
                        nsmall = 4
                      )
      )
      colnames(f04.01) <- c("Feature", "Moran index", "P value")
      f04.01
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
  volumes <- c('Root'="/")
  shinyDirChoose(input, 'directory', roots=volumes, session = session)
  shinyDirChoose(input, 'uploadAnaResB01', roots=volumes, session = session)
  shinyDirChoose(input, 'ChooseDirB03', roots = volumes, session = session)
  values$colnames<-NULL
  values$sampleSize <- NULL
  #### 01.data input ####
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
      InUseName<-grep(paste(pat01, pat01_01, sep = ""), grep(prefix[1], fl, value = TRUE, invert = FALSE), value = TRUE, invert = FALSE)
      InUseName02<-grep(paste(pat01, pat02, sep = ""), grep(prefix[1], fl, value = TRUE, invert = FALSE), value = TRUE, invert = FALSE)
      file01<-read.csv(paste(Dir, "/", InUseName, sep = ""), header = TRUE, sep = ",")
      ncol<-NCOL(file01)
      label<-c()
      for(i in 1:ncol){
        if(i == 1){
          label[1] <- "Time (s)"
        } else if (i == 2){
          label[i] <- str_c("S", prefix[1], "Region") 
        } else
          label[i] <- str_c("S", prefix[1], "Cell", " ", i-2)
      }
      colnames(file01)<-label
      file02<-read.csv(paste(Dir, "/", InUseName02, sep=""), header = TRUE, sep = ",")
      rawBatchData<-file01
      res00<-wzy.batch(wzy = file01, loc = file02)
      rownames(res00)[rownames(res00) == "Moran Index"] <- str_c("S", prefix[1], "Moran Index")
      rownames(res00)[rownames(res00) == "P value"] <- str_c("S", prefix[1], "P value")
      ids<-c(ids, prefix[1])
      res00<-cbind(res00, id = prefix[1])
      res<-rbind(res, res00)
      prefix<-prefix[! prefix %in% prefix[1]]
      while(length(prefix) > 1){
        id<-c()
        InUseName<-grep(paste(pat01, pat01_01, sep = ""), grep(prefix[1], fl, value = TRUE, invert = FALSE), value = TRUE, invert = FALSE)
        InUseName02<-grep(paste(pat01, pat02, sep = ""), grep(prefix[1], fl, value = TRUE, invert = FALSE), value = TRUE, invert = FALSE)
        file01<-read.csv(paste(Dir, "/", InUseName, sep = ""), header = TRUE, sep = ",")
        ncol<-NCOL(file01)
        label<-c()
        for(i in 1:ncol){
          if(i == 1){
            label[1] <- "Time (s)"
          } else if (i == 2){
            label[i] <- str_c("S", prefix[1], "Region") 
          } else
            label[i] <- str_c("S", prefix[1], "Cell", " ", i-2)
        }
        colnames(file01)<-label
        file02<-read.csv(paste(Dir, "/", InUseName02, sep=""), header = TRUE, sep = ",")
        rawBatchData<-cbind(rawBatchData, file01[,-1])
        res00<-wzy.batch(wzy = file01, loc = file02)
        rownames(res00)[rownames(res00) == "Moran Index"] <- str_c("S", prefix[1], "Moran Index")
        rownames(res00)[rownames(res00) == "P value"] <- str_c("S", prefix[1], "P value")
        ids<-c(ids, prefix[1])
        res00<-cbind(res00, id = prefix[1])
        res<-rbind(res, res00)
        prefix<-prefix[! prefix %in% prefix[1]]
      }
    })
    isolate(values$sampleSize <- as.numeric(length(ids)))
    Row_names<-rownames(res)
    rownames(res)<-NULL
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
      write.csv(values$tableB01.02, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_3.csv", sep = ""), row.names = FALSE)
      write.csv(values$tableB01.03, file = paste(parseDirPath(volumes, input$directory), "/AnalysisResults/Table_4.csv", sep = ""), row.names = FALSE)
    }
  )
  #=== input update part ===#
  values$tableB01.00<-data.frame("Table 2"=NA)
  values$tableB01.01<-data.frame("Table 2"=NA)
  values$tableB01.02<-data.frame("Table 3"=NA)
  values$tableB01.03<-data.frame("Table 4"=NA)
  observe({
    if(is.null(input$uploadAnaResB01)){
      tableB01.00<-fileList()
      No.<-c(1:length(tableB01.00))
      tableB01.00<-cbind(No., data = tableB01.00)
    } else {
      tableB01.00<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_1.csv", sep = ""), header=TRUE, sep=",")
    }
    isolate(values$tableB01.00<-tableB01.00)
  }) #TableB01.00 | Table 1
  observe({
    if(is.null(input$uploadAnaResB01)){
      res<-resB01()
      tableB01.01<-as.data.frame((res[1]))
    } else {
      tableB01.01<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_2.csv", sep = ""), header=TRUE, sep=",")
    }
    isolate(values$tableB01.01<-tableB01.01)
  }) #TableB01.01 | Table 2
  observe({
    if(is.null(input$uploadAnaResB01)){
      res<-resB01()
      tableB01.02<-as.data.frame((res[3]))
    } else {
      tableB01.02<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_3.csv", sep = ""), header=FALSE, sep=",")
      isolate(values$colnames <- as.vector(unlist(tableB01.02[1,])))
      tableB01.02<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_3.csv", sep = ""), header=TRUE, sep=",")
    }
    isolate(values$tableB01.02<-tableB01.02)
  }) #TableB01.02 | Table 3
  observe({
    if(is.null(input$uploadAnaResB01)){
      res<-resB01()
      tableB01.03<-as.vector(unlist(res[2]))
      tableB01.03<-data.frame(ID = tableB01.03, Group = NA, Label = NA)
    } else {
      tableB01.03<-read.csv(paste(parseDirPath(volumes, input$uploadAnaResB01), "/Table_4.csv", sep = ""), header=TRUE, sep=",")
    }
    isolate(values$sampleSize <- length(tableB01.03[, 1]))
    isolate(values$tableB01.03<-tableB01.03)
  }) #TableB01.03 | Table 4
    #=== 01.end ===#
  #### 02.Statistical Analysis ####
  #=== manipulation part ===#
  values$wavefeatureB02 <- data.frame("NoData" = NA)
  values$distributionB02 <- data.frame("NoData" = NA)
  values$MoranPB02 <- data.frame("NoData" = NA)
  values$regionB02 <- data.frame("NoData" =NA)
  values$GlobalClustB02 <- NULL
  observeEvent(input$staB02, {
    if(is.null(values$tableB01.01)) {
      return(NULL)
    }
    isolate(values$tableB01.01$id <- as.character(values$tableB01.01$id))
    isolate(values$tableB01.01$Row_names <- as.character(values$tableB01.01$Row_names))
    isolate(colnames(values$tableB01.02) <- values$colnames)
    tag<-c()
    for(i in 1:dim(values$tableB02)[1]) {
      tag<-c(tag, as.vector(rep(values$tableB02$Label[i], length(values$tableB01.01$id[values$tableB01.01$id == values$tableB02$ID[i]]))))
      isolate(values$tableB01.01$id[values$tableB01.01$id == values$tableB02$ID[i]] <- as.character(values$tableB02$Group[i]))
      isolate(colnames(values$tableB01.02) <- str_replace_all(colnames(values$tableB01.02), pattern = str_c("S", as.character(values$tableB02$ID[i]), sep = ""), str_c("G", as.character(values$tableB02$Group[i]), "_",                                                                                                                                                                    as.character(values$tableB02$ID[i]), sep = "")))
    }
    isolate(colnames(values$tableB01.01)[colnames(values$tableB01.01) == "id"] <- "Label")
    isolate(values$tableB01.01 <- cbind(values$tableB01.01, Tag = tag))
    isolate(rownames(values$tableB01.01)<-values$tableB01.01$Row_names)
    withBusyIndicatorServer("staB02", {
      GlobalClustB02<-WZY.Wavelet.clust2(values$tableB01.02)
    })
    fit <- hclust(GlobalClustB02, method = "ward.D")
    groups <- cutree(fit, k = 2)
    isolate(values$distributionB02 <- values$tableB01.01[grep("Moran Index", values$tableB01.01$Row_names, value = TRUE), ])
    isolate(values$distributionB02 <- values$distributionB02[order(values$distributionB02$Label), ])
    isolate(values$tableB01.01 <- values$tableB01.01[! rownames(values$tableB01.01) %in% grep("Moran Index", rownames(values$tableB01.01), value = TRUE), ])
    isolate(values$MoranPB02 <- values$tableB01.01[grep("P value", values$tableB01.01$Row_names, value = TRUE), ])
    isolate(values$MoranPB02 <- values$MoranPB02[order(values$MoranPB02$Label),])
    isolate(values$tableB01.01 <- values$tableB01.01[! rownames(values$tableB01.01) %in% grep("P value", rownames(values$tableB01.01), value = TRUE), ])
    isolate(values$GlobalClustB02 <- GlobalClustB02)
    isolate(values$tableB01.01 <- cbind(values$tableB01.01, Global_Group = groups))
    isolate(values$wavefeatureB02 <- values$tableB01.01[grep("Cell", values$tableB01.01$Row_names, value = TRUE), ])
    isolate(values$wavefeatureB02 <- values$wavefeatureB02[order(values$wavefeatureB02$Label),])
    isolate(values$regionB02 <- values$tableB01.01[grep("Region", values$tableB01.01$Row_names, value = TRUE), ])
    isolate(values$regionB02 <- values$regionB02[order(values$regionB02$Label),])
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
  SummaryWaveB03 <- eventReactive(input$staB02, {
    P.value <- c()
    Statistic <- c()
    for(i in 2:10){
      test <- wilcox.test(values$wavefeatureB02[, i] ~ values$wavefeatureB02$Label)
      P.value <-c(P.value, test$p.value) 
      Statistic <- c(Statistic, test$statistic)
    }
    group.n <- as.numeric(max(values$tableB02$Group))+1
    summary <- describeBy(values$wavefeatureB02[, 2:10], values$wavefeatureB02$Tag)
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
    for(i in 2:9){
      test <- wilcox.test(values$regionB02[, i] ~ values$regionB02$Label)
      P.value <-c(P.value, test$p.value) 
      Statistic <- c(Statistic, test$statistic)
    }
    group.n <- as.numeric(max(values$tableB02$Group))+1
    summary <- describeBy(values$regionB02[, 2:9], values$regionB02$Tag)
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
    summary <- describeBy(values$distributionB02[, 2:11], values$distributionB02$Tag)
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
    for(i in 2:11){
      test <- wilcox.test(values$MoranPB02[, i] ~ values$MoranPB02$Label)
      P.value <-c(P.value, test$p.value) 
      Statistic <- c(Statistic, test$statistic)
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
  plotClustB03 <- reactive({
    gp<-NULL
    GlobalClustB02 <- values$GlobalClustB02
    gp<-plot(hclust(GlobalClustB02, method = "ward.D"), sub = " ", main = "Wave Cluster", ylab = "Dissimilarity to reference", xlab = "", hang = -1)
    gp
  })
    #=== output part ===#
  output$downloadStaResB03 <- downloadHandler(
    filename = "Result_1.csv",
    content = function(file) {
      dir.create(paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/", sep = ""), showWarnings = FALSE)
      write.csv(values$wavefeatureB02, file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/SplitData_1.csv", sep = ""), row.names = FALSE)
      write.csv(values$regionB02, file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/SplitData_2.csv", sep = ""), row.names = FALSE)
      write.csv(values$distributionB02, file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/SplitData_3.csv", sep = ""), row.names = FALSE)
      write.csv(values$MoranPB02, file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/SplitData_4.csv", sep = ""), row.names = FALSE)
      write.csv(SummaryWaveB03(), file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/Result_1.csv", sep = ""), row.names = FALSE)
      write.csv(SummaryRegionB03(), file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/Result_2.csv", sep = ""), row.names = FALSE)
      write.csv(SummaryMoranIndexB03(), file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/Result_3.csv", sep = ""), row.names = FALSE)
      write.csv(SummaryMoranPB03(), file = paste(parseDirPath(volumes, input$ChooseDirB03), "/Statistic_Results/Result_4.csv", sep = ""), row.names = FALSE)
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
    #=== 03.end ===#
  #### 04. Plot Output ####
    #=== manipulation part ===#
  plotBoxB04.00<-reactive({
    # Figure_1 
    anno1 <- if(SummaryWaveB03()["P.value", 1]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 1]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 1]<0.05 && SummaryWaveB03()["P.value", 1]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 1]<0.01 && SummaryWaveB03()["P.value", 1]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 1]<0.001 && SummaryWaveB03()["P.value", 1]>0.0001){
      "***"
    } else {"****"}
    g1 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 2])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno1, digits=2),
                  y_position=max(values$wavefeatureB02[, 2])*1.15, xmin=1, xmax=2, textsize = 7, 
                  tip_length = c(1-(max(values$wavefeatureB02[, 2][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 2])*1.12)), 
                                 1-(max(values$wavefeatureB02[, 2][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 2])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Integrated" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 2])*1.23)
    # Figure_2 
    anno2 <- if(SummaryWaveB03()["P.value", 2]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 2]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 2]<0.05 && SummaryWaveB03()["P.value", 2]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 2]<0.01 && SummaryWaveB03()["P.value", 2]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 2]<0.001 && SummaryWaveB03()["P.value", 2]>0.0001){
      "***"
    } else {"****"}
    g2 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 3])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno2, digits=2),
                  y_position=max(values$wavefeatureB02[, 3])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$wavefeatureB02[, 3][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 3])*1.12)), 
                                 1-(max(values$wavefeatureB02[, 3][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 3])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Absolute Value" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 3])*1.23)
    # Figure_3
    anno3 <- if(SummaryWaveB03()["P.value", 3]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 3]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 3]<0.05 && SummaryWaveB03()["P.value", 3]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 3]<0.01 && SummaryWaveB03()["P.value", 3]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 3]<0.001 && SummaryWaveB03()["P.value", 3]>0.0001){
      "***"
    } else {"****"}
    g3 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 4])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno3, digits=2),
                  y_position=max(values$wavefeatureB02[, 4])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$wavefeatureB02[, 4][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 4])*1.12)), 
                                 1-(max(values$wavefeatureB02[, 4][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 4])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Variance" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 4])*1.23)
    # Figure_4
    anno4 <- if(SummaryWaveB03()["P.value", 4]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 4]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 4]<0.05 && SummaryWaveB03()["P.value", 4]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 4]<0.01 && SummaryWaveB03()["P.value", 4]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 4]<0.001 && SummaryWaveB03()["P.value", 4]>0.0001){
      "***"
    } else {"****"}
    g4 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 5])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno4, digits=2),
                  y_position=max(values$wavefeatureB02[, 5])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$wavefeatureB02[, 5][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 5])*1.12)), 
                                 1-(max(values$wavefeatureB02[, 5][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 5])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Root Mean Square" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 5])*1.23)
    # Figure_5
    anno5 <- if(SummaryWaveB03()["P.value", 5]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 5]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 5]<0.05 && SummaryWaveB03()["P.value", 5]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 5]<0.01 && SummaryWaveB03()["P.value", 5]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 5]<0.001 && SummaryWaveB03()["P.value", 5]>0.0001){
      "***"
    } else {"****"}
    g5 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 6])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno5, digits=2),
                  y_position=max(values$wavefeatureB02[, 6])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$wavefeatureB02[, 6][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 6])*1.12)), 
                                 1-(max(values$wavefeatureB02[, 6][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 6])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Waveform Length" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 6])*1.23)
    # Figure_6 
    anno6 <- if(SummaryWaveB03()["P.value", 6]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 6]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 6]<0.05 && SummaryWaveB03()["P.value", 6]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 6]<0.01 && SummaryWaveB03()["P.value", 6]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 6]<0.001 && SummaryWaveB03()["P.value", 6]>0.0001){
      "***"
    } else {"****"}
    g6 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 7])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno6, digits=2),
                  y_position=max(values$wavefeatureB02[, 7])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$wavefeatureB02[, 7][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 7])*1.12)), 
                                 1-(max(values$wavefeatureB02[, 7][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 7])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Main Period" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 7])*1.23)
    # Figure_7
    anno7 <- if(SummaryWaveB03()["P.value", 7]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 7]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 7]<0.05 && SummaryWaveB03()["P.value", 7]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 7]<0.01 && SummaryWaveB03()["P.value", 7]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 7]<0.001 && SummaryWaveB03()["P.value", 7]>0.0001){
      "***"
    } else {"****"}
    g7 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 8])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno7, digits=2),
                  y_position=max(values$wavefeatureB02[, 8])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$wavefeatureB02[, 8][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 8])*1.12)), 
                                 1-(max(values$wavefeatureB02[, 8][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 8])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Maximal Amplitude" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 8])*1.23)
    # Figure_8
    anno8 <- if(SummaryWaveB03()["P.value", 8]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 8]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 8]<0.05 && SummaryWaveB03()["P.value", 8]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 8]<0.01 && SummaryWaveB03()["P.value", 8]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 8]<0.001 && SummaryWaveB03()["P.value", 8]>0.0001){
      "***"
    } else {"****"}
    g8 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 9])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno8, digits=2),
                  y_position=max(values$wavefeatureB02[, 9])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$wavefeatureB02[, 9][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 9])*1.12)), 
                                                          1-(max(values$wavefeatureB02[, 9][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 9])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 9])*1.23)
    # Figure_9 
    anno9 <- if(SummaryWaveB03()["P.value", 9]>0.05){
      paste("P>", floor(SummaryWaveB03()["P.value", 9]*100)/100, sep = "") 
    } else if(SummaryWaveB03()["P.value", 9]<0.05 && SummaryWaveB03()["P.value", 9]>0.01){
      "*"
    } else if(SummaryWaveB03()["P.value", 9]<0.01 && SummaryWaveB03()["P.value", 9]>0.001){
      "**"
    }else if(SummaryWaveB03()["P.value", 9]<0.001 && SummaryWaveB03()["P.value", 9]>0.0001){
      "***"
    } else {"****"}
    g9 <- ggplot(values$wavefeatureB02, aes(x=values$wavefeatureB02$Tag, y=values$wavefeatureB02[, 10])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=anno9,
                  y_position=max(values$wavefeatureB02[, 10])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length =  c(1-(max(values$wavefeatureB02[, 10][values$wavefeatureB02$Label == 0])/(max(values$wavefeatureB02[, 10])*1.12)), 
                                  1-(max(values$wavefeatureB02[, 10][values$wavefeatureB02$Label == 1])/(max(values$wavefeatureB02[, 10])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Dissimilarity to Region" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$wavefeatureB02[, 10])*1.23)
   
    multiplot.wzy(g1, g2, g3, g4, g5, g6, g7, g8, g9, cols=3)
  })
  plotBoxB04.01<-reactive({
    # Figure_1 
    anno1 <- if(SummaryRegionB03()["P.value", 1]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 1]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 1]<0.05 && SummaryRegionB03()["P.value", 1]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 1]<0.01 && SummaryRegionB03()["P.value", 1]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 1]<0.001 && SummaryRegionB03()["P.value", 1]>0.0001){
      "***"
    } else {"****"}
    g1 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 2])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno1, digits=2),
                  y_position=max(values$regionB02[, 2])*1.15, xmin=1, xmax=2, textsize = 7, 
                  tip_length = c(1-(max(values$regionB02[, 2][values$regionB02$Label == 0])/(max(values$regionB02[, 2])*1.12)), 
                                 1-(max(values$regionB02[, 2][values$regionB02$Label == 1])/(max(values$regionB02[, 2])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Integrated" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 2])*1.23)
    # Figure_2 
    anno2 <- if(SummaryRegionB03()["P.value", 2]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 2]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 2]<0.05 && SummaryRegionB03()["P.value", 2]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 2]<0.01 && SummaryRegionB03()["P.value", 2]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 2]<0.001 && SummaryRegionB03()["P.value", 2]>0.0001){
      "***"
    } else {"****"}
    g2 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 3])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno2, digits=2),
                  y_position=max(values$regionB02[, 3])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$regionB02[, 3][values$regionB02$Label == 0])/(max(values$regionB02[, 3])*1.12)), 
                                 1-(max(values$regionB02[, 3][values$regionB02$Label == 1])/(max(values$regionB02[, 3])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Absolute Value" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 3])*1.23)
    # Figure_3
    anno3 <- if(SummaryRegionB03()["P.value", 3]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 3]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 3]<0.05 && SummaryRegionB03()["P.value", 3]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 3]<0.01 && SummaryRegionB03()["P.value", 3]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 3]<0.001 && SummaryRegionB03()["P.value", 3]>0.0001){
      "***"
    } else {"****"}
    g3 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 4])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno3, digits=2),
                  y_position=max(values$regionB02[, 4])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$regionB02[, 4][values$regionB02$Label == 0])/(max(values$regionB02[, 4])*1.12)), 
                                 1-(max(values$regionB02[, 4][values$regionB02$Label == 1])/(max(values$regionB02[, 4])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Variance" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 4])*1.23)
    # Figure_4
    anno4 <- if(SummaryRegionB03()["P.value", 4]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 4]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 4]<0.05 && SummaryRegionB03()["P.value", 4]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 4]<0.01 && SummaryRegionB03()["P.value", 4]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 4]<0.001 && SummaryRegionB03()["P.value", 4]>0.0001){
      "***"
    } else {"****"}
    g4 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 5])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno4, digits=2),
                  y_position=max(values$regionB02[, 5])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$regionB02[, 5][values$regionB02$Label == 0])/(max(values$regionB02[, 5])*1.12)), 
                                 1-(max(values$regionB02[, 5][values$regionB02$Label == 1])/(max(values$regionB02[, 5])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Root Mean Square" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 5])*1.23)
    # Figure_5
    anno5 <- if(SummaryRegionB03()["P.value", 5]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 5]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 5]<0.05 && SummaryRegionB03()["P.value", 5]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 5]<0.01 && SummaryRegionB03()["P.value", 5]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 5]<0.001 && SummaryRegionB03()["P.value", 5]>0.0001){
      "***"
    } else {"****"}
    g5 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 6])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno5, digits=2),
                  y_position=max(values$regionB02[, 6])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$regionB02[, 6][values$regionB02$Label == 0])/(max(values$regionB02[, 6])*1.12)), 
                                 1-(max(values$regionB02[, 6][values$regionB02$Label == 1])/(max(values$regionB02[, 6])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Waveform Length" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 6])*1.23)
    # Figure_6 
    anno6 <- if(SummaryRegionB03()["P.value", 6]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 6]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 6]<0.05 && SummaryRegionB03()["P.value", 6]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 6]<0.01 && SummaryRegionB03()["P.value", 6]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 6]<0.001 && SummaryRegionB03()["P.value", 6]>0.0001){
      "***"
    } else {"****"}
    g6 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 7])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno6, digits=2),
                  y_position=max(values$regionB02[, 7])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$regionB02[, 7][values$regionB02$Label == 0])/(max(values$regionB02[, 7])*1.12)), 
                                 1-(max(values$regionB02[, 7][values$regionB02$Label == 1])/(max(values$regionB02[, 7])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Main Period" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 7])*1.23)
    # Figure_7
    anno7 <- if(SummaryRegionB03()["P.value", 7]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 7]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 7]<0.05 && SummaryRegionB03()["P.value", 7]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 7]<0.01 && SummaryRegionB03()["P.value", 7]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 7]<0.001 && SummaryRegionB03()["P.value", 7]>0.0001){
      "***"
    } else {"****"}
    g7 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 8])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno7, digits=2),
                  y_position=max(values$regionB02[, 8])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$regionB02[, 8][values$regionB02$Label == 0])/(max(values$regionB02[, 8])*1.12)), 
                                 1-(max(values$regionB02[, 8][values$regionB02$Label == 1])/(max(values$regionB02[, 8])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Maximal Amplitude" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 8])*1.23)
    # Figure_8
    anno8 <- if(SummaryRegionB03()["P.value", 8]>0.05){
      paste("P>", floor(SummaryRegionB03()["P.value", 8]*100)/100, sep = "") 
    } else if(SummaryRegionB03()["P.value", 8]<0.05 && SummaryRegionB03()["P.value", 8]>0.01){
      "*"
    } else if(SummaryRegionB03()["P.value", 8]<0.01 && SummaryRegionB03()["P.value", 8]>0.001){
      "**"
    }else if(SummaryRegionB03()["P.value", 8]<0.001 && SummaryRegionB03()["P.value", 8]>0.0001){
      "***"
    } else {"****"}
    g8 <- ggplot(values$regionB02, aes(x=values$regionB02$Tag, y=values$regionB02[, 9])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno8, digits=2),
                  y_position=max(values$regionB02[, 9])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$regionB02[, 9][values$regionB02$Label == 0])/(max(values$regionB02[, 9])*1.12)), 
                                                          1-(max(values$regionB02[, 9][values$regionB02$Label == 1])/(max(values$regionB02[, 9])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$regionB02[, 9])*1.23)
    
    multiplot.wzy(g1, g2, g3, g4, g5, g6, g7, g8, cols=3)
  })
  plotBoxB04.02<-reactive({
    # Figure_1 
    anno1 <- if(SummaryMoranIndexB03()["P.value", 1]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 1]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 1]<0.05 && SummaryMoranIndexB03()["P.value", 1]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 1]<0.01 && SummaryMoranIndexB03()["P.value", 1]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 1]<0.001 && SummaryMoranIndexB03()["P.value", 1]>0.0001){
      "***"
    } else {"****"}
    g1 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 2])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno1, digits=2),
                  y_position=max(values$distributionB02[, 2])*1.15, xmin=1, xmax=2, textsize = 7, 
                  tip_length = c(1-(max(values$distributionB02[, 2][values$distributionB02$Label == 0])/(max(values$distributionB02[, 2])*1.12)), 
                                 1-(max(values$distributionB02[, 2][values$distributionB02$Label == 1])/(max(values$distributionB02[, 2])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Integrated" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 2])*1.23)
    # Figure_2 
    anno2 <- if(SummaryMoranIndexB03()["P.value", 2]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 2]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 2]<0.05 && SummaryMoranIndexB03()["P.value", 2]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 2]<0.01 && SummaryMoranIndexB03()["P.value", 2]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 2]<0.001 && SummaryMoranIndexB03()["P.value", 2]>0.0001){
      "***"
    } else {"****"}
    g2 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 3])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno2, digits=2),
                  y_position=max(values$distributionB02[, 3])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$distributionB02[, 3][values$distributionB02$Label == 0])/(max(values$distributionB02[, 3])*1.12)), 
                                 1-(max(values$distributionB02[, 3][values$distributionB02$Label == 1])/(max(values$distributionB02[, 3])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Absolute Value" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 3])*1.23)
    # Figure_3
    anno3 <- if(SummaryMoranIndexB03()["P.value", 3]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 3]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 3]<0.05 && SummaryMoranIndexB03()["P.value", 3]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 3]<0.01 && SummaryMoranIndexB03()["P.value", 3]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 3]<0.001 && SummaryMoranIndexB03()["P.value", 3]>0.0001){
      "***"
    } else {"****"}
    g3 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 4])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno3, digits=2),
                  y_position=max(values$distributionB02[, 4])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$distributionB02[, 4][values$distributionB02$Label == 0])/(max(values$distributionB02[, 4])*1.12)), 
                                 1-(max(values$distributionB02[, 4][values$distributionB02$Label == 1])/(max(values$distributionB02[, 4])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Variance" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 4])*1.23)
    # Figure_4
    anno4 <- if(SummaryMoranIndexB03()["P.value", 4]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 4]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 4]<0.05 && SummaryMoranIndexB03()["P.value", 4]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 4]<0.01 && SummaryMoranIndexB03()["P.value", 4]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 4]<0.001 && SummaryMoranIndexB03()["P.value", 4]>0.0001){
      "***"
    } else {"****"}
    g4 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 5])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno4, digits=2),
                  y_position=max(values$distributionB02[, 5])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$distributionB02[, 5][values$distributionB02$Label == 0])/(max(values$distributionB02[, 5])*1.12)), 
                                 1-(max(values$distributionB02[, 5][values$distributionB02$Label == 1])/(max(values$distributionB02[, 5])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Root Mean Square" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 5])*1.23)
    # Figure_5
    anno5 <- if(SummaryMoranIndexB03()["P.value", 5]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 5]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 5]<0.05 && SummaryMoranIndexB03()["P.value", 5]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 5]<0.01 && SummaryMoranIndexB03()["P.value", 5]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 5]<0.001 && SummaryMoranIndexB03()["P.value", 5]>0.0001){
      "***"
    } else {"****"}
    g5 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 6])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno5, digits=2),
                  y_position=max(values$distributionB02[, 6])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$distributionB02[, 6][values$distributionB02$Label == 0])/(max(values$distributionB02[, 6])*1.12)), 
                                 1-(max(values$distributionB02[, 6][values$distributionB02$Label == 1])/(max(values$distributionB02[, 6])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Waveform Length" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 6])*1.23)
    # Figure_6 
    anno6 <- if(SummaryMoranIndexB03()["P.value", 6]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 6]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 6]<0.05 && SummaryMoranIndexB03()["P.value", 6]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 6]<0.01 && SummaryMoranIndexB03()["P.value", 6]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 6]<0.001 && SummaryMoranIndexB03()["P.value", 6]>0.0001){
      "***"
    } else {"****"}
    g6 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 7])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno6, digits=2),
                  y_position=max(values$distributionB02[, 7])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$distributionB02[, 7][values$distributionB02$Label == 0])/(max(values$distributionB02[, 7])*1.12)), 
                                 1-(max(values$distributionB02[, 7][values$distributionB02$Label == 1])/(max(values$distributionB02[, 7])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Main Period" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 7])*1.23)
    # Figure_7
    anno7 <- if(SummaryMoranIndexB03()["P.value", 7]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 7]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 7]<0.05 && SummaryMoranIndexB03()["P.value", 7]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 7]<0.01 && SummaryMoranIndexB03()["P.value", 7]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 7]<0.001 && SummaryMoranIndexB03()["P.value", 7]>0.0001){
      "***"
    } else {"****"}
    g7 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 8])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno7, digits=2),
                  y_position=max(values$distributionB02[, 8])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$distributionB02[, 8][values$distributionB02$Label == 0])/(max(values$distributionB02[, 8])*1.12)), 
                                 1-(max(values$distributionB02[, 8][values$distributionB02$Label == 1])/(max(values$distributionB02[, 8])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Maximal Amplitude" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 8])*1.23)
    # Figure_8
    anno8 <- if(SummaryMoranIndexB03()["P.value", 8]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 8]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 8]<0.05 && SummaryMoranIndexB03()["P.value", 8]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 8]<0.01 && SummaryMoranIndexB03()["P.value", 8]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 8]<0.001 && SummaryMoranIndexB03()["P.value", 8]>0.0001){
      "***"
    } else {"****"}
    g8 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 9])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno8, digits=2),
                  y_position=max(values$distributionB02[, 9])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$distributionB02[, 9][values$distributionB02$Label == 0])/(max(values$distributionB02[, 9])*1.12)), 
                                                          1-(max(values$distributionB02[, 9][values$distributionB02$Label == 1])/(max(values$distributionB02[, 9])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 9])*1.23)
    # Figure_9
    anno9 <- if(SummaryMoranIndexB03()["P.value", 9]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 9]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 9]<0.05 && SummaryMoranIndexB03()["P.value", 9]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 9]<0.01 && SummaryMoranIndexB03()["P.value", 9]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 9]<0.001 && SummaryMoranIndexB03()["P.value", 9]>0.0001){
      "***"
    } else {"****"}
    g9 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 10])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno9, digits=2),
                  y_position=max(values$distributionB02[, 10])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$distributionB02[, 10][values$distributionB02$Label == 0])/(max(values$distributionB02[, 10])*1.12)), 
                                                          1-(max(values$distributionB02[, 10][values$distributionB02$Label == 1])/(max(values$distributionB02[, 10])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 10])*1.23)
    # Figure_10
    anno10 <- if(SummaryMoranIndexB03()["P.value", 10]>0.05){
      paste("P>", floor(SummaryMoranIndexB03()["P.value", 8]*100)/100, sep = "") 
    } else if(SummaryMoranIndexB03()["P.value", 10]<0.05 && SummaryMoranIndexB03()["P.value", 10]>0.01){
      "*"
    } else if(SummaryMoranIndexB03()["P.value", 10]<0.01 && SummaryMoranIndexB03()["P.value", 10]>0.001){
      "**"
    }else if(SummaryMoranIndexB03()["P.value", 10]<0.001 && SummaryMoranIndexB03()["P.value", 10]>0.0001){
      "***"
    } else {"****"}
    g10 <- ggplot(values$distributionB02, aes(x=values$distributionB02$Tag, y=values$distributionB02[, 11])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno10, digits=2),
                  y_position=max(values$distributionB02[, 11])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$distributionB02[, 11][values$distributionB02$Label == 0])/(max(values$distributionB02[, 11])*1.12)), 
                                                          1-(max(values$distributionB02[, 11][values$distributionB02$Label == 1])/(max(values$distributionB02[, 11])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$distributionB02[, 11])*1.23)
    
    multiplot.wzy(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, cols=3)
  })
  plotBoxB04.03<-reactive({
    # Figure_1 
    anno1 <- if(SummaryMoranPB03()["P.value", 1]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 1]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 1]<0.05 && SummaryMoranPB03()["P.value", 1]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 1]<0.01 && SummaryMoranPB03()["P.value", 1]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 1]<0.001 && SummaryMoranPB03()["P.value", 1]>0.0001){
      "***"
    } else {"****"}
    g1 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 2])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno1, digits=2),
                  y_position=max(values$MoranPB02[, 2])*1.15, xmin=1, xmax=2, textsize = 7, 
                  tip_length = c(1-(max(values$MoranPB02[, 2][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 2])*1.12)), 
                                 1-(max(values$MoranPB02[, 2][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 2])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Integrated" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 2])*1.23)
    # Figure_2 
    anno2 <- if(SummaryMoranPB03()["P.value", 2]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 2]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 2]<0.05 && SummaryMoranPB03()["P.value", 2]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 2]<0.01 && SummaryMoranPB03()["P.value", 2]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 2]<0.001 && SummaryMoranPB03()["P.value", 2]>0.0001){
      "***"
    } else {"****"}
    g2 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 3])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno2, digits=2),
                  y_position=max(values$MoranPB02[, 3])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$MoranPB02[, 3][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 3])*1.12)), 
                                 1-(max(values$MoranPB02[, 3][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 3])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Absolute Value" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 3])*1.23)
    # Figure_3
    anno3 <- if(SummaryMoranPB03()["P.value", 3]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 3]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 3]<0.05 && SummaryMoranPB03()["P.value", 3]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 3]<0.01 && SummaryMoranPB03()["P.value", 3]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 3]<0.001 && SummaryMoranPB03()["P.value", 3]>0.0001){
      "***"
    } else {"****"}
    g3 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 4])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno3, digits=2),
                  y_position=max(values$MoranPB02[, 4])*1.16, xmin=1, xmax=2, textsize = 7,
                  tip_length = c(1-(max(values$MoranPB02[, 4][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 4])*1.12)), 
                                 1-(max(values$MoranPB02[, 4][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 4])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Variance" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 4])*1.23)
    # Figure_4
    anno4 <- if(SummaryMoranPB03()["P.value", 4]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 4]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 4]<0.05 && SummaryMoranPB03()["P.value", 4]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 4]<0.01 && SummaryMoranPB03()["P.value", 4]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 4]<0.001 && SummaryMoranPB03()["P.value", 4]>0.0001){
      "***"
    } else {"****"}
    g4 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 5])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno4, digits=2),
                  y_position=max(values$MoranPB02[, 5])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$MoranPB02[, 5][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 5])*1.12)), 
                                 1-(max(values$MoranPB02[, 5][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 5])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Root Mean Square" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 5])*1.23)
    # Figure_5
    anno5 <- if(SummaryMoranPB03()["P.value", 5]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 5]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 5]<0.05 && SummaryMoranPB03()["P.value", 5]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 5]<0.01 && SummaryMoranPB03()["P.value", 5]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 5]<0.001 && SummaryMoranPB03()["P.value", 5]>0.0001){
      "***"
    } else {"****"}
    g5 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 6])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno5, digits=2),
                  y_position=max(values$MoranPB02[, 6])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$MoranPB02[, 6][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 6])*1.12)), 
                                 1-(max(values$MoranPB02[, 6][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 6])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Waveform Length" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 6])*1.23)
    # Figure_6 
    anno6 <- if(SummaryMoranPB03()["P.value", 6]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 6]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 6]<0.05 && SummaryMoranPB03()["P.value", 6]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 6]<0.01 && SummaryMoranPB03()["P.value", 6]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 6]<0.001 && SummaryMoranPB03()["P.value", 6]>0.0001){
      "***"
    } else {"****"}
    g6 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 7])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno6, digits=2),
                  y_position=max(values$MoranPB02[, 7])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$MoranPB02[, 7][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 7])*1.12)), 
                                 1-(max(values$MoranPB02[, 7][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 7])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Main Period" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 7])*1.23)
    # Figure_7
    anno7 <- if(SummaryMoranPB03()["P.value", 7]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 7]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 7]<0.05 && SummaryMoranPB03()["P.value", 7]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 7]<0.01 && SummaryMoranPB03()["P.value", 7]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 7]<0.001 && SummaryMoranPB03()["P.value", 7]>0.0001){
      "***"
    } else {"****"}
    g7 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 8])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno7, digits=2),
                  y_position=max(values$MoranPB02[, 8])*1.16, xmin=1, xmax=2, 
                  map_signif_level = TRUE, textsize = 7,
                  tip_length = c(1-(max(values$MoranPB02[, 8][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 8])*1.12)), 
                                 1-(max(values$MoranPB02[, 8][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 8])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Maximal Amplitude" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 8])*1.23)
    # Figure_8
    anno8 <- if(SummaryMoranPB03()["P.value", 8]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 8]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 8]<0.05 && SummaryMoranPB03()["P.value", 8]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 8]<0.01 && SummaryMoranPB03()["P.value", 8]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 8]<0.001 && SummaryMoranPB03()["P.value", 8]>0.0001){
      "***"
    } else {"****"}
    g8 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 9])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno8, digits=2),
                  y_position=max(values$MoranPB02[, 9])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$MoranPB02[, 9][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 9])*1.12)), 
                                                          1-(max(values$MoranPB02[, 9][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 9])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 9])*1.23)
    # Figure_9
    anno9 <- if(SummaryMoranPB03()["P.value", 9]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 9]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 9]<0.05 && SummaryMoranPB03()["P.value", 9]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 9]<0.01 && SummaryMoranPB03()["P.value", 9]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 9]<0.001 && SummaryMoranPB03()["P.value", 9]>0.0001){
      "***"
    } else {"****"}
    g9 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 10])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno9, digits=2),
                  y_position=max(values$MoranPB02[, 10])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$MoranPB02[, 10][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 10])*1.12)), 
                                                          1-(max(values$MoranPB02[, 10][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 10])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 10])*1.23)
    # Figure_10
    anno10 <- if(SummaryMoranPB03()["P.value", 10]>0.05){
      paste("P>", floor(SummaryMoranPB03()["P.value", 8]*100)/100, sep = "") 
    } else if(SummaryMoranPB03()["P.value", 10]<0.05 && SummaryMoranPB03()["P.value", 10]>0.01){
      "*"
    } else if(SummaryMoranPB03()["P.value", 10]<0.01 && SummaryMoranPB03()["P.value", 10]>0.001){
      "**"
    }else if(SummaryMoranPB03()["P.value", 10]<0.001 && SummaryMoranPB03()["P.value", 10]>0.0001){
      "***"
    } else {"****"}
    g10 <- ggplot(values$MoranPB02, aes(x=values$MoranPB02$Tag, y=values$MoranPB02[, 11])) +
      geom_boxplot(position="dodge") +
      geom_signif(annotation=formatC(anno10, digits=2),
                  y_position=max(values$MoranPB02[, 11])*1.16, xmin=1, xmax=2, textsize = 7,
                  map_signif_level = TRUE, tip_length = c(1-(max(values$MoranPB02[, 11][values$MoranPB02$Label == 0])/(max(values$MoranPB02[, 11])*1.12)), 
                                                          1-(max(values$MoranPB02[, 11][values$MoranPB02$Label == 1])/(max(values$MoranPB02[, 11])*1.12))))+
      labs(y = "Arbitrary Unit", title = "Mean Power Frequency" ) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
            axis.title.x=element_blank(), axis.text=element_text(size=14), 
            axis.title=element_text(size=14,face="bold")) + ylim(NA, max(values$MoranPB02[, 11])*1.23)
    
    multiplot.wzy(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, cols=3)
  })
    #=== output part ===#
  output$plotBoxB04.00<-renderPlot(plotBoxB04.00())
  output$plotBoxB04.01<-renderPlot(plotBoxB04.01())
  output$plotBoxB04.02<-renderPlot(plotBoxB04.02())
  output$plotBoxB04.03<-renderPlot(plotBoxB04.03())
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
        tags$h4(" P value of Moran index for each wave feature"),
        tags$hr(),
        plotOutput("plotBoxB04.03", width = "800px", height = "1067px")
      )
    )
  })
    #=== input update part ===#
  #### Global Setting ####
  options(scipen = 5)
  session$onSessionEnded(stopApp)
}
shinyApp(ui = ui, server = server)