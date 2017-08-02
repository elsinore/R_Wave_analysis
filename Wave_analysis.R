#### Required Packages and Functions ####
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


#### Service Function ####
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
      if (is.null(input$datafile)) {
        stop("choose another option")
      }
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
    resclu<-resclu[,1]
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
      res04t<-res04t[-1,]
      ozone<-ozone[-1,]
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
  volumes <- c('Root'="/Users/HSBR")
  shinyDirChoose(input, 'directory', roots=volumes, session=session)
  #### 01.data input ####
  #=== manipulation part ===#
  resB01<-eventReactive(input$anB01, {
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
      file02<-read.csv(paste(Dir, "/", InUseName02, sep=""), header = TRUE, sep = ",")
      rawBatchData<-file01
      res00<-wzy.batch(wzy = file01, loc = file02)
      ids<-c(ids, prefix[1])
      res00<-cbind(res00, id = prefix[1])
      res<-rbind(res, res00)
      prefix<-prefix[! prefix %in% prefix[1]]
      while(length(prefix) > 1){
        id<-c()
        InUseName<-grep(paste(pat01, pat01_01, sep = ""), grep(prefix[1], fl, value = TRUE, invert = FALSE), value = TRUE, invert = FALSE)
        InUseName02<-grep(paste(pat01, pat02, sep = ""), grep(prefix[1], fl, value = TRUE, invert = FALSE), value = TRUE, invert = FALSE)
        file01<-read.csv(paste(Dir, "/", InUseName, sep = ""), header = TRUE, sep = ",")
        file02<-read.csv(paste(Dir, "/", InUseName02, sep=""), header = TRUE, sep = ",")
        rawBatchData<-cbind(rawBatchData, file01[,-1])
        res00<-wzy.batch(wzy = file01, loc = file02)
        ids<-c(ids, prefix[1])
        res00<-cbind(res00, id = prefix[1])
        res<-rbind(res, res00)
        prefix<-prefix[! prefix %in% prefix[1]]
      }
    })
    Row_names<-rownames(res)
    rownames(res)<-NULL
    res<-cbind(Row_names, res)
    res<-list(res, ids, rawBatchData)
    res
  })
  fileList<-reactive({
    fileDir<-parseDirPath(volumes, input$directory)
    pat = input$postfix
    fl<-list.files(path = fileDir, all.files = FALSE)
    fl <- grep(pat, fl, value = TRUE)
    fl<-sort(fl)
    fl
  })
  #=== output part ===#
  output$tableB01.00<-DT::renderDataTable(
    head(values$tableB01.00), 
    caption = 'Table1: Files List',
    rownames = FALSE,
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
    )
  )
  output$tableB01.01<-DT::renderDataTable(
    head(values$tableB01.01), 
    caption = 'Table2: Files List',
    rownames = FALSE,
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
    )
  )
  output$tableB01.02<-DT::renderDataTable(
    head(values$tableB01.02), 
    caption = 'Table3: Files List',
    rownames = FALSE,
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
    )
  )
  output$tableB01.03<-DT::renderDataTable(
    head(values$tableB01.03), 
    caption = 'Table4: Samples ID',
    rownames = FALSE,
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv')
    )
  )
  #=== input update part ===#
  values$tableB01.00<-data.frame(V1 = NA, V2 = NA)
  values$tableB01.01<-data.frame(V1 = NA, V2 = NA)
  values$tableB01.02<-data.frame(V1 = NA, V2 = NA)
  values$tableB01.03<-data.frame(V1 = NA, V2 = NA)
  observe({
    if(is.null(input$tableB01.00)){
      tableB01.00<-fileList()
      No.<-c(1:length(tableB01.00))
      tableB01.00<-cbind(No., data = tableB01.00)
    } else {
      tableB01.00<-read.csv(input$tableB01.00$datapath, header=TRUE, sep=",")
    }
    isolate(values$tableB01.00<-tableB01.00)
  }) #TableB01.00
  observe({
    if(is.null(input$tableB01.01)){
      res<-resB01()
      tableB01.01<-as.data.frame((res[1]))
    } else {
      tableB01.01<-read.csv(input$tableB01.01$datapath, header=TRUE, sep=",")
    }
    isolate(values$tableB01.01<-tableB01.01)
  }) #TableB01.01
  observe({
    if(is.null(input$tableB01.02)){
      res<-resB01()
      tableB01.02<-as.data.frame((res[3]))
    } else {
      tableB01.02<-read.csv(input$tableB01.02$datapath, header=TRUE, sep=",")
    }
    isolate(values$tableB01.02<-tableB01.02)
  }) #TableB01.02
  observe({
    if(is.null(input$tableB01.03)){
      tableB01.03<-resB01()
      tableB01.03<-as.vector(unlist(tableB01.03[2]))
      tableB01.03<-data.frame(ID = tableB01.03, Group = 0)
    } else {
      tableB01.03<-read.csv(input$tableB01.03$datapath, header=TRUE, sep=",")
    }
    isolate(values$tableB01.03<-tableB01.03)
  }) #TableB01.03
    #=== 01.end ===#
  #### 02.Statistical Analysis ####
  #=== manipulation part ===#
  values$tableB02 <- data.frame(ID = NA, Group = NA)
  #=== output part ===#
  output$tableB02.00<-DT::renderDataTable({
      values$tableB02
  })
  #=== input updata part ===#
  observeEvent(input$AssigB02,{
    seRowB02<-input$tableB02.00_rows_selected
    isolate(values$tableB02[seRowB02,2]<-as.numeric(input$GroupMarkB02))
  })
  observeEvent(input$updateB02, {
    isolate(values$tableB02<-values$tableB01.03)
  })
  #### Global Setting ####
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)