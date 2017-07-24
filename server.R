#### Required Packages and Functions ####
library(shiny)
library(shinydashboard)
library(ggplot2)
library(biwavelet)
library(data.table)
library(stringr)
library(ape)
library(DT)
library(shinyFiles)
source("functions.R")
  ###=== end of packages and functions loading ===###
#### Service Function ####
function(input, output, session) {
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
    similarity<-WZY.Wavelet.clust(similarity)
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
  })
    ###=== 04.end ===###
####=== Batching Processing ===####
  volumes <- c('Root'="/Users/HSBR")
  shinyDirChoose(input, 'directory', roots=volumes, session=session)
  #### 00.data manipulation ####
  fileList<-reactive({
    fileDir<-parseDirPath(volumes, input$directory)
    pat = "\\.csv"
    fl<-list.files(path = fileDir, all.files = FALSE)
    fl <- grep(pat, fl, value = TRUE)
    fl<-sort(fl)
    fl
  })
  output$tableBatch01.00<-renderTable({
    if(is.null(input$directory)){
      return(NULL)
    } else
    tablein01.00<-fileList()
    No.<-c(1:length(tablein01.00))
    tablein01.00<-cbind(No., data = tablein01.00)
    tablein01.00
  })
}
