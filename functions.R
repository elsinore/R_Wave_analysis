#################################
# WZY	Basic Functions Collection#
#################################
#===============================#
###################### Basic Functions for Data Manipulation
#=== Convert Object of Class "dist" into Data Frame in R ####
# User: A5C1D2H2I1M1N2O1R2T1 From:(https://stackoverflow.com/questions/23474729/convert-object-of-class-dist-into-data-frame-in-r)
WZY.convertDist <- function(inDist) {
  if (class(inDist) != "dist") stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) sequence(A) else attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1):1),
    value = as.vector(inDist))
}
####### Example: Input############################################
#            CONT       INTG       DMNR       DILG
# INTG 0.56659545                                 
# DMNR 0.57684427 0.01769236                      
# DILG 0.49380400 0.06424445 0.08157452           
# CFMG 0.43154385 0.09295712 0.09332092 0.02060062
####### Example: Output################################################
#     row  col      value
# 1  INTG CONT 0.56659545
# 2  DMNR CONT 0.57684427
# 3  DILG CONT 0.49380400
# 4  CFMG CONT 0.43154385
# 5  DMNR INTG 0.01769236
# 6  DILG INTG 0.06424445
# 7  CFMG INTG 0.09295712
# 8  DILG DMNR 0.08157452
# 9  CFMG DMNR 0.09332092
# 10 CFMG DILG 0.02060062
#=== Mathematical Representation of Widely used Wave Feature Extraction ####
WZY.EMG.F <- function(wzy) {
  library("biwavelet")
  require("biwavelet")
  #### Global Variabels ####
  wzyo<-wzy
  wzy<-wzy[,-1]
  ncol <- ncol(wzy)
  nrow <- nrow(wzy)
  ncolo <- ncol(wzyo)
  nrowo <- nrow(wzyo)
  #### Easy calculation features ####
  iemg <- colSums(abs(wzy)) # Integrated EMG (IEMG)
  mav <- iemg/nrow # Mean Absolute Value (MAV)
  var <- colSums(wzy^2)/(nrow-1) # Variance of EMG (VAR)
  rms <- sqrt(colSums(wzy^2)/nrow) # Root Mean Square (RMS)
  # Finished easy part
  ###
  #### calculate the maximum amplitude (MA) ####
  main<-0
  ma<-c()
  for (main in 1:ncol){
    ma<-c(ma, max(wzy[, main])-min(wzy[, main]))
  }
  # finished calculation of Maximum Amplitude (MA)
  ###
  #### calculate the Waveform Length (WL) ####
  wl <- c(0)
  absDiff <-0
  y<-0
  for(j in 1:ncol) {
    y <- 0
    for(i in 1:(nrow-1)) {
      absDiff <- abs(wzy[i+1,j]-wzy[i,j])
      y <- y + absDiff
    }
    wl[j]<-y
  }
  # Finished calculation of Waveform Length (WL)
  ###
  #### calculate the Main Period (MP) ####
  dw<-0
  mp<-c()
  for(dw in 2:ncolo){
    x<-c()
    y<-c()
    s<-c()
    max<-0
    row<-0
    dwt<-wt(cbind(wzyo[, 1], wzyo[, dw]), do.sig = FALSE)
    y<-rowSums(abs(dwt$wave)^2)
    x<-dwt$period
    s<-cbind(x, y)
    max<-max(y)
    row<-which(s[, 2] == max)
    out <- s[row, 1]
    mp<-c(mp, out)
  }
  mp<-as.vector(mp)
  # Finished calculation of Main Period (MP)
  ###
  #### calculate the Mean Power Frequency (MPF) ####
  timeStep <- wzyo[3, 1]-wzyo[2, 1]
  sampleSize <- nrow
  Fs <- sampleSize/(sampleSize*timeStep)
  N <- sampleSize
  seq <- c(1:(N/2))
  xv <-(seq/N)*Fs
  xv <- c(0, xv)
  mpfin <- 0
  mpf<-c()
  for (mpfin in 1:ncol){
    X.k <- fft(wzy[, mpfin])
    mod <- Mod(X.k)
    mod<-mod[1:((N/2)+1)]
    mpf <- c(mpf, sum(mod*xv)/sum(mod))
  }
  #= finished calculation of Mean Power Frequency
  ###
  ##### result construction ####
  res<-data.frame(
    row.names = colnames(wzy),
    Int = iemg,
    MAV = mav,
    VAR = var,
    RMS = rms,
    WL = wl,
    MP = mp,
    MA = ma,
    MPF = mpf
  )
  return(list(data=wzyo, results=res))
}
WZY.EMG.F.MPDF <- function(wzy) {
  library("biwavelet")
  require("biwavelet")
  #### Global Variabels ####
  wzyo<-wzy
  wzy<-wzy[,-1]
  ncol <- ncol(wzy)
  nrow <- nrow(wzy)
  ncolo <- ncol(wzyo)
  nrowo <- nrow(wzyo)
  #### Easy calculation features ####
  iemg <- colSums(abs(wzy)) # Integrated EMG (IEMG)
  mav <- iemg/nrow # Mean Absolute Value (MAV)
  var <- colSums(wzy^2)/(nrow-1) # Variance of EMG (VAR)
  rms <- sqrt(colSums(wzy^2)/nrow) # Root Mean Square (RMS)
  # Finished easy part
  ###
  #### calculate the maximum amplitude (MA) ####
  main<-0
  ma<-c()
  for (main in 1:ncol){
    ma<-c(ma, max(wzy[, main])-min(wzy[, main]))
  }
  # finished calculation of Maximum Amplitude (MA)
  ###
  #### calculate the Waveform Length (WL) ####
  wl <- c(0)
  absDiff <-0
  y<-0
  for(j in 1:ncol) {
    y <- 0
    for(i in 1:(nrow-1)) {
      absDiff <- abs(wzy[i+1,j]-wzy[i,j])
      y <- y + absDiff
    }
    wl[j]<-y
  }
  # Finished calculation of Waveform Length (WL)
  ###
  #### calculate the Main Period (MP) ####
  dw<-0
  mp<-c()
  for(dw in 2:ncolo){
    x<-c()
    y<-c()
    s<-c()
    max<-0
    row<-0
    dwt<-wt(cbind(wzyo[, 1], wzyo[, dw]), do.sig = FALSE)
    y<-rowSums(abs(dwt$wave)^2)
    x<-dwt$period
    s<-cbind(x, y)
    max<-max(y)
    row<-which(s[, 2] == max)
    out <- s[row, 1]
    mp<-c(mp, out)
  }
  mp<-as.vector(mp)
  # Finished calculation of Main Period (MP)
  ###
  #### calculate the Mean Power Frequency (MPF) ####
  timeStep <- wzyo[3, 1]-wzyo[2, 1]
  sampleSize <- nrow
  Fs <- sampleSize/(sampleSize*timeStep)
  N <- sampleSize
  seq <- c(1:(N/2))
  xv <-(seq/N)*Fs
  xv <- c(0, xv)
  mpfin <- 1
  mpf<-c()
  for (mpfin in 1:ncol){
    X.k <- fft(wzy[, mpfin])
    mod <- Mod(X.k)
    mod <-mod[1:((N/2)+1)]
    mpf <- c(mpf, sum(mod*xv)/sum(mod))
  }
  #### calculate the Mean Power Density Frequency (MPDF) ####
  mpdfin <- 1
  mpdf<-c()
  for (mpdfin in 1:ncol){
    X.kd <- fft(wzy[, mpdfin])
    modd <- Mod(X.kd)
    modd <-modd[1:((N/2)+1)]
    modd <- (modd^2)/N 
    mpdf <- c(mpdf, sum(modd*xv)/sum(modd))
  }
  #= finished calculation of Mean Power Frequency
  ###
  ##### result construction ####
  res<-data.frame(
    row.names = colnames(wzy),
    Int = iemg,
    MAV = mav,
    VAR = var,
    RMS = rms,
    WL = wl,
    MP = mp,
    MA = ma,
    MPF = mpf,
    MPDF = mpdf
  )
  return(list(data=wzyo, results=res))
}
#=== Wavelet Analysis ####
WZY.Wavelet.clust <- function(input){ # wave clust in a same sample
  library("biwavelet")
  library("stringr")
  require("stringr")
  require("biwavelet")
  ncol<-NCOL(input)
  wt.t1 <- wt(cbind(input[ , 1], input[ , 2]), do.sig = FALSE)
  w.arr <- array(NA, dim = c(ncol-1, NROW(wt.t1$wave), NCOL(wt.t1$wave)))
  for(i in 2:ncol) {
    wt.t<-wt(cbind(input[ , 1], input[ , i]), do.sig = FALSE)
    w.arr[i-1, , ] <- wt.t$wave
  }
  w.arr <- round(w.arr, digits = 4)
  w.arr.dis<-wclust(w.arr)
  w.arr.dis<-w.arr.dis$dist.mat
  w.arr.dis<-as.matrix(w.arr.dis)
  label <- colnames(input[, 2:ncol])
  label[1] <- str_c("====>", label[1])
  colnames(w.arr.dis)<-label
  row.names(w.arr.dis)<-label
  w.arr.dis<-as.dist(w.arr.dis)
  return(w.arr.dis)
}
WZY.Wavelet.clust2 <- function(input){ # wave clust in a same sample
  library("biwavelet")
  library("stringr")
  require("stringr")
  require("biwavelet")
  ncol<-NCOL(input)
  wt.t1 <- wt(cbind(input[ , 1], input[ , 2]), do.sig = FALSE)
  w.arr <- array(NA, dim = c(ncol-1, NROW(wt.t1$wave), NCOL(wt.t1$wave)))
  for(i in 2:ncol) {
    wt.t<-wt(cbind(input[ , 1], input[ , i]), do.sig = FALSE)
    w.arr[i-1, , ] <- wt.t$wave
  }
  w.arr <- round(w.arr, digits = 4)
  w.arr.dis <- wclust(w.arr)
  w.arr.dis <- w.arr.dis$dist.mat
  w.arr.dis <- as.matrix(w.arr.dis)
  label <- colnames(input[, 2:ncol])
  colnames(w.arr.dis)<-label
  row.names(w.arr.dis)<-label
  w.arr.dis<-as.dist(w.arr.dis)
  return(w.arr.dis)
}
#=== Frequency Spectrum ===####
# From: (http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html)
wzy.plot.frequency.spectrum <- function(X.k, sampleSize, timeStep) {
  X.k[1]<-0
  Fs<-sampleSize/(sampleSize*timeStep)
  N<-sampleSize
  seq<-c(1:(N/2))
  xv=(seq/N)*Fs
  mod<-Mod(X.k)
  mod<-mod[1:(N/2)]
  plot.data  <- cbind(c(0, xv[1:((N/2)-1)]), mod)
  plot.data[xv[2:(N/2)],2] <- 2*plot.data[xv[2:(N/2)],2] 
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength",
       ylim=c(0,max(Mod(plot.data[,2]))))+title(main = "Power Spectrum")
}
wzy.plot.frequency.spectrum.density <- function(X.k, sampleSize, timeStep) {
  X.k[1]<-0
  Fs <- sampleSize/(sampleSize*timeStep)
  N <- sampleSize
  seq <- c(1:(N/2))
  xv=(seq/N)*Fs
  mod <- Mod(X.k)
  mod <- mod[1:(N/2)]
  mod <- (mod^2)/N 
  plot.data  <- cbind(c(0, xv[1:((N/2)-1)]), mod)
  plot.data[xv[2:(N/2)],2] <- 2*plot.data[xv[2:(N/2)],2] 
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Power Spectral Density",
       ylim=c(0,max(Mod(plot.data[,2]))))+title(main = "Power Spectral Density")
}
#=== Function for Batching Processing ===####
wzy.batch <- function (wzy, loc) {
  library("biwavelet")
  require("biwavelet")
  if (class(wzy) != "matrix") {
    data <- as.matrix(wzy)
  }
  dataO<-data
  #### Global Variabels ####
  ncol <- NCOL(wzy)
  for(i in 2:ncol){
    data[, i]<-data[, i]-mean(data[, i])
  }
  res<-WZY.EMG.F(data)
  res<-res$results
  resin<-WZY.EMG.F(dataO)
  resin<-resin$results
  #### calculate the dissimilarity ####
  similarity<-WZY.Wavelet.clust2(dataO)
  resclu<-as.matrix(similarity)
  resclu<-resclu[,1]
  fit <- hclust(similarity, method = "ward.D")
  groups <- cutree(fit, k = 2)
  ratio <- as.numeric(length(groups[groups == 1])/length(groups))*100
  ##### result construction ####
  res<-cbind(res,
    Dissimilarity = resclu,
    Group = groups
  )
  resin<-cbind(resin,
    Dissimilarity = resclu,
    Group = groups
  )
  res.m<-resin[-1, ] #remove the data from region
  loc<-loc[-1, ] #remove the data from region
  ncol04 <- ncol(resin)
  rownames04 <- colnames(resin)
  ozone.dists<- as.matrix(dist(cbind(loc[, 2], loc[, 3])))
  ozone.dists.inv <- 1/ozone.dists
  diag(ozone.dists.inv) <- 0
  P.value <- c()
  Moran.I <- c()
  for(resin04 in 1:ncol04){
    moran <- Moran.I(res.m[, resin04], ozone.dists.inv)
    Moran.I <- c(Moran.I, moran$observed)
    P.value <- c(P.value, moran$p.value)
  }
  Moran.I <- as.numeric(Moran.I)
  P.value <- as.numeric(P.value)
  res<-rbind(res, "Moran Index" = Moran.I, "P value" = P.value)
  res<-cbind(res, Ratio = ratio)
  return(res)
}
wzy.batch2 <- function (wzy, loc) {
  library("biwavelet")
  require("biwavelet")
  if (class(wzy) != "matrix") {
    data <- as.matrix(wzy)
  }
  #### Global Variabels ####
  ncol <- NCOL(wzy)
  for(i in 2:ncol){
    data[, i]<-data[, i]-mean(data[, i])
  }
  res<-WZY.EMG.F(data)
  res<-res$results
  #### calculate the dissimilarity ####
  similarity<-WZY.Wavelet.clust2(data)
  resclu<-as.matrix(similarity)
  resclu<-resclu[,1]
  fit <- hclust(similarity, method = "ward.D")
  groups <- cutree(fit, k = 2)
  ratio <- as.numeric(length(groups[groups == 1])/length(groups))*100
  ##### result construction ####
  res<-cbind(res,
             Dissimilarity = resclu,
             Group = groups
  )
  res.m<-res[-1, ] #remove the data from region
  loc<-loc[-1, ] #remove the data from region
  ncol04 <- ncol(res)
  rownames04 <- colnames(res)
  ozone.dists<- as.matrix(dist(cbind(loc[, 2], loc[, 3])))
  ozone.dists.inv <- 1/ozone.dists
  diag(ozone.dists.inv) <- 0
  P.value <- c()
  Moran.I <- c()
  for(resin04 in 1:ncol04){
    moran <- Moran.I(res.m[, resin04], ozone.dists.inv)
    Moran.I <- c(Moran.I, moran$observed)
    P.value <- c(P.value, moran$p.value)
  }
  Moran.I <- as.numeric(Moran.I)
  P.value <- as.numeric(P.value)
  res<-rbind(res, "Moran Index" = Moran.I, "P value" = P.value)
  res<-cbind(res, Ratio = ratio)
  return(res)
}
#=== UI Functions ####
# All the code in this file needs to be copied to your Shiny app, and you need
# to call `withBusyIndicatorUI()` and `withBusyIndicatorServer()` in your app.
# You can also include the `appCSS` in your UI, as the example app shows.
# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    )
  )
}
# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}
# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}
# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}
withMathJax.local <- function(...) {
  path <- "MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  tagList(
    tags$head(
      singleton(tags$script(src = path, type = 'text/javascript'))
    ),
    ...,
    tags$script(HTML('if (window.MathJax) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'))
  )
}
wzy.force.format.colname <- function(x, colname) {
  colnames(x)<-colname
  return(x)
}
multiplot.wzy <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}