#=== Widely Used Mathmatical Extraction of Interested wave signal ####
WZY.Wumei <- function(Data,Unit = 1, MeCe = FALSE, ChangeColName = FALSE, 
                      AddPrefix = FALSE, ProRep = TRUE, loc = NULL, Prefix = NULL,
                      FirstCol.name = NULL, SecondCol.name = NULL, RestCol.name = NULL) { 
  # MeCe: Mean Centering
  # Reference.name: first simple name
  #
  if(ProRep == TRUE) {
    on.exit(progress$close())
    progress <- Progress$new(min = 0, max = 1)
    progress$set(message = "Wave feature calculation")
    Sys.sleep(1)
  }
  #### Global Variabels ####
  Nrow <- nrow(Data)    # How many time points (sampling)
  Ncol <- ncol(Data)    # How many column in original data
  Data <- as.matrix(Data)
  #---end---#
  #
  if (ChangeColName == TRUE && AddPrefix == TRUE) {
    for (i in 1:Ncol) {
      if (i == 1) {
        colnames(Data)[i] <- FirstCol.name
      } else if (i == 2) {
        colnames(Data)[i] <- str_c("S", Prefix, SecondCol.name)
      } else {
        colnames(Data)[i] <- str_c("S", Prefix, RestCol.name, ".", i-2)
      }
    }
  } else if (ChangeColName == TRUE && AddPrefix == FALSE) {
    for (i in 1:Ncol) {
      if (i == 1) {
        colnames(Data)[i] <- FirstCol.name
      } else if (i == 2) {
        colnames(Data)[i] <- SecondCol.name
      } else {
        colnames(Data)[i] <- str_c(RestCol.name, " ", i-2)
      }
    }
  } else if (ChangeColName == FALSE && AddPrefix == TRUE) {
    for (i in 1:Ncol) {
      if (i == 1) {
        colnames(Data)[i] <- colnames(Data)[i]
      } else if (i == 2) {
        colnames(Data)[i] <- str_c("S", Prefix, colnames(Data)[i])
      } else {
        colnames(Data)[i] <- str_c("S", Prefix, colnames(Data)[i])
      }
    }
  }
  Co.Na <- colnames(Data)
  #### Mean centering processing ####
  if(ProRep == TRUE) {
    progress$set(value = 0.02, message = "Mean centering processing")
    Sys.sleep(1)
  }
  if (MeCe == TRUE) {
    for (i in 2:Ncol) {
      Data[, i] <- Data[, i]-mean(Data[, i])
    }
  }
  #### simple features ####
  if(ProRep == TRUE) {
    progress$set(value = 0.04, message = "simple features")
    Sys.sleep(1)
  }
  iemg <- as.vector(colSums(abs(Data[, -1])))        # Integrated Signal (Int)
  mav <- as.vector(iemg/Nrow)                        # Mean Absolute Value (MAV)
  var <- as.vector(colSums(Data[, -1]^2)/(Nrow-1))   # variance of signal (Var)
  rms <- as.vector(sqrt(colSums(Data[, -1]^2)/Nrow)) # Root Mean Square (RMS)
  #---end---#
  #
  #### Calculate the maximum amplitude (MA) ####
  if(ProRep == TRUE) {
    progress$set(value = 0.06, message = "Calculate the maximum amplitude (MA)")
    Sys.sleep(1)
  }
  ma <- c()
  for (i in 2:Ncol) {
    ma <- c(ma, max(Data[, i]) - min(Data[, i]))
  }
  ma <- as.vector(ma)
  #---end---#
  #
  #### Calculate the waveform Length (WL) ####
  if(ProRep == TRUE) {
    progress$set(value = 0.08, message = "Calculate the waveform Length (WL)")
    Sys.sleep(1)
  }
  wl <- c()
  absDiff <- 0 # Absolute difference between two points
  for (j in 2:Ncol) {
    y<-0       # final result
    for (i in 1:(Nrow-1)) {
      absDiff <- abs(Data[i+1, j]-Data[i, j])
      y <- y+absDiff
    }
    wl <-c(wl, y)  # Results from all samples
  }
  wl <- as.vector(wl)
  #---end---#
  #
  #### Mean centering processing for follow calculation ####
  for (i in 2:Ncol) {
    Data[, i] <- Data[, i]-mean(Data[, i])
  }
  #---end---#
  #
  #### Calculate the Mean Power Frequency (MPF) ---- using fast Fourier transform ####
  # From: (http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html)
  # Reset parameters
  mpf <- c()
  #---
  timeStep <- Data[3, 1]-Data[2, 1]
  sampleSize <- Nrow
  Fs <- sampleSize/(sampleSize*timeStep)
  N <- sampleSize
  seq <- c(1:(N/2)) # Frequency array
  xv <- (seq/N)*Fs
  xv <- c(0, xv)
  for (i in 2:Ncol) {
    x.k <- fft(Data[, i])
    mod <- Mod(x.k)
    mod <- mod[1:((N/2)+1)]
    mod <- (mod^2)/N
    mpf <- c(mpf, sum(mod*xv)/sum(mod))
    if(ProRep == TRUE) {
      progress$inc(amount = 0.32/(Ncol-1))
    }
  }
  #---end---#
  #
  #### Calculate the main period (MP) and J index (J_index) ---- by wavelet transform  ####
  if(ProRep == TRUE) {
    progress$set(value = 0.4, message = "Calculate the J index (J_index)")
    Sys.sleep(1)
  }
  mp <- c()
  indexJ <- c()
  x <- c()
  y <- c()
  s <- c()
  dwt <- wt(cbind(Data[ , 1], Data[ , 2]), mother = "morlet", do.sig = FALSE) # Diecrete Wavelet Transform
  y <- rowSums(abs(dwt$wave)^2)
  x <- dwt$scale
  s <- cbind(x, y)
  max <- max(y)
  row <- which(s[, 2] == max)
  out <- s[row, 1]
  mp <- c(mp, out)
  W <- dwt$wave
  a <- dwt$scale
  v <- 5/(2*pi*a)
  v <- v*Unit
  indexj<-c()
  for (i in 1:sampleSize) {
    Wpeaks <- findpeaks(abs(W[, i]))[, 1]
    Wpeaks <- Wpeaks^2
    vposition <- v[findpeaks((abs(W[, i])))[, 2]]
    temp <- sum(Wpeaks*vposition)/2
    indexj <- c(indexj, temp)
    if(ProRep == TRUE) {
      progress$inc(amount = 0.125/sampleSize)
    }
  }
  temp <- mean(indexj)
  indexJ<-c(indexJ, temp)
  w.arr <- array(NA, dim = c(Ncol-2, NROW(dwt$wave), NCOL(dwt$wave)))
  if(ProRep == TRUE) {
    progress$set(value = 0.525, message = "Calculate the main period (MP)")
    Sys.sleep(1)
  }
  for (i in 3:Ncol) {
    # Reset all parameters
    x <- c()
    y <- c()
    s <- c()
    #---
    dwt <- wt(cbind(Data[, 1], Data[, i]), mother = "morlet", do.sig = FALSE) # Diecrete Wavelet Transform
    y <- rowSums(abs(dwt$wave)^2)
    x <- dwt$scale
    s <- cbind(x, y)
    max <- max(y)
    row <- which(s[, 2] == max)
    out <- s[row, 1]
    mp <- c(mp, out)
    w.arr[i-2, , ]<-dwt$wave                                                  # Array of wavelet results from all samples (except the first column, the reference sample)
    W <- dwt$wave
    indexj <- c()
    for (j in 1:sampleSize) {
      Wpeaks <- findpeaks(abs(W[, j]))[, 1]
      Wpeaks <- Wpeaks^2
      vposition <- v[findpeaks((abs(W[, j])))[, 2]]
      temp <- sum(Wpeaks*vposition)/2
      indexj <- c(indexj, temp)
      if(ProRep == TRUE) {
        progress$inc(amount = 0.0625/(sampleSize*(Ncol-3)))
      }
    }
    temp <- mean(indexj)
    indexJ<-c(indexJ, temp)
    if(ProRep == TRUE) {
      progress$inc(amount = 0.0625/(Ncol-3))
    }
  }
  mp<-as.vector(mp)
  w.arr <- round(w.arr, digits = 8)                                           # Aovid un-convergence error
  if(ProRep == TRUE) {
    progress$set(value = 0.65, message = "Doing Wave cluster analysis")
  }
  w.arr.dis <- wclust(w.arr, quiet = TRUE)                                    # Wavelet clust
  w.arr.dis <- w.arr.dis$dist.mat                                             # Dissimilarity matrix
  w.arr.dis <- as.matrix(w.arr.dis)                                           # Convert to matrix for name changing
  row.names(w.arr.dis) <- Co.Na[-c(1,2)]
  w.arr.dis <- as.dist(w.arr.dis)                                             # Convert back to dist type
  #---end---#
  #
  #### Result construction for features extraction ####
  res <- data.frame(
    row.names = colnames(Data)[-c(1,2)],
    Int = iemg[-1],
    MAV = mav[-1],
    VAR = var[-1],
    RMS = rms[-1],
    WL = wl[-1],
    MP = mp[-1],
    MA = ma[-1],
    MPF = (mpf*Unit)[-1],
    J_index = indexJ[-1]
  )
  #---end---#
  #
  #### Calculate the Spatial Auto-correlection ####
  if(ProRep == TRUE) {
    progress$set(value = 0.85, message = "Calculate the Moran's Index")
    Sys.sleep(1)
  }
  if (is.null(loc) == FALSE) {
    loc <- loc[-1, ]                                      # Remove the reference sample
    Dist <- dist(cbind(loc[, 2], loc[, 3]))               # Spatial distance matrix
    #--- Moran index ( from: https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/ )
    ozone.dists.inv <- 1/as.matrix(Dist)                  # Take inverse values of the spatial distance matrix values
    diag(ozone.dists.inv) <- 0                            # Replace the diagonal entries with zero
    moran.I <- c()
    moran.I.p <- c()
    for (i in 1:ncol(res)) {
      moran <- Moran.I(res[, i], ozone.dists.inv)
      moran.I <-c(moran.I, moran$observed)
      moran.I.p <- c(moran.I.p, moran$p.value)
    }
    moran.I <- as.numeric(moran.I)
    moran.I.p <- as.numeric(moran.I.p)
    #--- Spearman's Rho and Dissimilarity matrix
    if(ProRep == TRUE) {
      progress$set(value = 0.95, message = "Calculate the Spearman's Rho")
      Sys.sleep(1)
    }
    SRD <- cor.test(w.arr.dis, Dist, method = "spearman") # Spearman's rank test for the correlation between Dissimilarity matrix and Spatial Distance matrix
    SRD.Rho <- SRD$estimate                               # Spearman's Rho
    SRD.p <- SRD$p.value                                  # P value of Spearman's Rho
    #---end---#
    #
    #### Result construction for global level ####
    res.r <- rbind("Region" = c(iemg[1], mav[1], var[1], rms[1], wl[1], mp[1], ma[1], (mpf*Unit)[1], indexJ[1]),
                   "Moran Index" = moran.I, "p value of Moran Index" = moran.I.p) # results at global level
    res.r <- as.data.frame(res.r)
    colnames(res.r)<-colnames(res)
    rownames(res.r)<- c(colnames(Data)[2], "Moran Index", "p value of Moran Index")
    res.s <- data.frame("Spearman's Rho" = SRD.Rho, "p value of Spearman's Rho" = SRD.p)
    #---end---#
    #
    #### Final result construction ####
    out <- list(Result = res, 
                G_Result = res.r, 
                S_Result = res.s, 
                matrix = w.arr.dis, 
                Data = Data,
                Data.L = loc) # G_Result, Global result; S_Result, Spearman's Rho Result
  } else if(is.null(loc) == TRUE) {
    res.r <- rbind("Region" = c(iemg[1], mav[1], var[1], rms[1], wl[1], mp[1], ma[1], (mpf*Unit)[1], indexJ[1]))
    colnames(res.r) <- colnames(res)
    rownames(res.r) <- colnames(Data)[2]
    out <- list(Result = res, 
                G_Result = res.r, 
                S_Result = NULL, 
                matrix = w.arr.dis, 
                Data = Data,
                Data.L = NULL) # G_Result, Global result; S_Result, Spearman's Rho Result
  }
  if(ProRep == TRUE) {
    progress$set(value = 1, message = "Complete")
    Sys.sleep(1)
  }
  return(out)
}
#=== Plot Frequency Spectrum ===####
# From: (http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html)
wzy.plot.MPF <- function(X.k, sampleSize, timeStep, Unit, UnitLabel) {
  X.k[1]<-0
  Fs <- sampleSize/(sampleSize*timeStep) 
  N <- sampleSize
  seq <- c(1:(N/2))
  xv=(seq/N)*Fs
  mod <- Mod(X.k)
  mod <- mod[1:(N/2)]
  mod <- (mod^2)/N 
  plot.data  <- cbind(c(0, xv[1:((N/2)-1)])*Unit, mod)
  plot.data[xv[2:(N/2)],2] <- 2*plot.data[xv[2:(N/2)],2] 
  plot(plot.data, t="h", lwd=2, main="", 
       xlab=paste("Frequency (", UnitLabel, ")", sep = ""), ylab="Power Spectral Density",
       ylim=c(0,max(Mod(plot.data[,2]))))+title(main = "Power Spectral Density")
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
# use withMathJax locally 
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