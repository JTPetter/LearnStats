#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

LSTdescriptives <- function(jaspResults, dataset, options, state = NULL) {
  inputType <- options[["lstDescDataType"]]
  ready <- (inputType == "dataRandom" |
              (inputType == "dataSequence" && options[["lstDescDataSequenceInput"]] != "") | 
              (inputType == "dataVariable" && options[["selectedVariable"]] != ""))
  data <- .getDataLSTdesc(jaspResults, options, inputType)
  
  #checking whether data is discrete or continuous, whereas only integers are treated as discrete
  discrete <- ifelse(all(data$x == as.integer(data$x)), TRUE, FALSE)
  
  if(options[["LSdescCentralOrSpread"]] == "LSdescCentralTendency"){
    if (options[["LSdescExplanationC"]])
      .descExplanation(jaspResults, options)
    if (options[["LSdescHistBar"]]) {
      .lstDescCreateHistogramOrBarplot(jaspResults, options, data, ready, discrete)
    }
    if (options[["LSdescDotPlot"]])
      .lstDescCreateDotplot(jaspResults, options, data, ready)
  }
  
  if(options[["LSdescCentralOrSpread"]] == "LSdescCentralTendency"){
    #all spread plots go here
  }
  
}


.descExplanation<- function(jaspResults, options) {
  jaspResults[["descExplanation"]] <- createJaspContainer(gettext("Explanation"))
  
  mean <- 0
  sd <- 1
  skew <- 100
  
  pdPlot <- createJaspPlot(title = gettext("Explanation"), width = 700, height = 800)
  pdPlot$position <- 1
  
  
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 4*sd, mean + 4*sd ))
  xLimits <- range(xBreaks)
  df <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
  pdPlotObject <-  ggplot2::ggplot(df, ggplot2::aes(x = x)) + ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..),
                                                                                    n = 2^7, bw = sd/3) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "")
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(ggplot2::ggplot_build(pdPlotObject)$data[[1]]$y)
  yLimits <- range(yBreaks)
  pdPlotObject <- pdPlotObject + ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = "Density") +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
  
  if(options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMMM"){
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = mean, size = 1, color = "red") +
      ggplot2::geom_label(data = data.frame(x = mean, y = max(yLimits)*0.95, label = gettext("Mean")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = 6)
  }
  
  
  if(options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM"){
    median <- median(df$x)
    pdPlotObject <- pdPlotObject +
      ggplot2::geom_vline(xintercept = median, size = 1, color = "green") +
      ggplot2::geom_label(data = data.frame(x = median, y = max(yLimits)*0.85, label = gettext("Median")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = 6)
    
    
    #ggplot2::geom_area(mapping = ggplot2::aes(x = ifelse(x>65 & x< 70 , x, 0)),
    # fill = 'purple') +      MAP DENSITY AT TOP LAYER??
  }
  
  if(options[["LSdescCT"]] == "LSdescMode"| options[["LSdescCT"]] == "LSdescMMM"){
    plotData <- ggplot2::ggplot_build(pdPlotObject)$data[[1]]
    mode <- plotData$x[plotData$y == max(plotData$y)]
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = mode, size = 1, color = "blue") + 
      ggplot2::geom_label(data = data.frame(x = mode, y = max(yLimits)*0.75, label = gettext("Mode")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = 6)
  }
  
  
  #text plot
  textPlot <- .ggplotWithText("For a data set, the arithmetic mean, also known as arithmetic average, is a central value of a finite set of numbers: specifically, the sum of the values divided by the number of values. The arithmetic mean of a set of numbers x1, x2, ..., xn is typically denoted by xn. If the data set were based on a series of observations obtained by sampling from a statistical population, the arithmetic mean is the sample mean to distinguish it from the mean, or expected value, of the underlying distribution, the population mean .[1]
                              
                              Outside probability and statistics, a wide range of other notions of mean are often used in geometry and mathematical analysis; examples are given below.")
  
  
  plotMat <- matrix(list(), 2, 1)
  plotMat[[1, 1]] <- pdPlotObject
  plotMat[[2, 1]] <- textPlot
  
  pdPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  
  jaspResults[["cltParentDistribution"]] <- pdPlot
}


.getDataLSTdesc <- function(jaspResults, options, inputType) {
  if (inputType == "dataRandom") {
    data <- .sampleRandomDataForLSTdesc(jaspResults, options)
  } else if (inputType == "dataSequence") {
    data <- .readInputSequenceForLSTdesc(jaspResults, options)
  } else if (inputType == "dataVariable") {
    data <- .readDataLSTdesc(jaspResults, options)
  }
  return(data)
}

.sampleRandomDataForLSTdesc <- function(jaspResults, options){
  set.seed(options[["lstDescSampleSeed"]])
  n <- options[["lstDescSampleN"]]
  if (options[["lstDescSampleDistType"]] == "lstSampleDistDiscrete") {
    if (options[["LSdescDiscreteDistributions"]] == "binomialDist") {
      data <- rbinom(n, 10, prob = .5)
    } else if (options[["LSdescDiscreteDistributions"]] == "poissonDist") {
      data <- rpois(n, 1)
    }
  } else if (options[["lstDescSampleDistType"]] == "lstSampleDistCont") {
    if (options[["LSdescContinuousDistributions"]] == "skewedNormal") {
      data <- sn::rsn(n, alpha = 100)
    } else if (options[["LSdescContinuousDistributions"]] == "uniform") {
      data <- runif(n = n, min = 0, max = 5)
    } else if (options[["LSdescContinuousDistributions"]] == "normal"){
      data <- rnorm(n, 0, 10)
    }
  }
  df <- data.frame(x = data)
  return(df)
}


.readInputSequenceForLSTdesc <- function(jaspResults, options){
  inputSequence <- options[["lstDescDataSequenceInput"]]
  inputSequence <- unlist(strsplit(inputSequence, split = ","))
  inputSequence <- gsub(" ", "", inputSequence, fixed = TRUE)
  df <- data.frame(x = as.numeric(inputSequence))
  return(df)
}


.readDataLSTdesc <- function(jaspResults, options){
  variable <- unlist(options[["selectedVariable"]])
  variable <- variable[variable != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric = variable)
  df <- data.frame(x = unlist(dataset))
  return(df)
}

.lstDescCreateHistogramOrBarplot <- function(jaspResults, options, data, ready, discrete){
  title <- ifelse(discrete, "Barplot", "Histogram")
  jaspResults[["descHistogramOrBarplot"]] <- createJaspContainer(gettext(title))
  p <- createJaspPlot(title = gettext(title), width = 700, height = 400)
  p$position <- 2
  
  if (ready) {
    if (discrete){
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
      yBreaks <- unique(round(jaspGraphs::getPrettyAxisBreaks(c(0, table(data$x)))))
      yLimits <- range(yBreaks) * 1.1
      plotObject <- ggplot2::ggplot(data, ggplot2::aes(x = x)) + ggplot2::geom_bar(fill = "grey",
                                                                                   col = "black", size = .3) + 
        ggplot2::scale_y_continuous(name = "Counts", breaks = yBreaks, limits = yLimits) + 
        ggplot2::scale_x_continuous(name = "Observations", breaks = xBreaks) + 
        jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
    } else{
      density <- options[["LSdescHistCountOrDens"]] == "LSdescHistDens"
      yBreaks <- (jaspGraphs::getPrettyAxisBreaks(c(0, max(data$x))))
      yLimits <- range(yBreaks) * 1.1
      plotObject <- jaspDescriptives:::.plotMarginal(data$x, variableName = "Observations", displayDensity = density,
                                                     binWidthType = "sturges") 
      
    }
    plotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, plotObject, yMax = max(yLimits))
    p$plotObject <- plotObject
  }
  jaspResults[["descHistogramOrBarplot"]] <- p
}

.lstDescCreateDotplot <- function(jaspResults, options, data, ready){
  jaspResults[["descDotplot"]] <- createJaspContainer(gettext("Dotplot"))
  height <- 400 + (length(data$x) / 50) * 25
  dp <- createJaspPlot(title = gettext("Dotplot"), width = 700, height = height)
  dp$position <- 3
  
  if (ready){
    dpPlotObjectList <- .lstDescCreateDotPlotObject(data, options)
    dpPlotObject <- dpPlotObjectList$p
    if ((options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMode" | options[["LSdescCT"]] == "LSdescMMM"))
      dpPlotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, dpPlotObject, yMax = dpPlotObjectList$yMax)
    dp$plotObject <- dpPlotObject
  }
  
  jaspResults[["descDotplot"]] <- dp
  
}


.ggplotWithText <- function(text){
  text <- "For a data set, the arithmetic mean, also known as arithmetic average, \n 
  is a central value of a finite set of numbers: specifically, the sum of the values \n 
  divided by the number of values. The arithmetic mean of a set of numbers x1, x2, ..., \n 
  xn is typically denoted by xn. If the data set were based on a series of observations \n 
  obtained by sampling from a statistical population, the arithmetic mean is the sample \n 
  mean to distinguish it from the mean, or expected value, of the underlying distribution, \n
  the population mean .[1]\n
  \n                            
  Outside probability and statistics, a wide range of other notions of mean are often used in \n 
  geometry and mathematical analysis; examples are given below."
  
  nText <- length(text)
  emptyBox <- data.frame(x = 0:10, y = 0:10)
  annotation <- data.frame(x = 5, y = 5, label = text)
  p <- ggplot2::ggplot(emptyBox, ggplot2::aes(x = x, y= y)) + ggplot2::geom_point(color = "white") + ggplot2::theme_void() +
    ggplot2::geom_text(data=annotation, ggplot2::aes(x = x, y = y, label = label), size = 5)
  
  return(p)
}



.drawMeanMedianOrModeLine <- function(jaspResults, options, data, plot, yMax){
  if (options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMMM") {
    mean <- mean(data$x)
    plot <- plot + ggplot2::geom_vline(xintercept = mean, size = 1, color = "red") +
      ggplot2::geom_label(data = data.frame(x = mean, y = yMax, label = gettextf("Mean = %.2f", mean)), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = 6)
  }
  if (options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM") {
    median <- median(data$x)
    plot <- plot +
      ggplot2::geom_vline(xintercept = median, size = 1, color = "green") +
      ggplot2::geom_label(data = data.frame(x = median, y = yMax, label = gettext("Median")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = 6)
  }
  if(options[["LSdescCT"]] == "LSdescMode"| options[["LSdescCT"]] == "LSdescMMM"){
    plotData <- ggplot2::ggplot_build(plot)$data[[1]]
    mode <- plotData$x[plotData$y == max(plotData$y)]
    plot <- plot + ggplot2::geom_vline(xintercept = mode, size = 1, color = "blue") + 
      ggplot2::geom_label(data = data.frame(x = mode, y = yMax*0.75, label = gettext("Mode")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = 6)
  }
  return(plot)
}


.lstDescCreateDotPlotObject <- function(data, options){
  n <- length(data$x)
  if (length(unique(data$x)) == 1){ 
    dotsize <- .0333
  } else if (n > 50){
    dotsize <- 1 - (log(n)) / 30
  } else {
    dotsize <- 1
  }
  
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data$x)
  xLimits <- range(xBreaks)
  
  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = x)) +
    ggplot2::geom_dotplot(binaxis = 'x', stackdir = 'up', dotsize = dotsize, fill = "grey") +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = xLimits) +
    ggplot2::coord_fixed()
  
  pData <- ggplot2::ggplot_build(p)$data
  dotWidth <- pData[[1]]$width[1] * dotsize
  yLabels <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(c(0, max(pData[[1]]$countidx)))))
  yBreaks <- yLabels * dotWidth
  yMax <- ifelse(max(yBreaks) < (6 * dotWidth), (6 * dotWidth), max(yBreaks))
  yLimits <-  c(0, yMax + (2 * dotWidth))

  p <- p + ggplot2::scale_y_continuous(name = "Count", limits = yLimits, breaks = yBreaks, labels = yLabels) + 
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
    # ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
    #                axis.title.y = ggplot2::element_blank(),
    #                axis.text.y = ggplot2::element_blank())
  
  if (options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM"){
    sortedDf <- data.frame(x = sort(data$x))
    halfway <- median(as.numeric(rownames(sortedDf)))
    if(n %% 2 != 0){   # if median is a single datapoint
      halfwayDot <- pData[[1]][halfway,]
      y0 <- ifelse(halfwayDot$countidx == 1, dotWidth/2, dotWidth/2 + (halfwayDot$countidx - 1) * dotWidth)
      circleData <- data.frame(x0 = halfwayDot$x, 
                               y0 = y0,
                               r = dotWidth / 2)
      p <- p + ggforce::geom_circle(data = circleData, mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
                                    inherit.aes = FALSE, fill = "green")
    } else {  # if median is the average of two points
      halfwayDots <- list("lowerDot" = pData[[1]][halfway - .5,],
                          "upperDot" = pData[[1]][halfway + .5,])
      y0lower <- ifelse(halfwayDots$lowerDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$lowerDot$countidx - 1) * dotWidth)
      y0upper <- ifelse(halfwayDots$upperDot$countidx == 1, dotWidth / 2, dotWidth / 2 + (halfwayDots$upperDot$countidx - 1) * dotWidth)
      if (all(c(halfwayDots$lowerDot$count, halfwayDots$upperDot$count) == 1)) {
        start <- c(pi * 2, pi * 2)
        end <-c(pi * 3, pi)
      } else {
        start <- c(pi * 1.5, pi / 2)
        end <- c(pi * 2.5, pi * 1.5)
      }
      circleData <- data.frame(x0 = c(halfwayDots$lowerDot$x, halfwayDots$upperDot$x),
                               y0 = c(y0lower, y0upper),
                               r = rep(dotWidth/2, 2),
                               r0 = rep(0, 2),
                               start = start,
                               end = end)
      
      p <- p + ggforce::geom_arc_bar(data = circleData,
                                     mapping = ggplot2::aes(x0 = x0, y0 = y0, r0 = r0, r = r,  start = start, end = end),
                                     inherit.aes = FALSE, fill = "green")
    }
  }
  return(list("p" = p, "yMax" = max(yLimits) * .95))
}
