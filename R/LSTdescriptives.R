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
  
  if (ready) {
    data <- .getDataLSTdesc(jaspResults, options, inputType)
    if(options[["LSdescCentralOrSpread"]] == "LSdescCentralTendency"){
      #all central tendeny plots go here
      .lstDescCreateBarplot(jaspResults, options, data)
      .lstDescCreateDotplot(jaspResults, options, data)
    }
  }
  
  if(options[["LSdescCentralOrSpread"]] == "LSdescCentralTendency"){
    #all spread plots go here
  }
  
  
  .descExplanation(jaspResults, options)
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
  if (options[["lstDescSampleDistType"]] == "lstSampleDistDiscrete") {
    #discrete random functions
  } else if (options[["lstDescSampleDistType"]] == "lstSampleDistCont") {
    #continuous functions
  }
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

.lstDescCreateBarplot <- function(jaspResults, options, data){
  jaspResults[["descBarplot"]] <- createJaspContainer(gettext("Barplot"))
  
  
  bp <- createJaspPlot(title = gettext("Barplot"), width = 700, height = 400)
  bp$position <- 2
  
  xBreaks <- seq(min(data$x), max(data$x))
  
  bpPlotObject <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x)) + 
    ggplot2::geom_bar() + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::scale_x_continuous(breaks = xBreaks)
  
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(ggplot2::ggplot_build(bpPlotObject)$data[[1]]$y)
  yLimits <- range(yBreaks)
  
  bpPlotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, bpPlotObject)
  
  # if(options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMMM"){
  #   mean <- mean(data$x)
  #   bpPlotObject <- bpPlotObject + ggplot2::geom_vline(xintercept = mean, size = 1, color = "red") +
  #     ggplot2::geom_label(data = data.frame(x = mean, y = max(yLimits)*0.95, label = gettextf("Mean = %.2f", mean)), 
  #                         mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = 6)
  # }
  
  bp$plotObject <- bpPlotObject
  
  jaspResults[["descBarplot"]] <- bp
}

.lstDescCreateDotplot <- function(jaspResults, options, data){
  jaspResults[["descDotplot"]] <- createJaspContainer(gettext("Dotplot"))
  
  
  dp <- createJaspPlot(title = gettext("Dotplot"), width = 700, height = 400)
  dp$position <- 3
  xBreaks <- seq(min(data$x), max(data$x))
  
  dpPlotObject <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x)) + 
    ggplot2::geom_dotplot() + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::scale_x_continuous(breaks = xBreaks)
  
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(ggplot2::ggplot_build(dpPlotObject)$data[[1]]$ymax)
  yLimits <- range(yBreaks)
  
  dpPlotObject <- .drawMeanMedianOrModeLine(jaspResults, options, data, dpPlotObject)
  
  dp$plotObject <- dpPlotObject
  
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



.drawMeanMedianOrModeLine <- function(jaspResults, options, data, plot){
  yLimits <- range(ggplot2::ggplot_build(plot)$data[[1]]$y)
  if (options[["LSdescCT"]] == "LSdescMean" | options[["LSdescCT"]] == "LSdescMMM") {
    mean <- mean(data$x)
    plot <- plot + ggplot2::geom_vline(xintercept = mean, size = 1, color = "red") +
      ggplot2::geom_label(data = data.frame(x = mean, y = max(yLimits)*0.95, label = gettextf("Mean = %.2f", mean)), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "red", size = 6)
  }
  if (options[["LSdescCT"]] == "LSdescMedian"| options[["LSdescCT"]] == "LSdescMMM") {
    median <- median(data$x)
    plot <- plot +
      ggplot2::geom_vline(xintercept = median, size = 1, color = "green") +
      ggplot2::geom_label(data = data.frame(x = median, y = max(yLimits)*0.85, label = gettext("Median")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = 6)
  }
  if(options[["LSdescCT"]] == "LSdescMode"| options[["LSdescCT"]] == "LSdescMMM"){
    plotData <- ggplot2::ggplot_build(plot)$data[[1]]
    mode <- plotData$x[plotData$y == max(plotData$y)]
    plot <- plot + ggplot2::geom_vline(xintercept = mode, size = 1, color = "blue") + 
      ggplot2::geom_label(data = data.frame(x = mode, y = max(yLimits)*0.75, label = gettext("Mode")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = 6)
  }
  return(plot)
}