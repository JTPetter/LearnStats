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

LSTcentralLimitTheorem <- function(jaspResults, dataset, options, state = NULL) {
  parentData <- .generateParentData(options)
  samples <- .cltTakeSamples(jaspResults, options, data = parentData)
  
  .cltParentDistribution(jaspResults, options)
  .cltPlotSamples(jaspResults, options, samples = samples)
  .cltSamplingDistribution(jaspResults, options, samples = samples)
  
  
}

.cltParentDistribution <- function(jaspResults, options) {
  jaspResults[["cltParentDistribution"]] <- createJaspContainer(gettext("Parent Distribution"))
  jaspResults[["cltParentDistribution"]]$position <- 1
  
  distribution <- options[["cltParentDistribution"]]
  mean <- options[["cltMean"]]
  sd <- options[["cltStdDev"]]
  if(options[["cltSkewIntensity"]] == "low"){
    skew <- 2
  }else if(options[["cltSkewIntensity"]] == "medium"){
    skew <- 5
  }else if(options[["cltSkewIntensity"]] == "high"){
    skew <- 50
  }
  
  pdPlot <- createJaspPlot(title = gettext("Parent Distribution"), width = 700, height = 400)
  pdPlot$plotObject <- .distributionPlotFunction(distribution, mean, sd, skew, skewDirection = options[["cltSkewDirection"]],
                                                 fillColor = "coral")
  jaspResults[["cltParentDistribution"]] <- pdPlot
}


.cltTakeSamples <- function(jaspResults, options, data) {
  parentData <- data
  n <- options[["cltSampleSize"]]
  k <- options[["cltSampleAmount"]]
  sampleList <- list()
  for(i in 1:k){
    sampleList[[i]] <- sample(x = parentData[["x"]], size = n, replace = T)
  }
  return(sampleList)
}


.cltPlotSamples <- function(jaspResults, options, samples) {
  jaspResults[["cltSamples"]] <- createJaspContainer(gettext("Samples"))
  jaspResults[["cltSamples"]]$position <- 2
  
  samples <- samples[1:7]
  
  matrixPlot <- createJaspPlot(title = gettext("Samples"), width = 1200, height = 1000)
  
  plotMat <- matrix(list(), 4, 2)
  
  allCounts <- vector()
  for(i in 1:7){
    h1 <- hist(samples[[i]], plot = F, right = F, breaks = 'Sturges')
    counts <- h1$counts
    allCounts <- c(allCounts, counts)
  }
  
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(allCounts)))
  yLimits <- range(yBreaks)
  
  h2 <- hist(unlist(samples), plot = F, right = F, breaks = 'Sturges')
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h2$breaks))
  xLimits <- range(xBreaks)
  binWidth <- (h2$breaks[2] - h2$breaks[1])
  
  
  
  index <- 0
  
  for(i in 1:4){
    for (j in 1:2){
      index <-  index + 1
      if (index == 8){
        samplePlot <- ggplot2::ggplot() + ggplot2::theme_void() + 
          ggplot2::annotate("text", x = 0, y = 0, label = gettextf("... until Sample Nr. %i", options[["cltSampleAmount"]]), size = 10)
      }else{
        mean <- mean(samples[[index]])
        
        samplePlot <- ggplot2::ggplot(data.frame(x = samples[[index]]), ggplot2::aes(x = x)) + 
          ggplot2::geom_histogram(mapping = ggplot2::aes(y =..count..),
                                  closed = "left", fill = "coral", col = "black", size = .7, binwidth = binWidth, center = binWidth/2) +
          ggplot2::geom_rug() +
          ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
          ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
          ggplot2::geom_vline(xintercept = mean, color = "cornflowerblue", size = 1) +
          ggplot2::geom_label(data = data.frame(x = mean, y = max(yLimits)*0.95, label = gettextf("Mean: %.2f", mean)), 
                              mapping = ggplot2::aes(x = x, y = y, label = label), color = "cornflowerblue", size = 6) +
          ggplot2::ggtitle(gettextf("Sample Nr. %i", index)) +
          jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
      }
      plotMat[[i,j]] <- samplePlot
    }
  }
  
  
  
  
  
  
  p <- jaspGraphs::ggMatrixPlot(plotMat)
  matrixPlot$plotObject <- p
  
  jaspResults[["cltSamples"]] <- matrixPlot
}

.generateParentData <- function(options){
  distribution <- options[["cltParentDistribution"]]
  mean <- options[["cltMean"]]
  sd <- options[["cltStdDev"]]
  if(distribution == "normal"){
    data <- rnorm(100000, mean, sd)
  }else if(distribution == "uniform"){
    min <- mean - sd/2
    max <- mean + sd/2
    data <- runif(100000, min = min, max = max)
  }else if(distribution == "skewed"){
    if(options[["cltSkewIntensity"]] == "low"){
      skew <- 1.5
    }else if(options[["cltSkewIntensity"]] == "medium"){
      skew <- 3
    }else if(options[["cltSkewIntensity"]] == "high"){
      skew <- 10
    }
    if (options[["cltSkewDirection"]] == "left"){
      skew <- skew * -1
    }
    data <- .scaledSkewedNormal(100000, xi = mean, omega = sd,  alpha = skew)
  }
  df <- data.frame(x = data)
  return(df)
}


.cltSamplingDistribution <- function(jaspResults, options, samples) {
  jaspResults[["cltSamplingDistribution"]] <- createJaspContainer(gettext("Sampling Distribution of the Mean"))
  jaspResults[["cltSamplingDistribution"]]$position <- 3
  
  means <- lapply(samples, mean)
  meanDf <- data.frame(x = unlist(means))
  n <- length(samples)
  meanOfMeans <- mean(meanDf[["x"]])
  sdOfMeans <- sd(meanDf[["x"]])
  
  h <- hist(meanDf[["x"]], plot = F, breaks = 'Sturges')
  counts <- h$counts
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(counts)*1.1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h$breaks), min.n = 4)
  xLimits <- range(xBreaks)
  binWidth <- (h$breaks[2] - h$breaks[1])
  
  sdPlot <- createJaspPlot(title = gettext("Sampling Distribution of the Mean"), width = 700, height = 400)
  sdPlotObject <- ggplot2::ggplot(meanDf, ggplot2::aes(x = x)) + 
    ggplot2::geom_histogram(mapping = ggplot2::aes(y =..count..),
                            closed = "left", fill = "cornflowerblue", col = "black", size = .7,
                            binwidth = binWidth, center = binWidth/2) +
    ggplot2::geom_rug() +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
    ggplot2::geom_vline(xintercept = meanOfMeans, color = "darkgoldenrod1", size = 1) +
    ggplot2::geom_label(data = data.frame(x = meanOfMeans, y = max(yLimits)*0.95, label = gettextf("Mean: %.2f", meanOfMeans)), 
                        mapping = ggplot2::aes(x = x, y = y, label = label), color = "darkgoldenrod1", size = 6)
  if (options[["SamplingDistShowNormal"]]){
    sdPlotObject <- sdPlotObject + 
      ggplot2::stat_function(fun = function(x)dnorm(x, mean = meanOfMeans, sd = sdOfMeans) * binWidth * n)
  }
  
  
  sdPlotObject <- sdPlotObject + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
  
  sdPlot$plotObject <- sdPlotObject
  jaspResults[["cltSamplingDistribution"]] <- sdPlot
}

.scaledSkewedNormal <- function(n, xi=0, omega=1, alpha=0, tau=0){
  y <- xi + omega*scale(sn::rsn(n, xi = xi, omega = omega, alpha = alpha, tau = tau))
  return(y)
}


.distributionPlotFunction <- function(distribution = c("normal", "uniform", "skewed"), mean, sd, skew, skewDirection = "right", fillColor,
                                      showMean = TRUE, returnData = FALSE){
  
  if(distribution == "normal"){
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 3*sd, mean + 3*sd ))
    xLimits <- range(xBreaks)
    pdPlotObject <-  ggplot2::ggplot(data.frame(x = xBreaks), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = dnorm, n = 100, args = list(mean = mean, sd = sd), geom = "area", fill = fillColor) +
      ggplot2::stat_function(fun = dnorm, n = 100, args = list(mean = mean, sd = sd))
    if(returnData)
      df <- data.frame(x = rnorm(n = 100000, mean = mean, sd = sd))
  }else if(distribution == "uniform"){
    min <- mean - sd/2
    max <- mean + sd/2
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min - sd/4, max + sd/4))
    xLimits <- range(xBreaks)
    pdPlotObject <-  ggplot2::ggplot(data.frame(x = xBreaks), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = dunif, n = 100, args = list(min = min, max = max), geom = "area", fill = fillColor) +
      ggplot2::stat_function(fun = dunif, n = 100, args = list(min = min, max = max))
    if(returnData)
      df <- data.frame(x = runif(n = 100000, min = min, max = max))
  }else if(distribution == "skewed"){
    if (skewDirection == "left")
      skew <- skew * -1
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(mean - 3*sd, mean + 3*sd ))
    xLimits <- range(xBreaks)
    df <- data.frame(x = .scaledSkewedNormal(100000, xi = mean, omega = sd, alpha = skew))
    pdPlotObject <-  ggplot2::ggplot(df, ggplot2::aes(x = x)) + ggplot2::geom_density(mapping = ggplot2::aes(y = ..density..),
                                                                                      n = 2^7, bw = sd/3, fill = fillColor)
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(ggplot2::ggplot_build(pdPlotObject)$data[[1]]$y)
  yLimits <- range(yBreaks)
  
  pdPlotObject <- pdPlotObject + ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits, name = "") +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits, name = "Density")
  if(showMean){
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = mean, color = "darkmagenta", size = 1) + 
      ggplot2::geom_label(data = data.frame(x = mean, y = max(yLimits)*0.95, label = gettextf("Mean: %.2f", mean)), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "darkmagenta", size = 6)
  }
  pdPlotObject <- pdPlotObject + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
  
  if(!returnData)
    return(pdPlotObject)
  
  if(returnData){
    return(list(plotobject = pdPlotObject, data = df))
  }
}
