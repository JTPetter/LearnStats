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
  .descExampleDistribution(jaspResults, options)
}


.descExampleDistribution<- function(jaspResults, options) {
  jaspResults[["descExampleDistribution"]] <- createJaspContainer(gettext("Distribution"))
  jaspResults[["descExampleDistribution"]]$position <- 1
  
  mean <- 0
  sd <- 1
  skew <- 100
  
  pdPlot <- createJaspPlot(title = gettext("Distribution"), width = 700, height = 400)
  
  
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
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = median, size = 1, color = "green") +
      ggplot2::geom_label(data = data.frame(x = median, y = max(yLimits)*0.85, label = gettext("Median")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "green", size = 6)
  }
  
  if(options[["LSdescCT"]] == "LSdescMode"| options[["LSdescCT"]] == "LSdescMMM"){
    plotData <- ggplot2::ggplot_build(pdPlotObject)$data[[1]]
    mode <- plotData$x[plotData$y == max(plotData$y)]
    pdPlotObject <- pdPlotObject + ggplot2::geom_vline(xintercept = mode, size = 1, color = "blue") + 
      ggplot2::geom_label(data = data.frame(x = mode, y = max(yLimits)*0.75, label = gettext("Mode")), 
                          mapping = ggplot2::aes(x = x, y = y, label = label), color = "blue", size = 6)
  }
  
  pdPlot$plotObject <- pdPlotObject
  
  jaspResults[["cltParentDistribution"]] <- pdPlot
}


.calculateMode <- function(x){
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}
