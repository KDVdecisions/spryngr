#'Generates scatter ternary plot
#'@param continuous: data.frame with columns l, t, r for left, top, right data points for ternary responses
#'@param discrete: vector containing corresponding discrete data points
#'@param fill:
#'@param labelPairs:
#'@param title:
#'@param lLabel:
#'@param tLabel:
#'@param rLabel:
#'@param showMean:
#'
ternaryPoint <- function(continuous, discrete = NULL, fill = NULL, labelPairs = NULL,
                        title = "Title",lLabel = "Left Label", tLabel = "Top Label",
                        rLabel = "Right Label", showMean = TRUE){

  #if no discrete variable is provided, just call all levels responses
  if(is.null(discrete)){
    discrete <- rep("responses", NROW(continuous))
  }

  plotData <- data.frame(continuous, discrete)

  #find levels which are present within the data (i.e. not dropped due to NA)
  presentLevels <- factor(plotData$discrete) %>%
    levels()
  #assign y axis tick marks
  yAxisTicks <- unlist(labelPairs[presentLevels])

  p <- ggtern(data = plotData) +
    geom_point(mapping = aes(x = l, y = t, z = r, fill = discrete)) +
    Llab(lLabel) +
    Tlab(tLabel) +
    Rlab(rLabel) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = fill, breaks = levels(plotData$discrete))

  if(showMean){
    p <- p + geom_mean_ellipse(mapping = aes(l,r,t))
  }

  return(p)

}

