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
                        title = "Title", discreteTitle = "Discrete", lLabel = "Left Label",
                        tLabel = "Top Label", rLabel = "Right Label", showMean = TRUE){

  #if no discrete variable is provided, just call all levels responses
  if(is.null(discrete)){
    discrete <- rep("responses", NROW(continuous))
  }

  plotData <- data.frame(continuous, discrete) %>%
    na.omit()

  #find levels which are present within the data (i.e. not dropped due to NA)
  presentLevels <- factor(plotData$discrete) %>%
    levels()

  #assign y axis tick marks
  #yAxisTicks <- unlist(labelPairs[presentLevels])

  print(labelPairs)

  p <- ggtern(data = plotData) +
    geom_point(mapping = aes(x = `L`, y = `T`, z = `R`, color = discrete)) +
    Llab(str_wrap(lLabel, width = 7)) +
    Tlab(str_wrap(tLabel, width = 7)) +
    Rlab(str_wrap(rLabel, width = 7)) +
    labs(title = title, color = str_wrap(discreteTitle, width = 25)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = fill, labels = labelPairs)
  #labels = unlist(labelPairs[presentLevels])
  if(showMean){
    p <- p + geom_mean_ellipse(mapping = aes(x = `L`, y = `T`, z = `R`))
  }

  return(p)

}

