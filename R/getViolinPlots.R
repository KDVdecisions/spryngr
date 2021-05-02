






getAllViolinPlots <- function(outputFolder, flatten = TRUE){

  discretePath <- paste0(outputFolder, "/Data/discreteData.xlsx")
  continuousPath <- paste0(outputFolder, "/Data/continuousData.xlsx")

  outline <- read.xlsx(paste0(outputFolder, "/Data/outline.xlsx"), sheetIndex = 1)

  outline$LABELS <- toList(outline$LABELS)

  continuousData <- getAllDataSheets(continuousPath)
  cOutline <- filter(outline, CLASS != "discrete")

  discreteData <- getAllDataSheets(discretePath)
  dOutline <- filter(outline, CLASS %in% c("discrete", "marble"))


  plots <- list()
  plotInd = 1

  for(i in 1:length(continuousData)){

    #get Outline row for continuous data
    thisOutline <- cOutline[i,]

    #get question title for continuous data
    title <- cOutline[i,]$QUESTION

    #get continuous data for this question and drop IS_NA col
    thisContinuous <- continuousData[[i]] %>%
      select(-c(IS_NA))


    for(j in 1:length(discreteData)){
      #get question title for discrete data
      ylab <- dOutline[j,]$QUESTION
      #get all selected factors for this
      thisDiscrete <- discreteData[[j]]$SET


      for(k in 1:NCOL(thisContinuous)){
        xlab <- generateXLabel(thisOutline, k)
        p <- violinPlot(thisContinuous[,k], thisDiscrete, title, xlab, ylab, flatten)
        plots[[plotInd]] <- p
        plotInd <- plotInd + 1

      }

    }
  }
  return(plots)
}


#'reads data from each sheet within a .xlsx file
getAllDataSheets <- function(path){
  #organize all discrete data into a list
  wb <- xlsx::loadWorkbook(file = path)
  names <- getSheets(wb) %>%
    names()
  data <- lapply(names, function(x){
    thisDat <- read.xlsx(file = path, sheetName = x, check.names = FALSE)
  })
  return(data)
}






generateXLabel <- function(thisOutline, index){
  base <- "%s <--------------------------> %s"
  leftLabel <- "-X Label"
  rightLabel <- "+X Label"

  if(thisOutline$CLASS == "marble"){
    if(index%%2 != 0){
      leftLabel <- thisOutline$LABELS[[1]][1]
      rightLabel <- thisOutline$LABELS[[1]][2]
    } else{
      leftLabel <- thisOutline$LABELS[[1]][3]
      rightLabel <- thisOutline$LABELS[[1]][4]
    }
  }else if(thisOutline$CLASS == "slider") {
    leftLabel <- thisOutline$LABELS[[1]][1]
    rightLabel <- thisOutline$LABELS[[1]][2]

  }else if(thisOutline$CLASS == "ternary"){
    leftLabel <- paste0("-",thisOutline$LABELS[[1]][index])
    rightLabel <- paste0("+",thisOutline$LABELS[[1]][index])
  }

  label <- sprintf(base, leftLabel, rightLabel)

  return(label)
}


flattenData <- function(data){
  levels <- c()
  indices <- c()
  data$discrete <- toList(data$discrete)



  for(i in 1:NROW(data)){
    for(level in unlist(data[i,2])){
      levels <- c(levels, level)
      indices <- c(indices, i)
    }
  }

  return(data.frame(continuous = data[,1][indices], discrete = levels))
}

toList <- function(data){
  result <- sapply(data, function(x){
    x <- str_split(x, ", ") %>%
      unlist() %>%
      trimws("both")
  }, USE.NAMES = FALSE)

  return(result)
}


violinPlot <- function(continuous, discrete, title, xlab, ylab, flatten = TRUE){

  plotData <- data.frame(continuous, discrete) %>%
    na.omit()

  if(flatten){
    plotData <- flattenData(plotData)
  }


  ggplot(data = plotData, mapping = aes(x = continuous, y = discrete)) +
    geom_violin() +
    labs(title = title) +
    xlab(xlab) +
    ylab(ylab)

}




