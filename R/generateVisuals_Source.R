
#-------------------------Data Prep Functions-------------------


#'This function takes an MCQ question represented by multiple logical columns and collapses it to a single column
#'@param mcq: question object of type MCQ whose data is represented by logical columns
#'
#'TODO: other column needs to be handled
collapseMcq <- function(mcq){
  mcq@type = "MCQ_Checkbox"
  collapsedCol = c()
  data <- mcq@data
  names <- sapply(names(data), function(name){
    unlist(strsplit(name, " -  "))[2]
  })
  names(data) <- names
  for(i in 1:NROW(data)){
    thisRow <- c()
    for(j in 1:NCOL(data)){
      if(!is.na(data[i,j])){
        if(data[i,j]=="1"){
          thisRow <- c(thisRow, names(data)[j])
        }
      }
    }
    if(is.null(thisRow)){
      collapsedCol <- append(collapsedCol, NA)
    }else{
      collapsedCol <- append(collapsedCol,paste(thisRow, collapse=" , "))
    }
  }#outer loop
  mcq@data <- data.frame(collapsedCol)
  names(mcq@data) <- mcq@title
  return(mcq)
}

#'scales slider data to fall between a specified lower and upper limit
#'
#'@param slider: question object of type slider
#'@param lower: lower scale limit
#'@param upper: upper scale limit
#'
normalizeSlider <- function(slider, lower, upper){
  scaledData <- c()
  for(thisData in slider@data[,1]){
    thisScaled <- (upper - lower) * thisData + lower
    scaledData <- c(scaledData, thisScaled)
  }
  slider@data[,1] <- scaledData

  return(slider)
}

#'Logical function which determines if a question is represented by logical columns or not
#'@param mcq: question object of type MCQ
#' essential logic is: if all columns (excluding NA and/or Other column if present) only contain the "1" character
#' the it's a checkbox
#'
isCheckboxMcq <- function(mcq){
  thisData <- mcq@data
  answerLevels <- sapply(names(thisData), function(name){
    if(!(grepl(" - NA", name, fixed=TRUE) | grepl(" - Other", name, fixed=TRUE))){
      return(levels(as.factor(thisData[,name])))
    }
  })

  levelsAtOne <- unlist(unique(answerLevels)) == "1"

  if(length(levelsAtOne) > 1){
    return(FALSE)
  }else{
    if(levelsAtOne == TRUE){
      return(TRUE)
    }
    return(FALSE)
  }
}



#-----------------Plotting Helper Functions------------------------


prepTernaries <- function(ternaries){
  preppedTernaries <- list()
  for(i in 1:length(ternaries)){
    ternary <- ternaries[[i]]
    for(j in 1:3){
      thisPrepped <- new("Question", type="Triangle",
                         labels=c(paste0("-",ternary@labels[j]),paste0("+",ternary@labels[j])),
                         title=ternary@title, data=ternary@data[j]/100)
      preppedTernaries <- append(preppedTernaries, thisPrepped)
    }
  }
  return(preppedTernaries)
}


#'Helper function for plotAllMarbles.  Generates collection of
#'question objects for each column of each marble question
#'@param marbles: list of marble objects
prepMarbles <- function(marbles){
  plots <- list()
  preppedMarbles <- list()
  for(i in 1:length(marbles)){
    marble <- marbles[[i]]
    for(j in 1:NCOL(marble@data)){
      #check to see if on an NA column, if so, skip.
      colName <- strsplit(names(marble@data[j]), " - ") %>%
        unlist()
      colName <- colName[length(colName)]
      if(colName == "NA"){
        next
      }
      if(j%%2 == 0){
        thisPrepped <- new("Question", type="Marble", labels=c(marble@labels[length(marble@labels)-1],
                                                               marble@labels[(length(marble@labels))]),
                           title=paste0(marble@title," - ",marble@labels[ceiling(j/2)]), data=marble@data[j])
      }else{
        thisPrepped <- new("Question", type="Marble", labels=c(marble@labels[length(marble@labels)-3],
                                                               marble@labels[(length(marble@labels))-2]),
                           title=paste0(marble@title," - ",marble@labels[ceiling(j/2)]), data=marble@data[j])
      }
      preppedMarbles <- append(preppedMarbles, thisPrepped)
    }
  }

  return(preppedMarbles)
}

#'helper function for plotAll function family. contains code to transform data in
#' preparation for violin plots for non-checkbox MCQs.
#'@param .dyad: question object of type slider
#'@param .factor: question object of type MCQ
getPlotDataRadio <- function(.dyad, .factor, plotInfo=NULL){

  .dyadData <- .dyad@data[1]
  .factorData <- .factor@data[1]
  data <- data.frame(.dyadData, .factorData, check.names=FALSE) %>%
    na.omit()
  names(data) <- c("continuous", "factor")
  return(plotQuestion(data, .dyad, .factor, plotInfo))
}

#'Helper function for plotAll function family. contains code to generate violin plots from
#'single dimension of continuous data over a set of factors
#'@param data: Data.frame containing data to generate plot
#'@param .dyad: question object of type slider
#'@param .factor: question object of type MCQ or MCQ_Checkbox
plotQuestion <- function(data, .dyad, .factor, plotInfo=NULL){

  data$factor <- str_wrap(data$factor, width=10)
  thisPlot <-  ggplot(data=data, mapping=aes(x=continuous, y=factor)) +
    geom_violin(fill="red") +
    stat_summary(fun="mean", geom="point", shape=8, size=2, color="black") +
    ylab(.factor@title) +
    xlab(paste0(.dyad@labels[1]," <----------> ",.dyad@labels[2])) +
    ggtitle(.dyad@title)
  #display meta data for plot all stuff
#
#   if(displayInfo && !is.null(plotInfo)){
#     message <- paste0(plotInfo$type,
#                       " cInd:", plotInfo$cInd,
#                       " fInd", plotInfo$fInd,
#                       " varInd:",plotInfo$varInd,
#                       " axis:",plotInfo$axis)
#
#
#     thisPlot <- thisPlot +
#       labs(tag = "Slider, qInd:2, varInd:1, axis:1") +
#       theme(plot.tag.position = c(0.2, 0.02))
#   }

  return(thisPlot)
}



#'helper function for plotAll function family. contains code to transform data in preparation for violin plots
#'For checkbox MCQs.
#'@param .dyad question object of type slider
#'@param .factor question object of type MCQ_Checkbox
getPlotDataCheckbox <- function(.dyad, .factor){
  .dyadData <- .dyad@data[1]
  .factorData <- .factor@data[1]
  data <- data.frame(.dyadData, .factorData, check.names=FALSE) %>%
    na.omit()
  continuous <- c()
  factor <- c()

  for(i in 1:NROW(data)){
    responses <- unlist(strsplit(data[i,2], " , "))
    for(response in responses){
      continuous <- append(continuous, data[i,1])
      factor <- append(factor, response)
    }
  }

  data <- data.frame(continuous, factor)

  return(plotQuestion(data, .dyad, .factor))

}


#'helper function for plot function family. Function writes ggplots out to output folder
#'
#'@param plots: Either a ggplot or list of ggplot objects
#'@param type: A string idicating which type of question is represented in the given plots
#'@param path: Location of the output/visualizations/<plot type> folder
#'
savePlots <- function(plots, type, path){

  #get maximum id number in relevant output folder
  nPlots <- getMaxPlotId(type, path)
  if(class(plots)[1] == "gg"){
    ggsave(paste0(path, "/", type, "_", (nPlots + 1), ".png"), plots)

  }
  else if(class(plots) == "list"){
    #write plots out to folder
    for(i in 1:length(plots)){
      ggsave(paste0(path, "/", type, "_", (nPlots + i), ".png"), plots[[i]])
    }
  }
  else{
    warning("Error in savePlots(): Unknown Data type provided to 'plots' param")
  }
}



#'helper function for savePlots() function. Obtains greatest ID number of .png images in a given plot folder
#'
#'@param type: A string idicating which type of question is represented in the given plots
#'@param path: Location of the output/visualizations/<plot type> folder
#'
getMaxPlotId <- function(type, path){
  plotFiles <- list.files(path)

  nameInds <- lapply(plotFiles, function(x){
    grepl(type, x, fixed=TRUE)
  }) %>% unlist()

  plotFiles <- plotFiles[nameInds]

  nPlots <- 0

  #get maximum plot id number in relevant output folder
  if(length(plotFiles) >= 1){
    nPlots <- sapply(plotFiles, function(x){
      x <- str_remove(x, ".png")
      num <- str_split(x, "_")[[1]][2] %>%
        as.integer()
      return(num)
    }) %>%
      max()
  }

  return(nPlots)
}

#' helper function for writePlots() function.  Generates random selection of
#' consonants and numbers of length 5 for file names
#'
randString <- function(){
  chars <- c("1","2","3","4","5","6","7","8","9",
             letters[!letters %in% c("a","e","i","o","u")])
  return(paste(sample(chars, 9, replace=TRUE), collapse=""))

}





