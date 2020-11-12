
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

#-----------------------Plot All Functions----------------------

#'Generates a violin plot for each combination of marble column and multiple choice question.
#'@param marbles: List of Marble Question Objects
#'@param factors: List of Multiple Choice Question objects
#'TODO: Accept a vector of marbleInds
#'
plotAllMarbles <- function(marbles, factors){
  marbles <- prepMarbles(marbles)
  plots <- list()
  i <- 1
  for(marble in marbles){
    for(factor in factors){
      #plotInfo: type, cInd, fInd, varInd, axis

      #plotInfo <- list(type="Marble", cInd=, fInd)


      if(factor@type == "MCQ_Checkbox"){
        thisPlot <- getPlotDataCheckbox(marble, factor)
      }else{
        thisPlot <- getPlotDataRadio(marble, factor)
      }
      plots[[i]] <- thisPlot
      i = i+1
    }
  }
  return(plots)
}


#'function plots all combinations of slider questions vs multiple choice questions
#'@param dyads: list of question objects of type slider
#'@param factors: list of question objects of type factor
#'
plotAllDyads <- function(dyads, factors){
  plots <- list()
  i = 1
  for(dyad in dyads){
    for(factor in factors){
      if(factor@type == "MCQ_Checkbox"){
        thisPlot <- getPlotDataCheckbox(dyad, factor)
      }else{
        thisPlot <- getPlotDataRadio(dyad, factor)
      }
      plots[[i]] <- thisPlot
      i = i+1
    }
  }
  return(plots)
}


#'Function plots all combinations of ternary dimensions vs factors
#'@param tenraries: List of Question objects of type triangle
#'@param factors: List of Question objects of type MCQ or MCQ_Checkbox
plotAllTernaries <- function(ternaries, factors){
  ternaries <- prepTernaries(ternaries)
  i <- 1
  plots <- c()
  for(ternary in ternaries){
    for(factor in factors){
      if(factor@type == "MCQ_Checkbox"){
        thisPlot <- getPlotDataCheckbox(ternary, factor)
      }else {
        thisPlot <- getPlotDataRadio(ternary, factor)
      }
      plots[[i]] <- thisPlot
      i = i + 1
    }
  }
  return(plots)
}

#-----------------------Plot Functions--------------------------

#'Function which plots single dyad vs single factor which allows specification of labels and tites
#'@param dyad: Question object of type Slider
#'@param factor: Question object of type MCQ or MCQ_Checkbox
#'@param title: String containing the title for the generated plot (optional)
#'@param ylab: String containing the Y label for the generated plot (optional)
#'@param xlab: Vector containing 2 Strings to label the x axis in the negative and positive direction respectively (optional)
#'Any labels left blank will be gathered from the generated labels document
plotDyad <- function(dyad, factor, title=NULL, ylab=NULL, xlab=NULL){
  plot <- NULL

  if(!is.null(title)){
    dyad@title <- title
  }
  if(!is.null(ylab)){
    factor@title <- ylab
  }
  if(!is.null(xlab)){
    dyad@labels <- xlab
  }

  if(factor@type == "MCQ_Checkbox"){
    plot <- getPlotDataCheckbox(dyad, factor)
  }else{
    plot <- getPlotDataRadio(dyad, factor)
  }
  return(plot)
}

#'Plots one axis of one marble from a Question object of type Marble
#'@param marble: Question object of type Marble
#'@param factor: Question object of type MCQ or MCQ_Checkbox
#'@param varIndex: integer indicating the index of the Marble to be used
#'@param axis: integer equal to 1 or 2 dictating x or y axis respectively
#'@param title: String containing the title for the generated plot (optional)
#'@param ylab: String containing the Y label for the generated plot (optional)
#'@param xlab: Vector containing 2 Strings to label the x axis in the negative and positive direction respectively (optional)
#'Any labels left blank will be gathered from the generated labels document
plotMarble <- function(marble, factor, varIndex, axis,
                       title=NULL, ylab=NULL, xlab=NULL){
  plot <- NULL
  if(varIndex > NCOL(marble@data)){
    warning("marble index out of bounds")
    return()
  }
  if(axis > 2){
    warning("axis must be equal to 1 (x axis) or 2 (y axis)")
    return()
  }

  if(!is.null(title)){
    marble@title <- title
  }else{
    marble@title <- marble@labels[varIndex]
  }
  if(!is.null(ylab)){
    factor@title <- ylab
  }
  #if specified x axis
  if(axis==1){
    #if xlabel is specied via argument
    if(!is.null(xlab)){
      marble@labels <- xlab
    }else{
      #set marble@labels to labels at corresponding to proper marble index and axis
      marble@labels <- c(marble@labels[(length(marble@labels))-3],
                         marble@labels[(length(marble@labels))-2])
    }

    marble@data <- marble@data[(varIndex + (varIndex - 1))]
  }#if specified y axis
  else if(axis==2){
    #if xlabel is specied via argument
    if(!is.null(xlab)){
      marble@labels <- xlab
    }else{
      #set marble@labels to labels at corresponding to proper marble index and axis
      marble@labels <- c(marble@labels[(length(marble@labels))-1],
                         marble@labels[length(marble@labels)])
    }
    marble@data <- marble@data[(varIndex + varIndex)]

  }
  #generate plot
  if(factor@type == "MCQ_Checkbox"){
    plot <- getPlotDataCheckbox(marble, factor)
  }else {
    plot <- getPlotDataRadio(marble, factor)
  }
  return(plot)

}

#'Plots one axis of a ternary vs one factor
#'@param ternary: Question object of type Triangle
#'@param factor: Question object of type MCQ or MCQ_Checkbox Question object
#'@param varIndex: int 1-3 indicating which axis of the Triangle/ternary to use
#'@param title: String containing the title for the generated plot (optional)
#'@param ylab: String containing the Y label for the generated plot (optional)
#'@param xlab: Vector containing 2 strings to label the x axis in the negative and positive direction respectively (optional)
#'Any labels left blank will be gathered from the generated labels document
#'
plotTernary <- function(ternary, factor, varIndex, title=NULL,
                        ylab=NULL, xlab=NULL){
  plot <- NULL
  if(varIndex > 3){
    print("varIndex should be 1-3")
    return()
  }

  #check for specified labels
  if(!is.null(title)){
    ternary@title <- title
  }
  if(!is.null(ylab)){
    factor@title <- ylab
  }
  if(!is.null(xlab)){
    ternary@labels <- xlab
  }else{
    ternary@labels <- c(paste0("- ",ternary@labels[varIndex]),
                        paste0("+ ",ternary@labels[varIndex]))
  }
  ternary@data <- ternary@data[varIndex]/100

  if(factor@type == "MCQ_Checkbox"){
    plot <- getPlotDataCheckbox(ternary, factor)
  }else {
    plot <- getPlotDataRadio(ternary, factor)
  }
  return(plot)
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
    #print(ternary)
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
  #LEFT OFF HERE------------
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
#'@param .dyad: question object of type slider
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




