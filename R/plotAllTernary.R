library(readxl)


plotAllTernary <- function(outputFolder, showMean = TRUE, palette = NULL,
                           writePlots = FALSE, setOutputLocation = FALSE,
                           marblesAsDiscrete = FALSE, subsetIds = NULL) {
  #read in outline file
  outline <- read_excel(paste0(outputFolder, "/Data/outline.xlsx"))
  #Convert comma separated values to lists of vectors
  outline$CONTINUOUS_LABELS <- toList(outline$CONTINUOUS_LABELS)
  outline$DISCRETE_LABELS <- toList(outline$DISCRETE_LABELS)
  outline$LEVELS <- toList(outline$LEVELS)


  #set which classes should be treated as discrete variables
  discreteClasses = c("discrete")
  if(marblesAsDiscrete){
    discreteClasses <- c("discrete", "marble")
  }

  continuousClasses = c("ternary")

  #read in column of subsetIds to use all questions if subsetIds is not specified by user
  if(is.null(subsetIds)){
    subsetIds <- outline$ID
  }

  #read in discrete data
  discreteData <- getDiscreteData(outputFolder, outline, subsetIds, discreteClasses)
  #format discrete data
  discreteData <- formatDiscrete_new(discreteData, outline)
  #generate key-value structure where keys are discrete levels in data and values are
  #labels to be used within graph
  discreteLabelPairs <- apply(outline[outline$ID %in% names(discreteData),],
                              MARGIN = 1, FUN = generateDiscreteLabels)
  #generate list of fills to be used for each discrete variable
  discreteFills <- lapply(discreteData, function(x){
    nLevels <- length(levels(x$observation))
    return(colorRampPalette(brewer.pal(8, palette))(nLevels))
  })


  continuousData <- getContinuousData(outputFolder, outline, subsetIds, continuousClasses)

  plots <- list()

  plotIndex <- 1
  for(i in 1:length(continuousData)){
    thisContinuousOutline <- outline[outline$ID == names(continuousData)[i],]
    thisContinuousData <- continuousData[[i]]
    continuousLabels <- unlist(thisContinuousOutline$CONTINUOUS_LABELS)

    for(j in 1:length(discreteData)){
      thisDiscreteData <- discreteData[[j]]
      discreteLabel = outline[outline$ID == names(discreteData)[j],]$QUESTION
      p <- ternaryPoint(continuous = thisContinuousData[thisDiscreteData$rowId,],
                       discrete = thisDiscreteData$observation,
                       fill = discreteFills[[j]],
                       labelPairs = discreteLabelPairs[[j]],
                       title = thisContinuousOutline$QUESTION,
                       discreteTitle = discreteLabel,
                       lLabel = continuousLabels[1],
                       tLabel = continuousLabels[2],
                       rLabel = continuousLabels[3],
                       showMean = showMean)


      plots[[plotIndex]] <- p
      plotIndex = plotIndex + 1

    }
  }

  return(plots)
}


#'function  converts discrete data to type factor and reformats discrete so each
#'observation only contains a single factor level and adds a rowId field so each
#'factor level can be associated with corresponding observations
#'@param discreteData: list containing each discrete data.frame
#'@param outline: dataframe  containing all meta data
#'
formatDiscrete_new <- function(discreteData, outline){
  for(i in 1:length(discreteData)){
    #flatten data so each row has a single factor level, add a rowId field so each
    #level can be associated with original observation
    discreteData[[i]] <- flattenDiscrete(discreteData[[i]]$SET)

    #store the meta data from outline file for this discrete question
    thisOutline <- outline[outline$ID == names(discreteData)[i],]

    #if user has specified this as an ordered factor convert to ordered factor
    #otherwise convert to unordered factor
    if(thisOutline$ORDERED){

      discreteData[[i]]$observation <- factor(x = discreteData[[i]]$observation, levels = unlist(thisOutline$LEVELS),
                                         ordered = TRUE)
    } else {
      discreteData[[i]]$observation <- factor(x = discreteData[[i]]$observation)
    }
  }
  return(discreteData)
}

#reformats discrete so eachobservation only contains a single factor level and adds a rowId field so each
#'@param discreteSet: vector containing all present levels within a discrete data observation
#'
flattenDiscrete <- function(discreteSet){
  observation <- c()
  rowId <- c()
  for(i in 1:NROW(discreteSet)){
    for(ob in unlist(discreteSet[i])){
      observation <- c(observation, ob)
      rowId <- c(rowId, i)
    }
  }
  return(data.frame(observation, rowId))
}


#' read in all discrete data sheets and return as list of dataframes
#' @param outputFolder: path to folder containing outline and datasheets
#' @param outline: outline data sheet containing all question meta data
#' @param subsetIds: character vector of ids specifying subset of questions to read in
#' @param discreteClasses: character vector of which classes may be treated as discrete variables
#'
getDiscreteData <- function(outputFolder, outline, subsetIds, discreteClasses){
  #store discrete data file path to note have as to not repeatedly call paste0
  discreteDataPath <- paste0(outputFolder, "/Data/discreteData.xlsx")


  #find all question IDs which are within specified subset AND
  #within the specified range of discreteClasses
  discreteIds <- filter(outline, ID %in% subsetIds & CLASS %in% discreteClasses)$ID

  #Read in all sheet names to later specify which sheets to read in as data tables
  discreteSheetNames <- readxl::excel_sheets(discreteDataPath)

  #initialize results container
  discreteTables <- list()

  #iterater over each sheet name, if the ID stored within that sheet name is within
  #the discreteIds object, add it to the result container
  for(sheetName in discreteSheetNames){
    sheetId <- str_split(sheetName, " ")[[1]][1]
    if(sheetId %in% discreteIds){
      thisTable <- read_excel(discreteDataPath,
                              sheetName)
      #format comma separated column to list structure
      thisTable$SET <- toList(thisTable$SET)

      discreteTables[[sheetId]] <- thisTable

    }
  }

  return(discreteTables)

}

#' read in all continuous data sheets and return as list of tibbles
#' @param outputFolder: path to folder containing outline and datasheets
#' @param outline: outline data sheet containing all question meta data
#' @param subsetIds: character vector of ids specifying subset of questions to read in
#' @param continuousClasses: character vector of which classes may be read in as continuous variables
#'
getContinuousData <- function(outputFolder, outline, subsetIds, continuousClasses){
  #store continuous data file path to note have as to not repeatedly call paste0
  continuousDataPath <- paste0(outputFolder, "/Data/continuousData.xlsx")

  #find all question IDs which are within specified subset AND
  #within the specified range of continuousClasses
  continuousIds <- filter(outline, ID %in% subsetIds & CLASS %in% continuousClasses)$ID

  #Read in all sheet names to later specify which sheets to read in as data tables
  continuousSheetNames <- readxl::excel_sheets(continuousDataPath)

  #initialize result container
  continuousTables <- list()

  #iterater over each sheet name, if the ID stored within that sheet name is within
  #the continuousIds object, add it to the result container
  for(sheetName in continuousSheetNames){
    sheetId <- str_split(sheetName, " ")[[1]][1]
    if(sheetId %in% continuousIds){
      continuousTables[[sheetId]] <- read_excel(continuousDataPath,
                                                sheetName) %>%
        select(`L`, `T`, `R`)
    }
  }

  return(continuousTables)
}

