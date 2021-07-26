library(readxl)


plotAllTernary <- function(outputFolder, showMean = TRUE, palette = NULL,
                           writePlots = FALSE, setOutputLocation = FALSE,
                           marblesAsDiscrete = FALSE, subsetIds = NULL) {
  #read in outline file
  outline <- read_excel(paste0(outputFolder, "/Data/outline.xlsx"))


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

  discreteData <- getDiscreteTables(outputFolder, outline, subsetIds, discreteClasses)
  continuousData <- getContinuousTables(outputFolder, outline, subsetIds, continuousClasses)

  print(discreteData)

#
#   #initialize fill container
#   discreteFills <- list()
#   #initialize label pair container
#   discreteLabPairs <- list()
#
#   #Iterate over discrete data, format data for each signifier, generate fills for
#   #each level
#   for(i in 1:length(discreteData)){
#     #Format discrete data
#     discreteData[[i]] <- formatDiscrete(discreteData[[i]]$SET, discreteOutline[i,])
#     #Generate a set of fills for each discrete question
#     nLevels <- length(levels(discreteData[[i]]$SET))
#     discreteFills[[i]] <- colorRampPalette(brewer.pal(8, palette))(nLevels)
#     #Get list of labels to use for each discrete level within plots
#     discreteLabPairs[[i]] <- generateDiscreteLabels(discreteOutline[i,])
#   }


}



#' read in all discrete data sheets and return as list of tibbles
#' @param outputFolder: path to folder containing outline and datasheets
#' @param outline: outline data sheet containing all question meta data
#' @param subsetIds: character vector of ids specifying subset of questions to read in
#' @param discreteClasses: character vector of which classes may be treated as discrete variables
#'
getDiscreteTables <- function(outputFolder, outline, subsetIds, discreteClasses){
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
getContinuousTables <- function(outputFolder, outline, subsetIds, continuousClasses){
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
                                                sheetName)
    }
  }

  return(continuousTables)
}






