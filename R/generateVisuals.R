
#source("R/generateVisuals_Source.R")

#'Generates question objects from label file and raw data
#' @param outputFolder: String containing path to folder generated from call to identifyQuestions
#' @export
getQuestionObjects <- function(outputFolder){
  labelFile <- paste0(outputFolder, "/", "Question_Labels.xlsx")
  pathFile <- paste0(outputFolder, "/", "Data_Path.txt")
  visualizationFolder <- paste0(outputFolder, "/", "Visualizations")

#TODO: displayInfo
  displayInfo <- TRUE





  #read in data
  rawData <- read.csv(readLines(pathFile), check.names = FALSE)
  answers <- rawData[,15:length(rawData)]

  setClass("Question", representation(type="character", title="character", labels="character", data="data.frame"))
  qTypes <- c("Marble","Slider","MCQ","Triangle","FreeText")

  qObjects <- list()

  #Establish what question types are in label file
  sheetNames <- loadWorkbook(labelFile) %>%
    getSheets() %>%
    names()
  qTypes <- intersect(sheetNames,qTypes)

  #Read label file in and instantiate question objects
  for(type in qTypes){
    qCategory <- read.xlsx(labelFile, sheetName = type)
    for(i in 1:NROW(qCategory)){
      thisQuestion <- qCategory[i,]
      labels <- strsplit(thisQuestion$labels," _ ") %>%
        unlist() %>%
        trimws()
      title <- labels[1]
      labels <- labels[2:length(labels)]
      thisQOb <- new("Question", type=type, labels=labels, title=title,
                     data=data.frame(answers[thisQuestion$startColIndex:thisQuestion$endColIndex],
                                     check.names=FALSE, fix.empty.names=FALSE))
      qObjects[[type]] <- append(qObjects[[type]], thisQOb)

    }
  }



  for(i in 1:length(qObjects$MCQ)){
    if(isCheckboxMcq(qObjects$MCQ[[i]])){
      qObjects$MCQ[[i]]<- collapseMcq(qObjects$MCQ[[i]])
    }

  }

  return(qObjects)
}



#
# #All sliders vs all MCQs
# dyPlots <- plotAllDyads(qObjects$Slider, qObjects$MCQ)
#
#
# #first triangle vs all MCQs
# triPlots <- plotAllTernaries(qObjects$Triangle[1], qObjects$MCQ)
#
# #first marble vs first MCQ
# marPlots <- plotAllMarbles(qObjects$Marble[1], qObjects$MCQ[1])
#
#
#
#
# plotTernary(qObjects$Triangle[[1]], qObjects$MCQ[[6]], varIndex=3, xlab=c("a non-vital part of the business", "a vital part of the business"))


#
#
