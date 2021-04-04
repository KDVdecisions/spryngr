



#top level function, user facing

#' Generates folder in working directory containing generated label file and text document with absolute path to input data
#'
#' @param inputFile String containing path to .csv containing input data
#' @param threshold Integer between 0 and 100 which sets the threshold at which two column titles are considered part of the same question
#'
#' @return NULL
#' @export
#'
generateLabelFile <- function(inputFile, threshold=65){
  if(!getWritePermission()){
    cat("Exiting...\n")

  }else{
    rawData <- read.csv(inputFile, check.names=F, na.strings = c("NA",""))
    answers <- rawData[15:length(rawData)]
    questions <- names(answers)[1:(length(names(answers))-1)]

    #Obtain begining and ending column indices of each question
    qInds <- getQIndices(threshold, questions, answers)

    #Classify each question
    qClasses <- classifyQuestions(questions, answers, qInds)

    # #If this set includes marbles, ensure all sub-marbles are within their respective question groups
    # if("Marble" %in% qClasses){
    #   qInds <- gatherMarbles(qInds, qClasses, questions)
    #   qClasses <- classifyQuestions(questions,answers, qInds)
    # }

    qClassesDf <- data.frame(startColIndex = sapply(qInds, function(x) return(x[1])),
                             endColIndex = sapply(qInds, function(x) return(x[2])),
                             qClass = unlist(qClasses)
    )

    answers <- naToLogical(qClassesDf, questions, answers)



    generateOutput(inputFile, qClassesDf, questions, answers)
  }
}
