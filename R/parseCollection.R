#'Group question data columns into single questions and classify as discrete or continuous
#'
#'@param inputFile: string containing path to spryng output file in csv
parseCollection <- function(inputFile, outlineFile = NULL){
  if(!getWritePermission()){
    cat("Exiting...\n")

  }else{
    rawData <- read.csv(inputFile, check.names=F, na.strings = c("NA",""))
    #clean collection data
    qData <- rawData[15:(length(rawData) - 1)]
    qTitles <- names(qData)[1:(length(names(qData)))]

    #separate columns into groups(questions hereafter) for each question
    qInds <- getQIndices(qTitles, qData)

    #classify groups of questions as continuous or discrete
    classifiedInds <- classifyQuestions(qData, qTitles, qInds)
    contInds <- classifiedInds[[1]]
    discInds <- classifiedInds[[2]]

    buildContinuous(qInds, qData)

   # writeCollectionData(inputFile)
  }

}

#helper, contains code to generate question begining and end indices
getQIndices <- function(qTitles, qData){

  qInds <- list()
  curInd <- c(1)
  j = 1

  #' iterates through and creates list that holds the beginning and end index of each question
  for(i in 2:length(qTitles)){

    curQ <- qTitles[i-1] %>%
      strsplit(" - ")
    curQ <- unlist(curQ)[1]

    nextQ <- qTitles[i] %>%
      strsplit(" - ")
    nextQ <- unlist(nextQ)[1]

    similarity <- compareChars(curQ,nextQ)
    #if next string isn't part of current question
    if(similarity < 65){
      curInd <- c(curInd, i-1)
      qInds[[j]] <- curInd
      curInd <- c(i)
      j=j+1
    }

    if(i == length(qTitles)){ #if on final iteration
      curInd <- c(curInd, i)
      qInds[[j]] <- curInd
      if(similarity < 65){  #if last question is on its own
        qInds[[j]] <- c(i,i)
      }
    }
  }

  return(qInds)

}


#'Compares 2 strings by dividing the first grouping of consecutive shared characters by the total number of characters of the larger string
#'
#'@param str1: first string to be compared
#'@param str2: second string to be compared
#'TODO: maybe rework suppressWarnings call to only apply to length warning
#'
compareChars <- function(str1,str2){
  equalityVector <- c()
  str1 <- strsplit(str1,"")[[1]]
  str2 <- strsplit(str2,"")[[1]]
  nTrue = 0
  equalityVector <- suppressWarnings(str1 == str2)  #might need work to suppress specific length warning
  for(i in 1:length(equalityVector)){
    if(equalityVector[i]){
      nTrue = nTrue + 1
    }
  }
  proportionShared <- (nTrue/length(equalityVector) * 100)
  return(proportionShared)
}


#' Classifies questions as either discrete or continuous
#'
#' @param qData: Survey data
#' @param qTitles: Survey data headers
#' @param qInds: Question column beginning and ending indexes
#'
classifyQuestions <- function(qData, qTitles, qInds){
  continuous <- list()
  discrete <- list()

  for(qInd in qInds){
    firstCol <- qData[qInd[1]]

    if(is.numeric(firstCol[,1])){

      if(is.integer(firstCol[,1])){
          discrete <- append(discrete, list(qInd))
      } else{
          continuous <- append(continuous, list(qInd))
          if(isMarble(qInd, qTitles)){
            discrete <- append(discrete, list(qInd))
          }
      }

    }else if(is.character(firstCol[,1])){
      discrete <- append(discrete, list(qInd))
    } else{
      print("Something unexpected happened on: ")
      print(paste0("      ", qInd))
    }
  }
  return(list(continuous, discrete))
}

#' Function returns logical based on whether a question is of type Marble
#'
#' @param qInds: a vector containing the beginning and ending index of a question
#' @param qTitles: a vector of question titles
#'
#' TODO: simplify, probably don't need qInds, just qTitles.
isMarble <- function(qInds, qTitles){
  if(qInds[2] > qInds[1]){

    #split first question title over ' - ' and get last element
    firstCol <- qTitles[qInds[1]] %>%
      str_split(" - ") %>%
      unlist() %>%
      tail(n = 1)

    #split second question title over ' - ' and get last element
    secondCol <- qTitles[qInds[2]] %>%
      str_split(" - ") %>%
      unlist() %>%
      tail(n = 1)

    #if first and second question titles end with X and Y, return true
    if(firstCol == "X" && secondCol == "Y"){
      return(TRUE)
    } else{
      return(FALSE)
    }

  } else{
    return(FALSE)
  }
}

buildOutline <- function(qInds, qData){

}

buildContinuous <- function(qInds, qData){
  contData <- list()
  for(i in 1:length(qInds)){
    #print(qInds[[i]][1])
    contData[[i]] <- qData[qInds[[i]][1]:qInds[[i]][2]]
  }
  print(contData)
}



