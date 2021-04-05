#'Group question data columns into single questions and classify as discrete or continuous
#'
#'@param inputFile: string containing path to spryng output file in csv
parseCollection <- function(inputFile, outline = NULL){
  if(!getWritePermission()){
    cat("Exiting...\n")

  }else{
    rawData <- read.csv(inputFile, check.names=F, na.strings = c("NA",""))
    #clean collection data
    qData <- rawData[15:(length(rawData) - 1)]
    qTitles <- names(qData)[1:(length(names(qData)))]

    #separate columns into groups(questions hereafter) for each question
    qInds <- getQIndices(qData, qTitles)



    if(is.null(outline)){
      outline <- buildOutline(qData, qInds, qTitles)
    }
    return(outline)
    #build outline table

    #build continuous table

    #build discrete table

    #write
  }

}

#helper, contains code to generate question begining and end indices
getQIndices <- function(qData, qTitles){

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




