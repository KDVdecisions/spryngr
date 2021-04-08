
#' Returns a character vector corresponding to qInds indicating whether a question
#' is discrete or continuous
#'
#' @param qData: Collection data
#' @param qInds: Question column beginning and ending indexes
#' @param qTitles: Survey data headers
#'
classifyQuestions <- function(qData, qInds, qTitles){
  qClass <- c()
  for(qInd in qInds){
    firstCol <- qData[qInd[1]]

    if(is.numeric(firstCol[,1])){

      if(is.integer(firstCol[,1])){
        qClass <- c(qClass, "discrete")
      } else {
        if(isMarble(qInd, qTitles)){
          qClass <- c(qClass, "marble")
        } else if (isTernary(qInd, qTitles)){
          qClass <- c(qClass, "ternary")
        } else{
          qClass <- c(qClass, "slider")
        }

      }

    }else if(is.character(firstCol[,1])){
      qClass <- c(qClass, "discrete")
    } else{
      print("Something unexpected happened on: ")
      print(paste0("      ", qInd))
    }
  }
  return(qClass)
}


#' Function returns logical based on whether a question is of type Marble
#'
#' @param qInd: a vector containing the beginning and ending index of a question
#' @param qTitles: a vector of question titles
#'
isMarble <- function(qInd, qTitles){
  #if question has more than one column
  if(qInd[2] > qInd[1]){

    #get first two column headers
    startingHeaders <- c(qTitles[qInd[1]], qTitles[(qInd[1] + 1)])

    #get ending label for both column headers
    colLabels <- sapply(startingHeaders, function(x){
      str_split(x, " - ") %>%
        unlist() %>%
        tail(n = 1)
    })

    #if first and second question titles end with X and Y, return true
    if(colLabels[1] == "X" && colLabels[2] == "Y"){
      return(TRUE)
    } else{
      return(FALSE)
    }

  } else{
    return(FALSE)
  }
}

isTernary <- function(qInd, qTitles){
  #get number of columns
  nCols <- qInd[2] - qInd[1]
  #if question has 5-6 columns
  if(nCols >= 5 && nCols < 7){
    #get column ending labels
    colLabels <- getLabels(qInd, qTitles)

    #if 4th element is X and 5th element is Y return true
    if(colLabels[4] == "X" && colLabels[5] == "Y"){
      return(TRUE)
    } else{
      return(FALSE)
    }
  } else{
    return(FALSE)
  }
}




