#'miscellaneous helper functions

#' checks if a set of question columns which represent a MCQ/Demographic contain
#' an 'other' free text field
#' include an 'other' field
#' @param qInd: question data column indices
#' @param qTitles: Column headers for collection data set
hasOtherColumn <- function(qInd, qTitles){
  labels <- getLabels(qInd, qTitles)
  if("other" %in% labels){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' checks if a set of question columns contain
#' an 'NA' field
#' include an 'other' field
#' @param qInd: question data column indices
#' @param qTitles: Column headers for collection data set
hasNaColumn <- function(qInd, qTitles){
  labels <- getLabels(qInd, qTitles)
  if("NA" %in% labels){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#'returns a vector containing each label found after the " - " in the data
#'collection column headers
#'
#' @param qInd: question data column indices
#' @param qTitles: Column headers for collection data set
getLabels <- function(qInd, qTitles){
  labels <- sapply(qTitles[qInd[1]:qInd[2]], USE.NAMES = FALSE, function(x){
    str_split(x, " - ") %>%
      unlist() %>%
      tail(n = 1) %>%
      trimws(which=c("both"))
  })
  return(labels)

}


#'returns a vector containing each label for marble questions
#'
#' @param qInd: question data column indices
#' @param qTitles: Column headers for collection data set
getMarbleLabels <- function(qInd, qTitles){
  labels <- sapply(qTitles[qInd[1]:qInd[2]], USE.NAMES = FALSE, function(x){
    str_split(x, " - ") %>%
      unlist() %>%
      getElement(2) %>%
      trimws(which=c("both"))
  })
  print(unique(labels))
  #return(labels)
}

#'function to return sepific element, created to be able to use %>% piping
#'in specific contexts
#'
#'@param data: a vector
#'@param index: index at which to extract element from vector
getElement <- function(data, index){
  return(data[index])
}
