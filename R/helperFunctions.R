#'miscellaneous helper functions

#' checks if a set of question columns which represent a MCQ/Demographic contain
#' an 'other' free text field
#' include an 'other' field
#' @param qInd: question data column indices
#' @param qTitles: Column headers for collection data set
hasOtherField <- function(qInd, qTitles){
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
hasNaField <- function(qInd, qTitles){
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

getTitle <- function(qInd, qTitles){
  title <- str_split(qTitles[qInd[1]], " - ") %>%
    unlist() %>%
    head(n = 1) %>%
    trimws(which=c("both"))

  return(title)
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
  return(unique(labels))
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

#'Generates and appends a logical field indicating if all other fields within a
#'given row are NA to a question
#'
#'@param thisQData: collection data for a single question
addNaField <- function(thisQData){
  thisQData <- data.frame(thisQData, IS_NA = rep(NA, NROW(thisQData)))

  for(i in 1:NROW(thisQData)){
    hasNa <- sapply(thisQData[i,], USE.NAMES = FALSE, function(x){
      if(is.na(x)){
        return(TRUE)
      } else{
        return(FALSE)
      }
    })

    if(FALSE %in% hasNa){
      thisQData[i,NCOL(thisQData)] <- FALSE
    } else{
      thisQData[i,NCOL(thisQData)] <- TRUE
    }

  }
  return(thisQData)

}

collapseListData <- function(data){
  for(i in 1:NCOL(data)){
    if(class(data[,i]) == "list"){
      data[,i] <- sapply(data[,i], function(x){
        if(FALSE %in% is.na(x)){
          return(paste(x, collapse = ", "))
        } else{
          return(NA)
        }

      })
    }
  }

  return(data)
}



