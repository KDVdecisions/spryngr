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
  labels <- sapply(qTitles[qInd[1]:qInd[2]], function(x){
    str_split(x, " - ") %>%
      unlist() %>%
      getElement(2) %>%
      trimws(which=c("both"))
  }, USE.NAMES = FALSE)

  return(unique(labels))
}

getTernaryLabels <- function(qInd, qTitles){

  labels <- qTitles[qInd[1]:(qInd[1]+2)] %>%
    sapply(function(x){
      str_split(x, " - ") %>%
        unlist() %>%
        getElement(2) %>%
        trimws(which = c("both"))
    }, USE.NAMES = FALSE)

  return(labels)
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
  thisQData <- data.frame(thisQData, check.names = FALSE, IS_NA = rep(NA, NROW(thisQData)))

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


#'scales slider data to fall between a specified lower and upper limit
#'
#'@param slider: question object of type slider
#'@param lower: lower scale limit
#'@param upper: upper scale limit
#'TODO:
normalizeData <- function(data, lower, upper){
  nData <- data
  for(i in 1:(NCOL(data) - 1)){
    nData[,i] <- sapply(data[,i], function(x){
      (upper - lower) * x + lower
    }, USE.NAMES = FALSE)
  }
}


#' Function which saves one or more ggplot objects
#'
#' @param plots a ggplot object or list of ggplot objects
#' @param path a string containing a location to save the plots.  If left blank a location may be selected via a windows file explorer dialogue box
#' @param names a vector of strings the same length as the list of plots.  If left blank random names will be generated
#'
#' @return
#' @export
#'
writePlots <- function(plots, path=NULL, names=NULL, width=NA, height=NA){
  if(is.null(path)){
    path <- choose.dir()
  }

  if(!is.null(names)){
    filesAtPath <- list.files(path) %>%
      str_remove(".png")
    overlap <- names %in% filesAtPath

    if(TRUE %in% overlap){
      warning(sprintf("Files with the name(s): %s already exist in this location",
                      paste(names[overlap], collapse=", ")))
      return()
    }
  }

  #if plots = a single ggplot object
  if(class(plots)[1] == "gg"){
    if(is.null(names)){
      names <- paste0("spryngr_plot_", randString())
    }
    else if(length(names) != 1){
      warning("names vector must be same length as plots argument")
      return()
    }
    ggsave(paste0(path, "/", names, ".png"), plots, width=width, height=heigth)
  }

  #if plots = a list of ggplot objects
  else if(class(plots) == "list"){
    if(is.null(names)){
      names <- replicate(length(plots), randString()) %>%
        lapply(function(x){
          paste0("spryngr_plot_", x)
        }) %>%
        unlist()
    }
    else if(length(names) != length(plots)){
      warning("names vector must be same length as plots argument")
      return()
    }
    for(i in 1:length(plots)){
      ggsave(paste0(path, "/", names[i], ".png"), plots[[i]], width=width, height=height)
    }
  }

}

#' helper function for writePlots() function.  Generates random selection of
#' consonants and numbers of length 5 for file names
#'
randString <- function(){
  chars <- c("1","2","3","4","5","6","7","8","9",
             letters[!letters %in% c("a","e","i","o","u")])
  return(paste(sample(chars, 9, replace=TRUE), collapse=""))

}



