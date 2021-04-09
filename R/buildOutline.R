
#'creates data.frame which contains outline for collection file to be written
#' out and editable by package user
#'
#' @param qData: collection data set
#' @param qInds: beginning and ending indices for each question within
#'  collection data set
#' @param qTitles: Column headers for collection data set
buildOutline <- function(qData, qInds, qTitles){

  # qIndsChar <- sapply(qInds, function(x){
  #   return(paste(x, collapse = ", "))
  # })

  outline = data.frame(QUESTION = (1:length(qInds)),
                       CLASS = classifyQuestions(qData, qInds, qTitles))
  outline$COL_IND <- qInds

  outline <- addLevelsField(outline, qData, qTitles) %>%
    addOrderedField(qData, qTitles) %>%
    addScaleField(qData, qTitles)




  #return(outline)
}

addLevelsField <- function(outline, qData, qTitles){
  LEVELS <- list()
  for(i in 1:NROW(outline)){
    #current question
    thisQ <- outline[i,]
    #starting column index of current question
    thisInd <- unlist(thisQ$COL_IND)

    #if question is an MCQ/Demographic
    if(thisQ$CLASS == "discrete" || thisQ$CLASS == "marble"){

      #get first column of data
      firstCol <- qData[,thisInd[1]]

      #obtain all labels for data columns
      labels <- getLabels(thisInd, qTitles)

      #if data is in dummy column format
      if(is.numeric(firstCol)){
        #obtain possible levels from headers
        if(thisQ$CLASS == "marble"){
          thisLevels <- getMarbleLabels(thisInd, qTitles)
        } else{
          thisLevels <- labels
        }

      } else if(is.character(firstCol)){
        thisLevels <- as.factor(firstCol) %>%
          levels()
        # if("other" %in% labels){
        #   thisLevels <- c(thisLevels, "other")
        # }
        #thisLevels <- paste(thisLevels, collapse = ", ")
      } else{
        print("Something unexpected happened")
      }
    } else{
      thisLevels <- NA
    }

    #drop NA from levels if it exists
    thisLevels <- thisLevels[thisLevels != "NA"]
    LEVELS[[i]] <- thisLevels
  }
  outline$LEVELS <- LEVELS
  return(outline)

}

addOrderedField <- function(outline, qData, qTitles){
  ORDERED = c()
  for(i in 1:NROW(outline)){
    thisQ <- outline[i,]
    if(thisQ$CLASS == "discrete"){
      ORDERED = c(ORDERED, 0)
    } else{
      ORDERED = c(ORDERED, NA)
    }
  }
  return(cbind(outline, ORDERED))
}

addScaleField <- function(outline, qData, qTitles){
  SCALE = c()
  for(i in 1:NROW(outline)){
    thisQ <- outline[i,]
    if(thisQ$CLASS == "slider"){
      SCALE = c(SCALE, "[0,1]")
    } else if(thisQ$CLASS == "marble"){
      SCALE = c(SCALE, "[0,1], [0,1]")
    } else{
      SCALE = c(SCALE, NA)
    }
  }
  return(cbind(outline, SCALE))
}



