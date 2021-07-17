
#'creates data.frame which contains outline for collection file to be written
#' out and editable by package user
#'
#' @param qData: collection data set
#' @param qInds: beginning and ending indices for each question within
#'  collection data set
#' @param qTitles: Column headers for collection data set
buildOutline <- function(qData, qInds, qTitles){

  allTitles <- sapply(qTitles, USE.NAMES = FALSE, function(x){
    str_split(x, " - ") %>%
      unlist() %>%
      head(n = 1) %>%
      trimws(which = c("both"))
  }) %>%
    unique()

  outline = data.frame(ID = as.character(1:length(qInds)), QUESTION = allTitles,
                       CLASS = classifyQuestions(qData, qInds, qTitles))
  outline$COL_IND <- qInds

  outline <- addLevelsField(outline, qData, qTitles) %>%
    addOrderedField(qData, qTitles) %>%
    addScaleField(qData, qTitles) %>%
    addLabelFields(qData, qTitles)
  return(outline)
}

#'addLabelFields() adds the DISCRETE_LABELS and CONTINUOUS_LABELS fields to the
#'outline data.frame
#'
#'@param outline: The outline data.frame
#'@param qData: All collection response data
#'@param qTitles: Titles for each signifier
addLabelFields <- function(outline, qData, qTitles){
  continuousLabels <- list()
  discreteLabels <- list()

  for(i in 1:NROW(outline)){
    thisQ <- outline[i,]
    thisInd <- unlist(thisQ$COL_IND)
    #set both to NA and reassign as needed
    thisContinuousLab <- NA
    thisDiscreteLab <- NA

    if(thisQ$CLASS == "slider"){
      thisContinuousLab <- c("-X Label", "+X Label")

    } else if(thisQ$CLASS == "ternary"){
      thisContinuousLab <- getTernaryLabels(thisInd, qTitles)

    } else if(thisQ$CLASS == "discrete"){
      thisDiscreteLab <- unlist(thisQ$LEVELS)

    } else if(thisQ$CLASS == "marble"){
      thisContinuousLab <- c("-X Label", "+X Label" , "-Y Label", "+Y Label")
      thisDiscreteLab <- unlist(thisQ$LEVELS)
    } else {
      print("something unexpected happend in addLabelsFields()")
    }
    continuousLabels[[i]] <- thisContinuousLab
    discreteLabels[[i]] <- thisDiscreteLab
  }

  outline$CONTINUOUS_LABELS <- continuousLabels
  outline$DISCRETE_LABELS <- discreteLabels

  return(outline)

}

#'Determines the levels included for each discrete signifier and adds them for each
#'response within the outline document under the LEVELS field
#'
#'@param outline: The outline data.frame
#'@param qData: All collection response data
#'@param qTitles: Titles for each signifier
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
      } else{
        print("Something unexpected happened BO_1")
      }
    } else if(thisQ$CLASS == "ternary"){
      thisLevels <- NA #getTernaryLabels(thisInd, qTitles)


    } else if(thisQ$CLASS == "slider"){
      thisLevels = NA #c("left lab", "right lab")
    } else{
      print("Something unexpected happend BO_2")
    }

    #drop NA from levels if it exists
    thisLevels <- thisLevels[thisLevels != "NA"]
    LEVELS[[i]] <- thisLevels %>%
      sort()
  }
  outline$LEVELS <- LEVELS
  return(outline)

}


#' Adds a binary field within the ouline document which dictates whether a
#' discrete signifier should be treated as an ordered or unordered factor
#'@param outline: The outline data.frame
#'@param qData: All collection response data
#'@param qTitles: Titles for each signifier
addOrderedField <- function(outline, qData, qTitles){
  ORDERED = c()
  for(i in 1:NROW(outline)){
    thisQ <- outline[i,]
    if(thisQ$CLASS %in% c("discrete", "marble")){
      ORDERED = c(ORDERED, 1)
    } else{
      ORDERED = c(ORDERED, NA)
    }
  }
  return(cbind(outline, ORDERED))
}

#'Adds the range scale field to the outline document which allows users to specify
#'the range on which sliders and marble axis should operate
#'@param outline: The outline data.frame
#'@param qData: All collection response data
#'@param qTitles: Titles for each signifier
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






