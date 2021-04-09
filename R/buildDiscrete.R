#'
buildDiscrete <- function(qData, outline, qTitles){
  discreteData <- list()
  for(i in 1:NROW(outline)){
    thisQ <- outline[i,]
    #skip if question is not of type discrete
    if(thisQ$CLASS != "discrete"){
      next
    }

    thisInd <- unlist(thisQ$COL_IND[[1]])
    thisQData <- as.data.frame(qData[,thisInd[1]:thisInd[2]])
    thisTitle <- paste(thisQ$QUESTION, getTitle(thisInd, qTitles))

    firstCol <- thisQData[,1]
    #if is non-checkbox mcq, expand fields,
    if(class(firstCol[1]) == "character"){
      #expand fields into dummy columns
      thisQData <- expandFields(thisQData, thisQ)
    } else{

    }
    #add set field







    #if single choice expand fields, add set field

  }

}

expandFields <- function(thisQData, thisQ){
  labels <- thisQ$LEVELS
  IS_NA <- c()

  qExpanded <- matrix(data = 0, nrow = NROW(thisQData),
                      ncol = length(unlist(thisQ$LEVELS)))
  colnames(qExpanded) <- unlist(thisQ$LEVELS)


  for(i in 1:NROW(thisQData)){

    col <- as.character(thisQData[i,1])
    if(is.na(col)){
      IS_NA <- append(IS_NA, TRUE)
    } else{
      IS_NA <- append(IS_NA, FALSE)
      qExpanded[i,col] <- 1
    }

  }

  qExpanded <- as.data.frame(qExpanded) %>%
    cbind(IS_NA)

  return(qExpanded)

}

addSetField <- function(qData, thisQ){


}
