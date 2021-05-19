#'
buildDiscrete <- function(qData, outline, qTitles){
  discreteData <- list()
  for(i in 1:NROW(outline)){
    thisQOutline <- outline[i,]
    #skip if question is not of type discrete
    if(!(thisQOutline$CLASS %in% c("discrete", "marble"))){
      next
    }

    thisInd <- unlist(thisQOutline$COL_IND[[1]])
    thisQData <- as.data.frame(qData[,thisInd[1]:thisInd[2]])
    thisTitle <- thisQOutline$QUESTION
    firstCol <- thisQData[,1]

    if(thisQOutline$CLASS == "marble"){
      thisQData <- expandMarbles(thisQData, thisInd, qTitles)
    } else if(class(firstCol[1]) == "character"){
      #expand fields into dummy columns
      thisQData <- expandFields(thisQData, thisQOutline)
      #data already in dummy columns, format
    } else{
      names(thisQData) <- getLabels(thisInd, qTitles)


      if(hasNaField(thisInd, qTitles)){
        thisQData <- thisQData[,1:(NCOL(thisQData) - 1)]
      }


      #add set column containing all selected levels per observation
      thisQData <- addNaField(thisQData) %>%
        replace(is.na(.), 0)



    }
    thisQData <- addSetField(thisQData, thisQOutline)
    discreteData <- append(discreteData, list(thisQData))


    names(discreteData)[length(discreteData)] <- thisTitle
  }


return(discreteData)
  #print(discreteData)

}

addSetField <- function(thisQData, thisQOutline){
  labels <- names(thisQData)
  SET <-  list()
  for(i in 1:NROW(thisQData)){
    if(thisQData[i,]$IS_NA){
      SET <- append(SET, NA)
    } else{
      SET <- append(SET, list(labels[thisQData[i,] != 0]))
    }
  }
  thisQData$SET <- SET

  thisQData <- relocate(thisQData, SET, IS_NA, .after = everything())

  return(thisQData)
}

#'
expandFields <- function(thisQData, thisQOutline){
  labels <- thisQOutline$LEVELS
  IS_NA <- c()

  qExpanded <- matrix(data = 0, nrow = NROW(thisQData),
                      ncol = length(unlist(thisQOutline$LEVELS)))

  colnames(qExpanded) <- unlist(thisQOutline$LEVELS)

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

expandMarbles <- function(thisQData, thisInd, qTitles){
  #drop native NA col
  if(hasNaField(thisInd, qTitles)){
    thisQData <- thisQData[,1:(NCOL(thisQData) - 1)]
  }
  thisQData <- thisQData[,seq(1, NCOL(thisQData), 2)]
  names(thisQData) <- getMarbleLabels(thisInd, qTitles)

  IS_NA <- c()
  thisQData[!is.na(thisQData)] <- 1
  thisQData[is.na(thisQData)] <- 0

  isZero <- thisQData == 0
  for(i in 1:NROW(thisQData)){
    if(!(FALSE %in% isZero[i,])){
      IS_NA <- c(IS_NA, TRUE)
    } else{
      IS_NA <- c(IS_NA, FALSE)
    }
  }
  thisQData <- cbind(thisQData, IS_NA)

  return(thisQData)
  #print(thisQData)
}

