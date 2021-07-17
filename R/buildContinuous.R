#'Build the continuous data file, an .xlsx file in which all continuous signifier
#'data is stored, this file contains a sheet for each continuous signifier
#'@param outline: The outline data.frame
#'@param qData: All collection response data
#'@param qTitles: Titles for each signifier
#'
buildContinuous <- function(qData, outline, qTitles){
  continuousData <- list()
  for(i in 1:NROW(outline)){
    thisQ <- outline[i,]
    #skip if question is of type discrete
    if(thisQ$CLASS == "discrete"){
      next
    }

    thisInd <- unlist(thisQ$COL_IND[[1]])
    thisQData <- qData[,thisInd[1]:thisInd[2]]
    thisTitle <- thisQ$QUESTION

    #if this question has an NA field, remove it and add a newly generated one
    if(hasNaField(thisInd, qTitles)){
      thisQData <- qData[,thisInd[1]:(thisInd[2] - 1)]
    }
    thisQData <- addNaField(thisQData)


    #generate titles based on question class
    if(thisQ$CLASS == "slider"){
      #write X, NA
      names(thisQData) <- c("X", "IS_NA")
      #add y column (just 1-x)
      thisQData$Y <- (1-thisQData$X)
      thisQData <- select(thisQData, "X", "Y", "IS_NA")


      continuousData <- append(continuousData, list(thisQData))

    } else if(thisQ$CLASS == "ternary"){
      #write L,T,R, NA
      thisQData <- select(thisQData, 1:3, NCOL(thisQData))
      names(thisQData) <- c("L", "T", "R", "IS_NA")
      continuousData <- append(continuousData, list(thisQData))
      #continuousData[[i]] <- thisQData

    } else if(thisQ$CLASS == "marble"){
      #write X,Y, NA
      nPairs = (NCOL(thisQData) - 1)/2
      names(thisQData) <- c(rep(c("X", "Y"), nPairs), "IS_NA") %>%
        make.unique()
      continuousData <- append(continuousData, list(thisQData))
      #continuousData[[i]] <- thisQData
    } else{
      print("something unexpected happend in build continuous")
    }
    #name list element
    names(continuousData)[length(continuousData)] <- thisTitle

  }
 return(continuousData)
}

