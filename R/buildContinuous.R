#TODO
buildContinuous <- function(qData, outline, qTitles){
  continuousData <- list()
  for(i in 1:NROW(outline)){
    thisQ <- outline[i,]
    thisInd <- unlist(thisQ$COL_IND[[1]])
    thisQData <- qData[,thisInd[1]:thisInd[2]]

    if(!hasNaColumn(thisInd, qTitles)){
      #add an NA column
      thisQData <- addNaField(thisQData)

    } else{
      #convert NA to logical
      thisQData[,NCOL(thisQData)] <- sapply(thisQData[,NCOL(thisQData)],
                                            USE.NAMES = FALSE, function(x){
                                              if(is.na(x)){
                                                return(FALSE)
                                              }else if(x == "True"){
                                                return(TRUE)
                                              }
                                            })

    }

    if(thisQ$CLASS == "slider"){
      #write X, NA
      names(thisQData) <- c("X", "IS_NA")
      continuousData[[i]] <- thisQData



    } else if(thisQ$CLASS == "ternary"){
      #write L,T,R, NA
      thisQData <- select(thisQData, 1:3, NCOL(thisQData))
      names(thisQData) <- c("L", "T", "R", "IS_NA")
      continuousData[[i]] <- thisQData

    } else if(thisQ$CLASS == "marble"){
      #write X,Y, NA
      nPairs = (NCOL(thisQData) - 1)/2
      names(thisQData) <- c(rep(c("X", "Y"), nPairs), "IS_NA")
      continuousData[[i]] <- thisQData

    }
  }
  print(continuousData)
}

addNaField <- function(thisQData){
  thisQData <- data.frame(thisQData, IS_NA = rep(NA, NROW(thisQData)))


  for(i in 1:NROW(thisQData)){
    thisRow <-
    hasNA <- sapply(thisQData[i,], USE.NAMES = FALSE, function(x){
      if(is.na(x)){
        return(TRUE)
      }else{
        return(FALSE)
      }
    })
    #if no FALSE in hasNA then it's an NA row
    if(!(FALSE %in% hasNA)){
      thisQData[i,]$IS_NA <- TRUE
    } else{
      thisQData[i,]$IS_NA <- FALSE
    }

  }
  return(thisQData)

}
