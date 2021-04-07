#TODO
buildContinuous <- function(qData, outline, qTitles){
  continuousQs <- list()
  for(i in 1:length(outline)){
    thisQ <- outline[i,]
    thisInd <- unlist(thisQ$COL_IND[[1]])
    thisQData <- qData[,thisInd[1]:thisInd[2]]
    if(thisQ$CLASS == "slider"){

    } else if(thisQ$CLASS == "ternary"){

    } else if(thisQ$CLASS == "marble"){

    } else{
      print("Something unexpected happend in buildContinuous()")
      return(NULL)
    }
    print(thisQData)
    print("------------------------------")
  }
}
