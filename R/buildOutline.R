

buildOutline <- function(qData, qInds, qTitles){

  # qIndsChar <- sapply(qInds, function(x){
  #   return(paste(x, collapse = ", "))
  # })

  outline = data.frame(QUESTION = (1:length(qInds)),
                       CLASS = classifyQuestions(qData, qInds, qTitles))
  outline$COL_IND <- qInds

  #addLevelsField(outline, qData, qTitles)


  return(outline)
}

addLevelsField <- function(outline, qData, qTitles){
  for(i in 1:NROW(outline)){
    #current question
    thisQ <- outline[i,]
    #starting column index of current question
    thisInd <- unlist(thisQ$COL_IND)

    if(thisQ$CLASS == "discrete"){


      firstCol <-qData[,thisInd[1]]



      if(is.integer(firstCol)){
        #obtain possible levels from headers
        levels <- sapply(qTitles[thisInd[1]:thisInd[2]], function(x){
          str_split(x, " - ") %>%
            unlist() %>%
            tail(n = 1) %>%
            trimws(which=c("both"))
        })
        print(paste(levels, collapse = ", "))

      } else if(is.character(firstCol)){

      } else{
        print("Something unexpected happened")
      }
    } else{
      #enter NA,
    }
  }
}



# #TODO
# buildContinuous <- function(qData, qInds){
#   contData <- list()
#   for(i in 1:length(qInds)){
#     #print(qInds[[i]][1])
#     contData[[i]] <- qData[qInds[[i]][1]:qInds[[i]][2]]
#   }
#   print(contData)
# }


