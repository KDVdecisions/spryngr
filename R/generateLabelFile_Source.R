#'Compares 2 strings by dividing the first grouping of consecutive shared characters by the total number of characters of the larger string
#'
#'@param str1: first string to be compared
#'@param str2: second string to be compared
#'TODO: maybe rework suppressWarnings call to only apply to length warning
#'
compareChars <- function(str1,str2){
  equalityVector <- c()
  str1 <- strsplit(str1,"")[[1]]
  str2 <- strsplit(str2,"")[[1]]
  nTrue = 0
  equalityVector <- suppressWarnings(str1 == str2)  #might need work to suppress specific length warning
  for(i in 1:length(equalityVector)){
    if(equalityVector[i]){
      nTrue = nTrue + 1
    }
  }
  proportionShared <- (nTrue/length(equalityVector) * 100)
  return(proportionShared)
}

#'Iterates over list of questions along with question column indices and classifies each question
#'
#'@param questions: char vector containing all questions column titles
#'@param qInds: Beginning and end index of each question column
#'
#'Notes: The order in which these logical helper functions are called is important to the functionality of this program
classifyQuestions <- function(questions, answers, qInds){
  qClasses <- list() #result container

  for(i in 1:length(qInds)){
    columns <- questions[qInds[[i]][1]:qInds[[i]][2]]  #gather column names
    #remove NA Cols
    colSplit <- strsplit(columns[length(columns)]," ")
    if(colSplit[[1]][length(colSplit[[1]])]=="NA"){
      columns <- columns[1:(length(columns)-1)]
    }

    if(isTriangle(columns)){
      qClasses[i] <- "Triangle"
    }
    else if(isSlider(columns)){
      qClasses[i] <- "Slider"
    }
    else if(isMarble(columns)){
      qClasses[i] <- "Marble"
    }
    else if(isMcq(columns, answers)){
      qClasses[i] <- "MCQ"
    }
    else if(isFreeText(columns)){
      qClasses[i] <- "FreeText"
    }
    else{
      qClasses[i] <- "TBD"
    }

  }#for loop
  return(qClasses)
}




#'Determines if given set column names for a question describes a triangle signifier returns logical type
#'Helper to classifyQuestions()
#'
#'@param columns: list column names which describe a given question
#'
isTriangle <- function(columns){
  if(length(columns)==5){
    colSplit <- strsplit(columns," ")
    lastTwo <- paste(colSplit[[4]][length(colSplit[[4]])],
                     colSplit[[5]][length(colSplit[[5]])],sep=",")
    if(lastTwo == "X,Y"){
      return(TRUE)
    } #if has five columns (NA col has been dropped if it exists) and last two columns end in X and Y it's a triangle
  }
  else{
    return(FALSE)
  }

}

#'Determines if given set column names for a question describes a marble signifier returns logical type
#'Helper to classifyQuestions()
#'
#'@param columns: list column names which describe a given question
#'
isMarble <- function(columns){
  #if number of columns is even
  if(length(columns)%%2 == 0){
    for(i in 1:length(columns)){
      colSplit <- strsplit(columns[i]," ")
      if(i%%2 == 1){  #if on odd index
        if(colSplit[[1]][length(colSplit[[1]])]!="X"){ #if last char != X
          return(FALSE)
        }
      }
      else if(i%%2 == 0){#if on even index
        if(colSplit[[1]][length(colSplit[[1]])] != "Y"){ #if last char != Y
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


#'Determines if given set column names for a question describes a slider signifier returns logical type
#'Helper to classifyQuestions()
#'
#'@param columns: list column names which describe a given question
#'
isSlider <- function(columns){
  if(length(columns)==1){
    colSplit <- strsplit(columns," ")
    if(colSplit[[1]][length(colSplit[[1]])]=="X"){ #if length of columns = 1 and last character in col name is X it's a slider
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    return(FALSE)
  }
}


#'Determines if given set column names for a question describes a multiple choice question.  returns a logical
#'Helper to classifyQuestions()
#'
#'@param columns: list column names which describe a given question
#'
isMcq <- function(columns, answers){
  if(length(columns)>1){
    return(TRUE)
  }
  else{
    answersComplete <- answers[[columns]]
    answersComplete <- answersComplete[!is.na(answers[[columns]]) & answers[[columns]] != ""]
    nIdentical <- 0
    for(i in 1:length(answersComplete)){
      equalityVector <- answersComplete[i] == answersComplete[-i]
      currentN <- length(equalityVector[equalityVector==TRUE])
      nIdentical = nIdentical + currentN
      if(nIdentical >= length(answersComplete)*0.25){
       return(TRUE)
      }
    }

  }
  return(FALSE)
}

#this needs to be more than returns true probably.....
isFreeText <- function(columns){
  # if(length(columns < 2)){
  #
  #   print(answers[columns])
  # }
  return(TRUE)

}

#'Returns corrected qInd object
#'Helper for gatherMarbles()
#'
#'@param qInds: set of original question indices
#'@param marbleInds: list in which each element contains start and end indices
#'                   for marble signifiers which need to be gathered
#'@param naInds: Indices of NA columns for marble signifiers which have been incorrectly identified
#'
updateQuestionInds <- function(qInds,marbleInds,naInds){
    lims <- c()
    qInds <- unlist(qInds)
    for(i in 1:length(marbleInds)){
      lims <- c(lims,(marbleInds[[i]][1] + 1):(marbleInds[[i]][2] - 1))
    }
    qIndsUpdated <- qInds[!(qInds %in% lims)]

    if(length(naInds) > 0){
      qIndsUpdated <- qIndsUpdated[-match(naInds,qIndsUpdated)]
    }
    qIndsUpdated <- reshapeInds(qIndsUpdated)
    return(qIndsUpdated)
 }


#'takes a flat vector of indices and generates a list, each element containing
#'the beginning and ending indices for a set of question indices
#'helper for gatherMarbles()
#'
#'@param qLimits: flat vector containing beginning and ending elements of marble signifiers
#'
reshapeInds <- function(qLimits){
   output <- list()
   curInds <- c()
   j <- 1
   for(i in 1:length(qLimits)){
     curInds <- c(curInds,qLimits[i])
     if(i%%2 == 0){
       output[[j]] <- curInds
       j <- j + 1
       curInds <- c()
     }
   }
   return(output)
 }



#'Corrects qInds by examning if a single marble signifier has been split into
#'several questions due to dissimilar column titles
#'
#'@param qInds: list in which each element contains start and end column indices
#'              of each question
#'@param qClasses: List of classifications for each question
#'
#'TODO: try to simplify algorithm.  Not a particuarly elegant solution even if it does work.
gatherMarbles <- function(qInds, qClasses, questions){
  marbleFlag <- FALSE
  marbleInds <- list()
  naInds <- c()

  for(i in 1:(length(qClasses)-1)){
    #If current and next question classes are marbles
    if(qClasses[i] == "Marble" && qClasses[i+1] == "Marble"){
      curQ <- strsplit(questions[qInds[[i]][1]]," - ")
      nextQ <- strsplit(questions[qInds[[i+1]][1]]," - ")
      #If we are at the first pair of consecutive marbles mark first index of Q and set flag to TRUE
      if(curQ[[1]][1] == nextQ[[1]][1]){
        if(!marbleFlag){
          marbleInds <- append(marbleInds,qInds[[i]][1])
          marbleFlag <- TRUE
        }
        #if at last question
        if(i+1 == length(qClasses) && marbleFlag){
          marbleInds <- append(marbleInds,qInds[[i+1]][2])
        }
      }
    }

    #If we are at the last marble from a consecutive group
    else if(qClasses[i] == "Marble" && marbleFlag){
      #split next question after marble
      endQ <- questions[qInds[[i+1]][1]] %>%
        strsplit(" - ") %>%
        unlist()
      #if next question ends in NA include in gathered marble index
      if(endQ[length(endQ)] == " NA"){
        naInds <- c(naInds,qInds[[i+1]][1])
        marbleInds <- append(marbleInds, qInds[[i+1]][2])
        marbleFlag <- FALSE
      }
      else{
        marbleInds <- append(marbleInds,qInds[[i]][2])
        marbleFlag <- FALSE
      }
    }
  }
  marbleInds <- reshapeInds(unlist(marbleInds))
  if(length(marbleInds) > 0){
    return(updateQuestionInds(qInds, marbleInds,naInds))
  }
  else{
    return(qInds)
  }

}#gatherMarbles

getQNames <- function(qClassesDf, questions){
  qNames <- c()

  for(startInd in qClassesDf$startColIndex){
    thisName <- strsplit(questions[startInd]," - ")[[1]][1] %>%
      trimws(which=c("both"))
    qNames <- c(qNames,thisName)
  }
  return(qNames)
}


generateLabels <- function(qClassesDf, questions, answers){
  qNames <- getQNames(qClassesDf, questions)
  labels <- list()
  for(i in 1:NROW(qClassesDf)){
    labels[[i]] <-  switch(qClassesDf[i,]$qClass,
                           "Triangle"=c(qNames[i],triangleLabel(questions[qClassesDf[i,]$startColIndex:
                                                                             (qClassesDf[i,]$startColIndex+2)])),

                           "Slider"=c(qNames[i],"-X Label","+X Label"),

                           "Marble"=c(qNames[i],marbleLabel(questions[qClassesDf[i,]$startColIndex:
                                                                        qClassesDf[i,]$endColIndex]),
                                      "-X Label", "+X Label", "-Y Label", "+Y Label"),
                           "MCQ"=c(qNames[i],

                                   mcqLabel(questions[qClassesDf[i,]$startColIndex:qClassesDf[i,]$endColIndex],
                                         answers,
                                         qClassesDf[i,]$startColIndex)



                                   ),
                           "FreeText"=c(qNames[i])
    )

  }
return(labels)
}


marbleLabel <- function(marble){
  marbleLabs <- c()
  for(i in 1:length(marble)){
    if(i%%2 == 0){
      thisLab <- unlist(strsplit(marble[i], " - "))[2] %>%
        trimws(which=c("both"))
      marbleLabs <- c(marbleLabs, thisLab)
    }else{
      next
    }

  }
  return(marbleLabs)
}

triangleLabel <- function(triangle){
  triLabels <- lapply(triangle, function(x){
    spltTriangle <- trimws(x, which=c("both")) %>%
      strsplit(" - ") %>%
      unlist()
    return(spltTriangle[length(spltTriangle)])
  })
  return(unlist(triLabels))
}


mcqLabel <- function(mcq, answers, index){
  levels <- levels(as.factor(answers[,index]))
  if(length(levels) <= 1){
    mcqLabels <- sapply(mcq, USE.NAMES=FALSE, function(x){
      thisLabel <- strsplit(x, " - ") %>%
        unlist() %>%
        trimws(which="left")
      return(thisLabel[length(thisLabel)])
    })
    return(unlist(mcqLabels))
  }

  return(levels)
}



#helper, contains code to generate question begining and end indices
getQIndices <- function(threshold, questions, answers){

  qInds <- list()
  curInd <- c(1)
  j = 1

  #' iterates through and creates list that holds the beginning and end index of each question
  for(i in 2:length(questions)){
    curQ <- questions[i-1] %>%
      strsplit(" - ")
    curQ <- unlist(curQ)[1]

    nextQ <- questions[i] %>%
      strsplit(" - ")
    nextQ <- unlist(nextQ)[1]
    similarity <- compareChars(curQ,nextQ)
    if(similarity < threshold){   #if next string isn't part of current question
      curInd <- c(curInd, i-1)
      qInds[[j]] <- curInd
      curInd <- c(i)
      j=j+1
    }

    if(i == length(questions)){ #if on final iteration
      curInd <- c(curInd, i)
      qInds[[j]] <- curInd
      if(similarity < threshold){  #if last question is on its own
        qInds[[j]] <- c(i,i)
      }
    }
  }

  return(qInds)

}

#helper function, converts NA columns within answers to logical type
naToLogical <- function(qClassesDf, questions, answers){
  for(i in 1:NROW(qClassesDf)){
    lastCol <- unlist(strsplit(questions[qClassesDf[i,]$endColIndex]," - ")) %>%
      str_remove_all(" ")
    if(lastCol[length(lastCol)] == "NA"){
      answers[qClassesDf[i,]$endColIndex] <- sapply(answers[qClassesDf[i,]$endColIndex], function(x){
        sapply(x, function(x){
          if(is.na(x)){
            return(0)
          }else{
            return(1)
          }
        })
      })
    }
  }
  return(answers)
}


generateOutput <- function(inputFile, qClassesDf, questions, answers){
  #Isolate input file name
  inputName <- strsplit(inputFile, "/") %>%
    unlist()
  inputName <- inputName[length(inputName)] %>%
    strsplit("[.]")
  inputName <- unlist(inputName)[1]

  outputFolder <- getOutputFolder()
  outputName <- paste0(getwd(),"/",inputName,"_output")

  if(outputFolder == ""){
    cat(sprintf("Writing %s\n",outputName))
  }
  else{
    outputName <- paste0(outputFolder,"\\",inputName,"_output")
    cat(sprintf("Writing %s\n",gsub("\\\\", "/",outputName)))
  }

  if(file.exists(outputName)){
    warning("An output folder with this filename already exists")
    cat("Exiting... \n")
    return()
  }else{
    dir.create(outputName)
    #create viz directory

    dir.create(paste0(outputName, "/", "Visualizations"))
    dir.create(paste0(outputName, "/", "Visualizations", "/", "Triangles"))
    dir.create(paste0(outputName, "/", "Visualizations", "/", "Sliders"))
    dir.create(paste0(outputName, "/", "Visualizations", "/", "Marbles"))
    #Write textfile containing path to relevant input file
    writeLines(normalizePath(inputFile), paste0(outputName,"/","Data_Path.txt"))

    #generate label file
    writeLabelFile(outputName, qClassesDf, questions, answers)

    # write.xlsx(normalizePath(inputFile),"spryng_labels.xlsx", sheetName="Path",
    #            col.names=FALSE, row.names=FALSE, append=TRUE)


  }

}


writeLabelFile <- function(outputName, qClassesDf, questions, answers){
  qTypes <- c("Marble","Slider","MCQ","Triangle","FreeText")
  qName <- getQNames(qClassesDf, questions)
  indices <- cbind(qClassesDf,qName)


  labelFile <- paste0(outputName,"/", "Question_Labels.xlsx")


  #write out indices sheet
  write.xlsx(indices, labelFile, sheetName="Indices",
             row.names=FALSE)



  #'Iterate over each label, unlist group of labels, trim whitespace,
  #'encase individual labels in single quotes,
  #'Concattenate each vector of labels to single string,
  #'write each of these strings out to xlsx file
  #'
  for(qType in qTypes){
    thisType <- qClassesDf[qClassesDf$qClass == qType,]

    if(NROW(thisType) > 0){

      #returns all labels for the questions of this type
      labels <- generateLabels(thisType, questions, answers)
      #iterates over groups of labels for each question
      labels <- sapply(labels, function(i){
        label <- unlist(i) %>%
          trimws() %>%
          paste(collapse=" _ ")
        #paste(thisQLabels, collapse=", ")
      })

      thisType$labels <- labels
      write.xlsx(thisType, labelFile,sheetName=qType,
                 row.names=FALSE, append=TRUE)
    }

  }
}



