
#' @export
identifyQuestions <- function(inputFile, threshold=65){
  rawData <- read.csv(inputFile, check.names=F, na.strings = c("NA",""))
  qInds <- list()
  curInd <- c(1)
  j = 1

  answers <- rawData[15:length(rawData)]
  questions <- names(answers)[1:(length(names(answers))-1)]


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




  qClasses <- classifyQuestions(questions, answers, qInds)


  if("Marble" %in% qClasses){
    qInds <- gatherMarbles(qInds, qClasses, questions)
    qClasses <- classifyQuestions(questions,answers, qInds)
  }

  qClassesDf <- data.frame(startColIndex = sapply(qInds, function(x) return(x[1])),
                           endColIndex = sapply(qInds, function(x) return(x[2])),
                           qClass = unlist(qClasses)
  )

  #Convert NA columns to logical
  for(i in 1:NROW(qClassesDf)){
    lastCol <- unlist(strsplit(questions[qClassesDf[i,]$endColIndex],"-")) %>%
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

  generateOutput(inputFile, qClassesDf, questions, answers)
}#identify questions



#identifyQuestions("employee_experiences_12Nov2019.csv")

















#--------------qInds ends here----------



#The below code chunk which generates qNames needs to be implemented into q() function
#if(some file doesn't exist):

#Generate labels()


#-----------------Output Generation----------------------------------



generateOutput <- function(inputFile, qClassesDf, questions, answers){
  #Isolate input file name
  inputName <- strsplit(inputFile, "/") %>%
    unlist()
  inputName <- inputName[length(inputName)] %>%
    strsplit("[.]")
  inputName <- unlist(inputName)[1]

  outputName <- paste0(inputName,"_output")


  if(file.exists(outputName)){
    warning("An output folder with this filename already exists")
    return()
  }else{
    dir.create(outputName)
    #create viz directory

    dir.create(paste0(outputName, "/", "Visualizations"))
    #Write textfile containing path to relevant input file
    writeLines(file_path_as_absolute(inputFile), paste0(outputName,"/","Data_Path.txt"))

    #generate label file
    generateLabelFile(outputName, qClassesDf, questions, answers)

    # write.xlsx(file_path_as_absolute(inputFile),"spryng_labels.xlsx", sheetName="Path",
    #            col.names=FALSE, row.names=FALSE, append=TRUE)


  }

}


generateLabelFile <- function(outputName, qClassesDf, questions, answers){
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


#
# generateOutputFolder <- function(inputFile, inputName, qClassesDf){
#
#
#   outputName <- paste0(inputName,"_output")
#
#
#   if(file.exists(outputName)){
#     warning("An output folder with this filename already exists")
#     return()
#   }else{
#     dir.create(outputName)
#     #create viz directory
#
#     dir.create(paste0(outputName, "/", "Visualizations"))
#     #Write textfile containing path to relevant input file
#     writeLines(file_path_as_absolute(inputFile), paste0(outputName,"/","Data_Path.txt"))
#
#     #generate label file
#     generateLabelFile(outputName, qClassesDf, questions)
#
#     # write.xlsx(file_path_as_absolute(inputFile),"spryng_labels.xlsx", sheetName="Path",
#     #            col.names=FALSE, row.names=FALSE, append=TRUE)
#
#
#   }
#
# }

