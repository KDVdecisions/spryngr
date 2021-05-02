

#(inputFile, qClassesDf, questions, answers)

writeCollectionData <- function(inputFile, outline, continuousData, discreteData){
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
    dir.create(paste0(outputName, "/", "Data"))
    #Write textfile containing path to relevant input file
    writeLines(normalizePath(inputFile), paste0(outputName,"/","Data_Path.txt"))

    outline <- collapseListData(outline)
    continuousData <- lapply(continuousData, collapseListData)
    discreteData <- lapply(discreteData, collapseListData)


    write.xlsx(x = outline, file = paste0(outputName, "/", "Data/outline.xlsx"),
               showNA = TRUE, row.names = FALSE)

    for(i in 1:length(continuousData)){
      name <- gsub("([\\?])","", names(continuousData)[i])

      write.xlsx(x = continuousData[[i]], file = paste0(outputName, "/", "Data/continuousData.xlsx"),
                 sheetName = name, showNA = TRUE,
                 row.names = FALSE, append = TRUE)
    }

    for(i in 1:length(discreteData)){
      name <- gsub("([\\?])","", names(discreteData)[i])


      write.xlsx(x = discreteData[[i]], file = paste0(outputName, "/", "Data/discreteData.xlsx"),
                 sheetName = name, showNA = TRUE,
                 row.names = FALSE, append = TRUE)
    }

    # write.xlsx(x = continuousData, file = paste0(outputName, "/",
    #                                              "Data/continuousData.xlsx"),
    #            showNA = TRUE, row.names = FALSE)
    #
    # write.xlsx(x = discreteData, file = paste0(outputName, "/",
    #                                            "Data/discreteData.xlsx"),
    #            showNA = TRUE, row.names = FALSE)



    #generate label file
    #writeLabelFile(outputName, qClassesDf, questions, answers)

    # write.xlsx(normalizePath(inputFile),"spryng_labels.xlsx", sheetName="Path",
    #            col.names=FALSE, row.names=FALSE, append=TRUE)


  }

}


