

#(inputFile, qClassesDf, questions, answers)
writeCollectionData <- function(inputFile){
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



    #generate label file
    #writeLabelFile(outputName, qClassesDf, questions, answers)

    # write.xlsx(normalizePath(inputFile),"spryng_labels.xlsx", sheetName="Path",
    #            col.names=FALSE, row.names=FALSE, append=TRUE)


  }

}


