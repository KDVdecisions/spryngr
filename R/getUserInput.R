getWritePermission <- function(positive = "Yes", negative = "No"){
  cat(sprintf("This function needs to write to your drive, is this alright?\n1 %s \n2 %s",
              positive, negative))
  if(yesNoLoop()){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}



getOutputFolder <- function(){
  cat(sprintf("By default the output folder is written to the working directory, which is currently set to:\n '%s'\n", getwd()))
  cat(sprintf("Would you like to specify a different location for the output folder?\n1 Specify another location\n2 Write to current working directory"))
  if(yesNoLoop()){
    return(choose.dir())
  }
  else{
    return("")
  }

}


yesNoLoop <- function(positive, negative){
  for(i in 1:6){
    response=readline()
    if(response=="1"){
      return(TRUE)
    }
    else if(response=="2"){
      return(FALSE)
    }
    else{
      cat(sprintf("Please enter either a 1 (%s) or a 2 (%s)\n", positive, negative))
    }
    if(i == 5){
      return(FALSE)
    }
  }
}


