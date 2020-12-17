getWritePermission <- function(){
  cat("This function needs to write to your drive, is this alright?\n1 Yes\n2 No")
  if(yesNoLoop()){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}



getOutputFolder <- function(){
  cat(sprintf("By default the output folder is written to the working directory, which is currently set to:\n '%s'\n", getwd()))
  cat("Would you like to specify a different location for the output folder?\n1 Yes\n2 No")
  if(yesNoLoop()){
    return(choose.dir())
  }
  else{
    return("")
  }

}


yesNoLoop <- function(){
  for(i in 1:6){
    response=readline()
    if(response=="1"){
      return(TRUE)
    }
    else if(response=="2"){
      return(FALSE)
    }
    else{
      cat("Please enter either a 1 (Yes) or a 2 (no)\n")
    }
    if(i == 5){
      return(FALSE)
    }
  }
}


