#'-------
#'Author: Eliot Dixon
#'        KDV Decisions,
#'Date:   2021/5/31
#'-------


#'reads data from each sheet within a .xlsx file
#'
#'@param path: path to a .xslx file
getAllDataSheets <- function(path){
  #organize all discrete data into a list
  wb <- xlsx::loadWorkbook(file = path)
  names <- getSheets(wb) %>%
    names()
  data <- lapply(names, function(x){
    thisDat <- read.xlsx(file = path, sheetName = x, check.names = FALSE)
  })
  return(data)
}

#'Generates a list containing labels to be used for discrete levels within generated
#'plots.  Returns list labelPairs, where the name of each element is a level
#'of a factor from a discrete signifier, and the value of each element is the
#'label for that level which should appear within generated plots.  Labels may
#'be specified within the DISCRETE_LABELS field in the outline file.  Labels
#'should be in respective order of that signifier's levels within the LEVELS field.
#'
#'@param thisOutline: A row from the outline file for a discrete signifier
#'
generateDiscreteLabels <- function(thisOutline){
  labelPairs <- list()

    for(i in 1:length(thisOutline$LEVELS)){
      thisLevel <- unlist(thisOutline$LEVELS[i])
      labelPairs[thisLevel] <- unlist(thisOutline$DISCRETE_LABELS[i])
    }

  return(labelPairs)
}


#'generates x axis label based on question input and outline file
#'@param thisOutline: row from outline representing current question
#'@param index: index representing current column within question
generateXLabel <- function(thisOutline, index){
  base <- "%s <--------------------------> %s"
  leftLabel <- "-X Label"
  rightLabel <- "+X Label"

  if(thisOutline$CLASS == "marble"){
    if(index%%2 != 0){
      leftLabel <- thisOutline$CONTINUOUS_LABELS[[1]][1]
      rightLabel <- thisOutline$CONTINUOUS_LABELS[[1]][2]
    } else{
      leftLabel <- thisOutline$CONTINUOUS_LABELS[[1]][3]
      rightLabel <- thisOutline$CONTINUOUS_LABELS[[1]][4]
    }
  }else if(thisOutline$CLASS == "slider") {
    leftLabel <- thisOutline$CONTINUOUS_LABELS[[1]][1]
    rightLabel <- thisOutline$CONTINUOUS_LABELS[[1]][2]

  }else if(thisOutline$CLASS == "ternary"){
    leftLabel <- paste0("-",thisOutline$CONTINUOUS_LABELS[[1]][index])
    rightLabel <- paste0("+",thisOutline$CONTINUOUS_LABELS[[1]][index])
  }

  label <- sprintf(base, leftLabel, rightLabel)

  return(label)
}


#'Formats discrete data, if n levels of a factor are present within a single
#'observation, separated by commas, data will be split into n observations, each
#'including only a single level.  The field 'ROW_ID' is then added which holds
#'the original row index for each added observation so each row will still be
#'associated with original data within other fields
#'(this process is henceforht referred to as flattening)
#'
#'Data is then converted to a factor, if the user specifies ordered within the
#'ORDERED field in the outline file, the it will be converted to an ordered factor
#'in the order of level present in the LEVELS field within the outline file
#'
#' separated by commas are present in a
#'single observation, this observation will be extended to
#'
#'flattens multiple levels within a single row
#'to multiple rows, adds a ROW_ID column to ensure obsersations are associated
#'with original data within other fields
#'
#'@param data: data for a discrete signifier
#'@param thisOutline: the row within the outline file for this specific discrete
#'signifier
formatDiscrete <- function(data, thisOutline){

  observations <- c()
  indices <- c()
  #convert comma separated values to vectors
  data <- toList(data)

  #flatten all discrete data, save row indices to keep other obs
  for(i in 1:NROW(data)){
    for(ob in unlist(data[i])){
      observations <- c(observations, ob)
      indices <- c(indices, i)
    }
  }

  #convert to factor, ordered if specified in outline
  if(thisOutline$ORDERED){
    observations <- factor(x = observations, levels = unlist(thisOutline$LEVELS),
                           ordered = TRUE)
  } else{
    observations <- factor(x = observations)
  }


  return(data.frame(SET = observations, ROW_ID = indices))

}


#' splits single strings of multiple levels seperated by ', ' into a list of vectors
#' of multiple strings
#' @param data: char vector
toList <- function(data){
  result <- sapply(data, function(x){
    x <- str_split(x, ", ") %>%
      unlist() %>%
      trimws("both")
  }, USE.NAMES = FALSE)

  return(result)
}




