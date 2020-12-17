pkgGlobals <- new.env(parent=emptyenv())

pkgGlobals$outPath <- ""

setOutPath <- function(path){
  pkgGlobals$outPath <- path
}

