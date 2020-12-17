#' pkgGlobals is an environment which exists in the namespace of the spryngr package.
#'
#' pkgGlobals$outPath is a global variable in the package namespace which holds the
#' location of the output folder from which the question objects are being generated

pkgGlobals <- new.env(parent=emptyenv())

pkgGlobals$outPath <- ""

setOutPath <- function(path){
  pkgGlobals$outPath <- path
}

