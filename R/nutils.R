# nutils.R - some debugging etc utils
# Part of codeGenToolsR package

#--- Utility functions catln(), macfinder
#' catln -- cat with newline at end (analogous to Pascal \code{writeln()})
#'
#' @param ... (1)  -- arguments to pass to cat
#'
#' @return N/A
#' @export
#' @examples
#' x=1; y=2; cc='banana'
#' catln(x,y,cc)
#'
catln = function(...){
  #   print(paste("class", class(x)))
  cat(...,"\n")
}
#-h-- genSampleDataFrameWithFactors----
#' Generate a dummy dataframe with factors
#'
#' @return Returns a test data frame
#'
#' @export
#'
#' @examples
#' tdf <- genSampleDataFrameWithFactors()
#' print(summary(tdf))
#'
genSampleDataFrameWithFactors=function(){
n <- 20
dtof <- data.frame(x = stats::rnorm(n),
                   f = sample(rep(
                     c("lo", "med", "hi"),
                     ceiling(n/3)),
                     n),
                   f2 = sample(rep(
                     c("bt","cntr", "lft",  "rt",   "tp" ),
                     ceiling(n/5),
                     n))
)
}

#--------------macopen --------------
# macopen -- issue open command
#' Open a Mac OS file or folder by it's default application.
#'
#' Open directory in Mac OS finder, by default current working directory. Or open a file by it's default application.
#'

#' @param dirOrFileName  path to folder to open in mac os; default `getwd()`
#'
#' @return  Null If argument is a directory, opens it in finder; else  opens it with default application.
#' @export
#' @examples
#' ## not run
#' txtFile=system.file("extdata","myTextFile.txt",package='nutils')
#' macopen(txtFile)
#'
#' rFile=system.file("extdata","myRFile.R",package='nutils')
#' macopen('rFile)
#'
macopen = function(dirOrFileName=getwd()) {

  # Copyright (c) T M Nearey 2015
  # Version 1.0.0     Sat May  9 14:25:30 2015

      if (Sys.info()["sysname"]!="Darwin"){
    catln("Not Darwin can't open ",dirOrFileName)
  } else{
      tpath=path.expand(dirOrFileName)
      catln('tpath',tpath)
    open.cmd=paste("open ","\"",tpath,"\"",sep="")
    catln('cmd', open.cmd)
    system(open.cmd)}
}
#--------------macedit --------------
#' Open a file in Mac OS text editor
#'
#' @param fname full path name of file to open
#' @return  N/A .. (Opens file in Text Edit app.)
#' @export
#' @examples
#' # not run
#' txtFile=system.file("extdata","myTextFile.txt",package='nutils')
#' macedit(txtFile)

macedit = function(fname){
  if (Sys.info()["sysname"]!="Darwin"){
    catln("Not Darwin can't open ",fname)
  } else{

      tpath=path.expand(fname)
      # catln('tpath',tpath)
    open.cmd=paste("open -e ","\"",tpath,"\"",sep="")
    system(open.cmd)}
}

#---------- showvars ------------
#' Display names, types and values of variables (for quick wordy debugging)
#'
#' @param  ... (1) -- list of variables
#' @return  N/A (lists variables to console)
#' @export
#' @examples
#'  x=1; y=2; cc='banana'
#' showvars(x,y,cc)
#'
showvars <- function (...){
  # Display names of variables and their contents
  #   http://stackoverflow.com/questions/25486448/capture-a-variable-number-of-named-arguments-to-a-function
  dots=list(...)
  #  print(dots)
  t=as.list(sys.call())
  #  http://stackoverflow.com/questions/8557403/r-get-formals-from-call-object
  for (i in 2:length(t)){
    tval=dots[[i-1]]
    cat('"',t[[i]],'": ', class(tval), '\n',sep='')
    print(tval)
  }
}

#' Read a string (max 5000 char in mac) from clipboard
#' @param testText  a string/character array usually missing. It allows test text to be put in
#'     and mostly gets around a roxygen2 issue I couldn't figure.
#' @return   a string
#' @details
#' Uses \code{writeChar()} on mac. See e.g. \url{http://stackoverflow.com/a/13445458/1795127}
#' @export
#' @examples
#' # Select someting and copy it to clipboard
#' # Then input a line
#' readline('Type cr when ready')
#' print(readStringFromClip())
#'
readStringFromClip <- function(testText=NULL){
  # http://stackoverflow.com/a/13445458/1795127 and other sources
    if  (!is.null(testText)) return(paste(testText))
  if (Sys.info()['sysname']=="Darwin"){
    clip <- pipe("pbpaste")
    outStr <- readChar(clip,5000)
    close(clip)
  }else{
    # Not sure for Win / Unix
    outStr <- readClipboard("clipboard",5000)
  }
  return(outStr)
}
#---------- writeStringToClip ------------
#' Write the string in inStr to clipboard
#' @param inStr string to copy to clipboard (uses writeChar on mac)
#' @return  NULL
#' @export
writeStringToClip <- function (inStr){
  # http://stackoverflow.com/a/13445458/1795127
  if (Sys.info()['sysname']=="Darwin"){
    clip <- pipe("pbcopy", "w")
    writeChar(inStr, clip)
    close(clip)
  }else{
    # Not sure for Win / Unix
    utils::writeClipboard(inStr)
  }
}

#---------- isQuote -----------------
#' Determine if character is a quote (', " , `)
#'
#' @param ch (1)  -- character to test
#' @return --- boolean indicating whether ch is a quote char.
#' @export
#'
isQuote=function(ch){
  # is ch a quote char?
  quoteChars=c("'",'"','`')
  return( ch %in% quoteChars)
}


