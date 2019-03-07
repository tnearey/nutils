# nutils.R - some debugging etc utils
# Part of codeGenToolsR package

#--- Utility functions catln(), macfinder
#' catln -- cat with newline at end
#'
#' @param ... (1)  -- args to pass to cat
#'
#' @return N/A
#' @export
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
#
#' Open directory in Mac OS finder, by default current working directory
#'
#' @param dirOrFileName (1) = getwd()   path to folder to open in mac os
#'
#' @return  NONE : If argument is a directory
#' @export

macopen = function(dirOrFileName=getwd()) {
  #' macopen = function(dirOrFileName=getwd())
  #' Open directory in Mac OS finder, by default current working directory.
  #' Or open a file by it's default application.
  #' Args:
  #    1) dirOrFileName=getwd() --
  #' Returns:
  #  None
  # Copyright (c) T M Nearey 2015
  # Version 1.0.0     Sat May  9 14:25:30 2015
  # ------- End internal doc ----------------
  # openWorkingDirectory ----s
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
#' @param fname (1) -- fullpath name of file to open
#' @return  N/A .. opens file in Text Edit app
#' @export
#--------- End roxygen ------------------
macedit = function(fname)
{# openWorkingDirectory ----s
  if (Sys.info()["sysname"]!="Darwin"){
    catln("Not Darwin can't open ",fname)
  } else{

      tpath=path.expand(fname)
      # catln('tpath',tpath)

    open.cmd=paste("open -e ","\"",tpath,"\"",sep="")
    system(open.cmd)}
}

#---------- showvars ------------
#--------- Start roxygen ------------------
#' Displays names and values of atomic variables (for texty debugging)
#'
#' @param  ... (1) -- list of variables
#' @return  N/A (lists variables to console)
#' @export
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
#' ReadStringFromClip
#'
#' Read a string (max 5000 char in mac) from clipboard
#' @param  None
#' @return -- a string
#' @details
#' Uses writeChar on mac. See e.g. http://stackoverflow.com/a/13445458/1795127
#' @export
readStringFromClip<- function(){
  # http://stackoverflow.com/a/13445458/1795127 and other sources
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
#' @param inStr (1)  -- string to copy to clipboard (uses writeChar on mac)
#' @return --- NA
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

#   replace with require pyrex eventually
#http://stackoverflow.com/questions/14547069/how-to-write-from-r-to-the-clipboard-on-a-mac
#--------- Start roxygen ------------------
#' Determine if character is a quote ('"`)
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
# parsetext(...)
#'
#' Abrreviates parse(text= paste0(.....)) useful for eval[.parent](parsetext(...)) ----
#' parsetext parse a character vector
#'
#' @param ... -- text args to parse after pasting
#' @param shouldShowText -- boolean showText or not
#'
#' @return An object of type "expression" see ?parse
#' @export
#'
#' @examples
#'  parsetext('print("This is a test")')
#'
parsetext <- function(...,shouldShowText=FALSE) {
    warning('Hard to use.. need to take care of environments better just use parse(text=...)')
  if (shouldShowText){
    txt=paste0(...)
    # catln('Text of commmand:',txt)
  }
  parse(text=paste0(...))
}

