
#' nutilsTestInteractive.R
#'  Test \{nutils\} package interactive functions by hand.
#' @return Nothing. User decides if each test has worked.
#' @export
#'
#' @examples
#' nutilsTestInteractive()

nutilsTestInteractive <- function (){



# Test macedit
tTextFile <- system.file("inst/extdata/", "myText.txt", package = "nutils")
catln(tTextFile)
tmp= checkActionOk("macedit TextEditor myText.txt", func=function() macedit(tTextFile))

if (tmp!=1) stop('macedit interactive test failed')

# Test macopen of a file
tmp= checkActionOk("macopen of text file", func=function() macopen(tTextFile))

if (tmp!=1) stop('macopen of text file: interactive test failed')

# Test open of a folder

tFolder <- system.file("inst/extdata/",package = "nutils")
catln(tFolder)
tmp= checkActionOk("macopen folder", func=function() macopen(tFolder))

if (tmp!=1) stop('macopen  of folder: interactive test failed')

}



checkActionOk=function(msg,func=func){
    func()
    print(func)
    nutils::catln (msg)
    if (interactive()){
        utils::menu(c('Yes','No'))
    }
    else{
        catln(" noninteractive mode automatic return")
        return(1)
    }
}
