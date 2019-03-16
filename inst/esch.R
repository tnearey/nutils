
pesc<- function (pat, cesc='`'){
    # % Use another character in a regex patterm to force a literal backslash inside the string
    # Except for backslash itself, these look more like perlish escaped regular expression
    # To escape backslash use '``' or other double cesc.
    # gsub(')
    # Change remaining (unpaired) cesc's to backslash (R escape)
    if (is.null(cesc)) {cesc='`'}
    special="([.|)\\^{}+$*?:]"
    if ((nchar(cesc)!=1)||grepl(cesc,special,fixed=TRUE)){
        browser()
        stop(paste0('cesc  is ', cesc, 'but it must be single character not in :', special))
    }
    pat=gsub(cesc,'\\\\',pat)
    return(pat)
}



