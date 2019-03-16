#------------------------------     praw    ------------------------------

#' Pattern (or Pseudo) raw: Approximate Python-like `raw` Strings
#'
#' Uses `backtick` (or another character or string) as an alias for `backslash` in regex pattern strings,
#'
#' @param pat a string or character vector
#' @param cesc  a character or string (default `)  to be substituted by
#'  double \\ in the returned pattern string where any cescs appeared on input.
#' @return the pattern with the `cesc` character replaced by two backslashes. See details.
#'
#' @details Literal occurrences of characters such as ([.|)\\^{}+$*?:] often need to be escaped
#'     in regular expression patterns. This may make them look very different from, e.g.,
#'     Python raw string patterns.
#'     Thus, the equivalent of Python's  \code{pyPat= r'\\(\\s*\\w+\\s*\\)'} would have to look like:
#'      \code{rPat <- "\\\\(\\\\s*\\\\w+\\\\s*\\\\)"}. With \code{praw()} we can instead use
#'       \code{erPat <- ('`(`s*`w+`s*`)')}
#'      To match a literal single backslash in a string,  \code{pat <- praw('``')} is necessary, generating  the same
#'      string as \code{pat= '\\\\'}.
#'      To match a double literal backslash R would require  an eight 'leaning toothpicks' pattern \code{('\\\\\\\\\\\\\\\\')}, but with praw, we get away with
#'      four backticks \code{ pat <- praw('````')}, yielding \\\\\\\\\\\\\\\\.
#'      If a literal backtick is in the pattern of interest, you can substitute another unused character to stand in for \\\\ sequences.
#' @export
#'
#' @examples
#' praw('`(`s*`w+`s*`)') == "\\(\\s*\\w+\\s*\\)"
#'
#' # Replace a\\b with a:b
#'
#'#' gsub(praw("````"), ":", "a\\\\b")
#' # or  using '%' instead of `'`:
#' gsub(praw('%%%%','%'), ":", "a\\\\b")



praw<- function (pat, cesc='`'){

    # gsub(')
    # Change remaining (unpaired) cesc's to backslash (R escape)
    if (is.null(cesc)) {cesc='`'}
    pat=gsub(cesc,'\\\\',pat)
    return(pat)
}



#------------------------------     praw1    ------------------------------

#' Pattern (or Pseudo) raw: Approximate Python-like `raw` Strings (with warnings)
#'
#' @param pat a string or character vector
#' @param cesc  a single character(default `)  to be substituted by
#'  double \\ in the pattern where any cescs appeared on input.
#'     Note this warns if `cesc` is not a single character or if it is the  special set: "([.|)\\^{}+$*?:]"
#' @return the pattern with the `cesc` character replaced by two backslashes. See details.
#'
#' @details Literal occurrences of characters such as ([.|)\\^{}+$*?:] often need to be escaped
#'     in regular expression patterns. This makes them look very different than e.g.  raw string patterns.
#'     Thus, the equivalent of Python's
#'     \code{ pyPat= r'\\(\\s*\\w+\\s*\\)'}
#'     would have to look like:
#'      \code{rPat <- "\\\\(\\\\s*\\\\w+\\\\s*\\\\)"}.
#'      With \code{praw()} we can instead use \code{erPat <- ('`(`s*`w+`s*`)')}
#'      To match a literal single backslash in a string,  \code{pat <- praw('``', '`' )} is necessary, generating pat= '\\\\'.
##' @export
#' @seealso  \code{\link{praw}}
#' @examples
#' praw1('`(`s*`w+`s*`)') == "\\(\\s*\\w+\\s*\\)"
#'
#' # Replace a\\b with a:b
#'
#' gsub(praw1('%%%%','%'), ":", "a\\\\b")
#' # or
#' gsub(praw("\\`\\```"), ":", "a\\\\b")


#'
praw1<- function (pat, cesc='`'){
    # % Use another single character in a regex patterm to force a literal backslash inside the string
    # Except for backslash itself, these look more like perlish escaped regular expression
    # To escape backslash use '``' or other double `cesc`.
    # gsub(')
    # Change remaining (unpaired) `cesc`'s to backslash (R escape)
    if (is.null(cesc)) {cesc='`'}
    special="([.|)\\^{}+$*?:]"
    if ((nchar(cesc)!=1)||grepl(cesc,special,fixed=TRUE)){
        browser()
        stop(paste0('cesc  is ', cesc, '. but praw1 expects cesc to be single characte not in :', special))
    }
    pat=gsub(cesc,'\\\\',pat)
    return(pat)
}




