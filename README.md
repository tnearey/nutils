
<!-- README.md is generated from README.Rmd. Please edit that file -->
nutils
======

<!-- badges: start -->
<!-- badges: end -->
The package *nutils* contains two kinds of utilities.

1.  Those that I found useful particularly in early debugging 'scaffolding'

    -   `catln` -- just cat with a `newline` appended, analogous to python3 `print()`. Name inspired by venerable Pascal `writeln`.

    -   `showvars` -- show variable names and print its type and contents.

2.  A couple of functions that interface with the mac
    -   macopen - open a Mac folder or file in Finder or in the file's default application.

    -   macedit - open a text file in Mac TextEdit app.

Installation
------------

You can install the latest commit from [GITHUB](https://github.com/) with: `devtools:install_github("tnearey/nutils")`

Example
-------

``` r
library(nutils)
catln('a',"the banana is in the cupboard")
#> a the banana is in the cupboard
x=1
y="Why not"
lst=list('x'=x,'y'=y,fred=data.frame(x=x,y=y,z="Not me"))
showvars(x,y,lst)
#> "x": numeric
#> [1] 1
#> "y": character
#> [1] "Why not"
#> "lst": list
#> $x
#> [1] 1
#> 
#> $y
#> [1] "Why not"
#> 
#> $fred
#>   x       y      z
#> 1 1 Why not Not me
```
