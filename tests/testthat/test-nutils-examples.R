context("nutils")

context("catln")
test_that("catln() works", {
  x <- 1
  y <- 2
  cc <- "banana"
  expect_equal(capture.output(catln(x, y, cc))
               , capture.output(cat(x,y,cc,'\n')))
})

context("nutils")
test_that("`genSampleDataFrameWithFactors()` works", {
 expect_equal(names(genSampleDataFrameWithFactors()),
              c("x","f","f2"))

})


context("showvars")
test_that("`showvars()` works", {
    x <- 1
    y <- 2
    cc <- "banana"
    expectedText='"x": numeric;[1] 1;"y": numeric;[1] 2;"cc": character;[1] "banana"'
    myText=paste0(capture.output(showvars(x,y,cc)),collapse=';')
    showvars(myText,expectedText)
    expect_match( myText , expectedText,fixed=TRUE)
})

