context("test-nutils")

test_that("multiplication works", {
    expect_equal(2 * 2, 4)
})

test_that("catln", {
    expect_equal(capture.output(cat('HelloThere \n')),
                 capture.output(catln("HelloThere")))

})


# showvars
x = 1
cc = "cc"
banana = data.frame(x = 'fruit')
catln(' x= 1; cc= "cc"; banana = data.frame(x = "fruit")' )
test_that('showvars', {
    jnk = readline('showvars: Type CR if ok else x:')
    expect_equal(jnk, "")
}
)





#  writeStringToClip and readStringFromClip
tmsg='This was written to clip'
writeStringToClip(tmsg)
tt=readStringFromClip()
test_that('readStringFromClip', {
    expect_equal(tmsg, tt)
}
)

# genSampleDataFrameWithFactors
test_that("genSampleDataFrameWithFactors", {
    expect_equal(is.data.frame(genSampleDataFrameWithFactors()),
                 TRUE)
})

# isQuote=function(ch)
testVec=c('"',"'",'`','a','b')
t2Vec=sapply(testVec,isQuote)

refVec=c(TRUE,TRUE,TRUE,FALSE,FALSE)
names(refVec)=names(t2Vec)
test_that("isQuote", {
    expect_equal(t2Vec,
                 refVec)

})


 # parsetext <- function(...,shouldShowText=FALSE) {
#' #

t1=parse(text='print("Hello")');
t2=parsetext('print("Hello")')
catln(' But note  that |parsetext("<string>")| and |parse(text=="<string>"))|  are not idential' )
print(all.equal(t1,t2))
tcompare=identical( setdiff(deparse(t1),deparse(t2)),character(0))
test_that("parsetext",{
    expect_equal( tcompare,
                  TRUE)
})


if (interactive() && rstudioapi::isAvailable()) {

# macopen
    macopen()
    test_that('macopen', {
        jnk = readline('macopen: Type CR if ok else x:')
        expect_equal(jnk, "")
    })

# macedit
    macedit(rstudioapi::getSourceEditorContext()$path)
    test_that('macedit', {
        jnk = readline('macedit: Type CR if ok else x:')
        expect_equal(jnk, "")
    })





}


