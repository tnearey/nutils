context("Praw")

test_that("`praw()` works", {
  expect_equal(praw("`(`s*`w+`s*`)"), "\\(\\s*\\w+\\s*\\)")
  expect_equal(gsub(praw("%%%%", "%"), ":", "a\\\\b"),"a:b" )
  expect_equal(gsub(praw("````"), ":", "a\\\\b"), "a:b")
})

context("Praw1")
test_that("`praw1()` works", {
  expect_match(praw1("`(`s*`w+`s*`)") ,"\\(\\s*\\w+\\s*\\)" ,fixed=TRUE)
  expect_match(gsub(praw1("%%%%", "%"), ":", "a\\\\b"),"a:b",fixed=TRUE )
  expect_match(gsub(praw1("````"), ":", "a\\\\b"), "a:b",fixed=TRUE)
})
