context("apputils")

test_that("use_apputils", {
  cl <- list(tag="shiny.tag", taglist=c("shiny.tag.list", "list"))
  expect_is(use_apputils(), cl$tag)
  expect_is(use_apputils(TRUE, TRUE), cl$taglist)
  expect_is(use_apputils(TRUE, FALSE), cl$taglist)
  expect_is(use_apputils(FALSE, TRUE), cl$taglist)
})
