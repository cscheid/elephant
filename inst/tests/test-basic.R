library(stringr)

context("sanity checks.")

elephant.path <- str_c(tempdir(), "/elephant1")

test_that("elephants can be created", {
  e <- new.elephant(elephant.path)
  expect_that(e, is_a("elephant"))
})

test_that("elephants can be told and asked things", {
  e <- new.elephant(elephant.path)
  tell.elephant(e, "foo", 3)
  expect_that(ask.elephant(e, "foo"), equals(3))
  tell.elephant(e, "foo", 5)
  expect_that(ask.elephant(e, "foo"), equals(5))
  tell.elephant(e, "foo", NULL)
  expect_that(ask.elephant(e, "foo"), throws_error())
})
