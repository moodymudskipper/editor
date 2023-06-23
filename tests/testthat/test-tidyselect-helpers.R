test_that("editor works", {

  expect_snapshot({
    code <- '
# some
# comment
with_equal = c(
  1,
  2
)

with_arrow <- c(
  3,
  4
)

this <- {
  "this"
}

fun1 <- function() {
  hello <- 1
  world <- 2
  fun2 <- function() {
    hi <- 2
    again <- 3
  }
}

a(1)

ab(2)

abc(3)
'

  file <- tempfile()
  writeLines(code, file)

  edit_remove(file, 2:3) # remove leading comment
  edit_remove(file, expr_defines("with_equal"))
  edit_remove(file, expr_defines("with_arrow"))
  edit_replace(file, toupper, expr_calls(stringr::regex("a"), 3))
  edit_replace(file, toupper, expr_calls("a"))
  edit_remove(file, in_function("fun1", expr_defines("world")))
  edit_remove(file, in_function("fun1", in_function("fun2", expr_defines("again"))))
  edit_insert(file, 'print("hello")', after = expr_defines("this"))
  edit_insert(file, ~paste("#", .x), before = expr_defines("this"))
  edit_replace(file, toupper, line_matches('"this"'))

  styler::style_text(readLines(file))

  })


})

