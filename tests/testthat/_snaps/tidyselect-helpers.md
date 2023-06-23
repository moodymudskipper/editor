# editor works

    Code
      code <-
        "\n# some\n# comment\nwith_equal = c(\n  1,\n  2\n)\n\nwith_arrow <- c(\n  3,\n  4\n)\n\nthis <- {\n  \"this\"\n}\n\nfun1 <- function() {\n  hello <- 1\n  world <- 2\n  fun2 <- function() {\n    hi <- 2\n    again <- 3\n  }\n}\n\na(1)\n\nab(2)\n\nabc(3)\n"
      file <- tempfile()
      writeLines(code, file)
      edit_remove(file, 2:3)
      edit_remove(file, expr_defines("with_equal"))
      edit_remove(file, expr_defines("with_arrow"))
      edit_replace(file, toupper, expr_calls(regex("a"), 3))
      edit_replace(file, toupper, expr_calls("a"))
      edit_remove(file, in_function("fun1", expr_defines("world")))
      edit_remove(file, in_function("fun1", in_function("fun2", expr_defines("again"))))
      edit_insert(file, "print(\"hello\")", after = expr_defines("this"))
      edit_insert(file, ~ paste("#", .x), before = expr_defines("this"))
      edit_replace(file, toupper, line_matches("\"this\""))
      styler::style_text(readLines(file))
    Output
      # this <- {
      #   "this"
      # }
      this <- {
        "THIS"
      }
      print("hello")
      
      fun1 <- function() {
        hello <- 1
        fun2 <- function() {
          hi <- 2
        }
      }
      
      A(1)
      
      ab(2)
      
      ABC(3)

