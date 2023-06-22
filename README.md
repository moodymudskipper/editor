
<!-- README.md is generated from README.Rmd. Please edit that file -->

# editor

Experimental!

{editor} can be used to edit scripts programmatically.

We propose the functions

- `edit_insert(path, code, before = NULL, after = NULL, style = TRUE)`
- `edit_replace(path, code, selection, style = TRUE)`
- `edit_remove(path, selection, style = TRUE)`

The position where to insert/replace/remove code is fed to the
`selection`, `before` or `after` args.

We can provide line numbers there but also by tailor made tidy selection
helpers:

- `line_matches(code, n = NULL)` to match a line’s code
- `expr_defines(var, n = NULL)` to match a variable definition
- `expr_calls(fun, n = NULL)` to match a function call

By default the matches are exact (ignoring leading and trailing white
spaces on both sides), but we can use {stringr}’s modifiers like
`stringr::regex()` for more flexibility.

To match a definition or call inside a function definition we can use :

- in_function(fun, selection, n = NULL)

So for example we’ll do

    edit_insert("my_script.R", 'print("hello")', after = in_function("fun1", expr_defines("x")))`

And we’ll have a `print("hello")` statement inserted in the `fun1()`’s
definition right after `x` is defined.

## Installation

Install with:

    remotes::install_github("moodymudskipper/editor")
