#' Remove code from file
#'
#' @param path String. Path to file to edit
#' @param code Character vector. Code to insert
#' @param before,after <tidy-select> using {editor} helpers. destination of `code`, use either, not both and not not none.
#' @param style Boolean, wether to style the output. Useful because it's hard to monitor
#'   indention, so set to `TRUE` by default
#'
#' @export
edit_remove <- function(path, selection, style = TRUE) {
  old_code <- readLines(path)
  new_code <- code_remove(readLines(path), {{ selection }}, style)
  writeLines(new_code, path)
}

code_remove <- function(old, selection, style = TRUE) {
  location <- code_select(old, {{ selection }})
  if (!length(location)) {
    rlang::abort("Location not found")
  }
  code <- old[-location]
  if (style) code <- styler::style_text(code)
  code
}
