#' @export
#' @rdname edit-file
edit_remove <- function(path, selection, style = TRUE) {
  old_code <- readLines(path)
  new_code <- code_remove(readLines(path), {{ selection }}, style)
  writeLines(new_code, path)
  invisible(NULL)
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
