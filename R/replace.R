#' @export
#' @rdname edit-file
edit_replace <- function(path, code, selection, style = TRUE) {
  old_code <- readLines(path)
  new_code <- code_replace(readLines(path), code, {{ selection }}, style)
  writeLines(new_code, path)
  invisible(NULL)
}

code_replace <- function(old, new, selection, style = TRUE) {
  location <- code_select(old, {{ selection }})
  if (!length(location)) {
    rlang::abort("Location not found")
  }
  if (!identical(location, min(location):max(location))) {
    rlang::abort("Can't replace code when matched lines are not consecutive")
  }

  if (is.language(new)) {
    new <- deparse(new)
  } else if (rlang::is_formula(new)) {
    new <- rlang::as_function(new)
    new <- new(old[location])
  } else if (is.function(new)) {
    new <- new(old[location])
  }

  old <- old[-location]
  location <- min(location)

  l <- length(old)
  code <- c(
    old[seq_len(location-1)],
    new,
    if (location < l) old[location:l]
  )
  if (style) code <- styler::style_text(code)
  code
}
