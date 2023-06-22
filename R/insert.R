#' Insert code in file
#'
#' @param path String. Path to file to edit
#' @param code Character vector. Code to insert
#' @param before,after <tidy-select> using {editor} helpers. destination of `code`, use either, not both and not not none.
#' @param style Boolean, wether to style the output. Useful because it's hard to monitor
#'   indention, so set to `TRUE` by default
#'
#' @export
edit_insert <- function(path, code, before = NULL, after = NULL, style = TRUE) {
  old_code <- readLines(path)
  new_code <- code_insert(readLines(path), code, {{ before }}, {{ after }}, style)
  writeLines(new_code, path)
}

code_insert <- function(old, new, before = NULL, after = NULL, style = TRUE) {
  if (missing(before) && missing(after)) {
    rlang::abort("Provide either `before`, or `after`. None were provided")
  } else if (!missing(before) && !missing(after)) {
    rlang::abort("Provide either `before`, or `after`. Bioth were provided")
  }

  if (!missing(before)) {
    location <- code_select(old, {{ before }})
    location <- min(location) - 1
  } else {
    location <- code_select(old, {{ after }})
    location <- max(location)
  }

  if (!length(location)) {
    rlang::abort("Location not found")
  }

  l <- length(old)
  code <- c(
    old[seq_len(location)],
    new,
    if (location < l) old[(location + 1):l]
  )
  if (style) code <- styler::style_text(code)
  code
}


