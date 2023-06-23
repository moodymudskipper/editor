#' Edit file
#'
#' @param path String. Path to file to edit
#' @param code Character vector. Code to insert or use as a replacement. If a language
#'   object (type "language" or "symbol") it will be deparsed, if a function it will be applied on the matched
#'   code, if a formula it will be converted to a function with `rlang::as_function()` and
#'   applied on the matched code.
#' @param before,after <[`tidy-select`][tidyselect::language]> using [editor helpers][editor_tidy_select]. destination of `code`, use either, not both and not not none.
#' @param selection <[`tidy-select`][tidyselect::language]> using [editor helpers][editor_tidy_select]. Location of code to replace or remove.
#' @param style Boolean, wether to style the output. Useful because it's hard to monitor
#'   indention, so set to `TRUE` by default. It also conveniently fails when the produced
#'   code is not syntactic, before editing the file.
#' @export
#' @return Returns `NULL` invisibly, called for side effects.
#' @name edit-file
edit_insert <- function(path, code, before = NULL, after = NULL, style = TRUE) {
  old_code <- readLines(path)
  new_code <- code_insert(readLines(path), code, {{ before }}, {{ after }}, style)
  writeLines(new_code, path)
  invisible(NULL)
}

code_insert <- function(old, new, before = NULL, after = NULL, style = TRUE) {
  if (missing(before) && missing(after)) {
    rlang::abort("Provide either `before`, or `after`. None were provided")
  } else if (!missing(before) && !missing(after)) {
    rlang::abort("Provide either `before`, or `after`. Both were provided")
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


