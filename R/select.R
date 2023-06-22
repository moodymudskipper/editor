code_select <- function(code, where) {
  # tidy selection needs names but we won't use any of those so we just set numbers as names
  names(code) <- seq_along(code)
  out <- tidyselect::eval_select(
    rlang::enquo(where),
    code
  )
  unname(out)
}
