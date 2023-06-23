#' {editor} selection helpers
#'
#' @description
#' These are to be used in `selection`, `before` or `after` arguments of the
#' [editing functions][edit-file], or in `in_function()` described below.
#' They return the indexes of the matching lines in the file.
#'
#' * `line_matches()` matches lines of code and return stheir indices
#' * `expr_defines()` matches a variables being defined and returns the indices of the
#'   lines spanned by the definitions
#' * `expr_calls()` matches a function being called at the top level and returns the indices of the
#'   lines spanned by the call
#' * `in_function()` is used to restrict the scope of a selection to a function definition,
#'   it has a `selection` argument thst might receive a line numbers or calls to the above
#'   functions. its `fun` and `n` arguments are themselves passed internally to `expr_defines()`
#'   to locate the function definition.
#'
#' When used with `edit_*()`. functions (the only place where these should be used),
#' these functions and the stringr helpers `boundary()`, `coll()`, `fixed()` and `regex()`
#' can be use without attaching the package or using the `pkg::fun()` notation.
#'
#' @param code,var,fun Target to match exactly, or pattern such as returned by `stringr::regex()`
#'   or other [stringr modifiers][stringr::modifiers].
#' @param n index of the match to consider. By default all, to choose the last use `Inf`.
#' @return An integer vector
#' @export
#' @name editor_tidy_select
line_matches <- function(code, n = NULL) {
  # FIXME assert input
  old_code <- tidyselect::peek_data()
  loc <- if (inherits(code, "stringr_pattern")) {
    which(stringr::str_detect(old_code, code))
  } else {
    which(trimws(code) == trimws(old_code))
  }
  if (!is.null(n)) {
    if (n == Inf) n <- length(loc)
    loc <- loc[n]
  }
  loc
}

#' @rdname editor_tidy_select
#' @export
expr_defines <- function(var, n = NULL) {
  # FIXME assert input
  old_code <- tidyselect::peek_data()
  parsed_data <- getParseData(parse(text = old_code, keep.source = TRUE), includeText = TRUE)

  top_level_ids <- parsed_data$id[parsed_data$parent == 0]
  top_level_assignments <- subset(
    parsed_data,
    parent %in% top_level_ids & grepl("ASSIGN", token)
  )
  # FIXME: support left to right assignment
  if (inherits(var, "stringr_pattern")) {
    relevant_assignments_lgl <-
      stringr::str_detect(
        subset(parsed_data, id %in% (top_level_assignments$id-1))$text,
        var
      )
  } else {
    relevant_assignments_lgl <-
      subset(parsed_data, id %in% (top_level_assignments$id-1))$text %in% var
  }

  top_level_assignments <- top_level_assignments[relevant_assignments_lgl,]

  loc_df <- subset(parsed_data, id %in% top_level_assignments$parent, select = c("line1", "line2"))
  loc_list <- Map(seq, loc_df$line1, loc_df$line2)
  if (!is.null(n)) {
    if (n == Inf) n <- length(loc_list)
    loc_list <- loc_list[n]
  }
  loc <- unlist(loc_list)
  loc
}

#' @rdname editor_tidy_select
#' @export
expr_calls <- function(fun, n = NULL) {
  # FIXME assert input
  old_code <- tidyselect::peek_data()
  parsed_data <- getParseData(parse(text = old_code, keep.source = TRUE), includeText = TRUE)

  top_level_ids <- parsed_data$id[parsed_data$parent == 0]
  top_level_calls <- subset(parsed_data, parent ==  0 & token == "expr")
  funs <- lapply(top_level_calls$id, function(x) head(subset(parsed_data, parent ==  x), 1))
  match_lgl <- sapply(funs, function(x) subset(parsed_data, parent ==  x$id)$token == "SYMBOL_FUNCTION_CALL")
  fun_nms <- sapply(funs, `[[`, "text")
  if (inherits(fun, "stringr_pattern")) {
    match_lgl <- match_lgl & stringr::str_detect(fun_nms, fun)
  } else {
    match_lgl <- match_lgl & fun_nms %in% fun
  }


  loc_df <- subset(parsed_data, id %in% top_level_calls$id[match_lgl])
  loc_list <- Map(seq, loc_df$line1, loc_df$line2)
  if (!is.null(n)) {
    if (n == Inf) n <- length(loc_list)
    loc_list <- loc_list[n]
  }
  loc <- unlist(loc_list)
  loc
}

#' @export
#' @name editor_tidy_select
in_function <- function(fun, selection, n = NULL) {
  # FIXME assert input
  old_code <- tidyselect::peek_data()
  function_loc <- code_select(old_code, expr_defines(fun, n))
  function_code <- old_code[function_loc]
  parsed_data <- getParseData(parse(text = function_code, keep.source = TRUE), includeText = TRUE)
  expr_id <- parsed_data$id[parsed_data$parent == 0]
  fun_call_id <- tail(parsed_data$id[parsed_data$parent == expr_id], 1)
  body_id <- tail(parsed_data$id[parsed_data$parent == fun_call_id], 1)
  parsed_data <- subset(parsed_data, parent == body_id)
  if (parsed_data$text[[1]] != "{") {
    abort("`in_function()` can't be used when the body of the function doesn't use curly braces")
  }
  parsed_data <- parsed_data[-c(1, nrow(parsed_data)),]
  body_loc <- seq.int(min(parsed_data$line1), max(parsed_data$line2))
  body_code <- function_code[body_loc]
  loc <- code_select(body_code, {{ selection }})
  loc + min(function_loc) + min(body_loc) - 2
}


