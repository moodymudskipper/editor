code_select <- function(code, where) {
  # tidy selection needs names but we won't use any of those so we just set numbers as names
  names(code) <- seq_along(code)
  where_quo <- enquo(where)
  # we want to enrich this env so we can use editor and stringr helpers without namespace
  rlang::local_bindings(
    line_matches = line_matches,
    expr_calls = expr_calls,
    expr_defines = expr_defines,
    in_function = in_function,
    boundary = stringr::boundary,
    coll = stringr::coll,
    fixed = stringr::fixed,
    regex = stringr::regex,
    .env = rlang::quo_get_env(where_quo)
  )
  out <- tidyselect::eval_select(
    where_quo,
    code
  )
  unname(out)
}
