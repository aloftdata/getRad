#' @importFrom utils citation
add_reference_vpts <- function(x, source, ..., call) {
  if (
    !rlang::is_scalar_character(source) || !(source %in% names(vptsReferences))
  ) {
    cli::cli_abort(
      "{.arg source} should be a scalar character of a known source.",
      call = call,
      class = "getRad_error_add_reference_vpts_invalid_source"
    )
  }
  if (bioRad::is.vpts(x)) {
    x$attributes$references <- c(
      vptsReferences[source],
      getRad = citation("getRad")
    )
    return(x)
  }
  if (is.data.frame(x)) {
    attr(x, "references") <- c(
      vptsReferences[source],
      getRad = citation("getRad")
    )
    return(x)
  }
  if (is.list(x)) {
    return(purrr::map(x, add_reference_vpts, source = source))
  }

  cli::cli_abort(
    "References can only be added to {.cls data.frame}/{.cls tibble} or {.cls vpts} objects.",
    call = call,
    class = "getRad_error_add_reference_vpts_only_df_and_vpts"
  )
}
