#' Get coverage for vpts from various sources
#'
#' @inheritParams get_vpts
#' @param ... Arguments passed on to internal functions.
#'
#' @returns A `data.frame` or `tibble` with at least three columns, `source`, `radar` and `date` to indicate the combination for which data exists
#'
#' @details
#' ```{r get url to fetch coverage from, echo = FALSE, results = FALSE}
#' cov_url <- paste(
#'   getOption("getRad.aloft_data_url"), "coverage.csv", sep = "/"
#' )
#' ```
#'
#' The coverage file for aloft is fetched from <`r cov_url`>. This can be changed by
#' setting `options(getRad.aloft_data_url)` to any desired url.
#'
#' @export
#'
#' @examplesIf interactive()
#' get_vpts_coverage()
get_vpts_coverage<-function(source=c("baltrad", "uva", "ecog-04003", "rmi"),...){
  source<-rlang::arg_match(source, multiple =TRUE)
  if(length(source)>1L){
    return(dplyr::bind_rows(lapply(source, get_vpts_coverage,...)))
  }
  if(length(source)==0){
    cli::cli_abort("Source should atleast have one value.", class="getRad_error_length_zero")
  }
  # Note for future this function can possibly be made faster by grouping all calls to aloft
  switch(source,
             "rmi"=get_vpts_coverage_rmi(...),
               get_vpts_coverage_aloft(...) |>
                      dplyr::filter(source==!!source)
             ) |>
    dplyr::relocate(source, radar, date)

}
