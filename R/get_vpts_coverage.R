#' Get coverage for vpts from various sources
#'
#' @inheritParams get_vpts source
#'
#' @returns A `data.frame` or `tibble` with at least three columns, `source`, `radar` and `date` to indicate the combination for which data exists
#'
#' #' @details
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
#' @examples
#' get_vpts_coverage()
get_vpts_coverage<-function(source=c("baltrad", "uva", "ecog-04003", "rmi",...)){
  source<-rlang::arg_match(source, multiple =TRUE)
  if(length(source)>1L){
    return(dplyr::bind_rows(lapply(source, get_vpts_coverage,...)))
  }
  if(length(source)==0){
    cli::cli_abort("Source should atleast have one value.", class="getRad_error_length_zero")
  }
  dplyr::case_match(source,
             "rmi" ~ list(get_vpts_coverage_rmi(...)),
             c("baltrad", "uva", "ecog-04003") ~
               list(get_vpts_coverage_aloft(...) |>
                      dplyr::filter(source==!!source))
             )[[1]] |>
    dplyr::relocate(source, radar, date)

}
