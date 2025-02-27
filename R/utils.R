#' Convert a character vector to integer, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#'
#' @param x A character vector
#'
#' @return An integer vector
#' @seealso [as_numeric_shh()] [as_logical_shh()]
#'
#' @noRd
#'
#' @examples
#' as_integer_shh(c("1", "2", "3"))
as_integer_shh <- function(x) {
  if (!is.character(x)) {
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.integer(x))
}

#' Convert a character vector containing `Ỳ`, `N` and `NA` to a logical vector.
#'
#' @param x A character vector only containing `Y`, `N` and `NA`. Any other
#'   values will be silenty converted to `NA`.
#'
#' @return A logical vector
#' @seealso [as_numeric_shh()] [as_integer_shh()]
#'
#' @noRd
#'
#' @examples
#' yes_no_as_logical(c("Y", "N", NA, NA, "Y"))
#' yes_no_as_logical(c("Y", "foo", "bar", "N", NA))
yes_no_as_logical <- function(x) {
  # x needs to be a character vector
  if (!is.character(x)) {
    cli::cli_abort("x must be a character vector")
  }

  # Convert `Y` to TRUE, `N` to FALSE and `NA` to NA
  converted_vector <-
    dplyr::case_when(
      x == "Y" ~ TRUE,
      x == "N" ~ FALSE,
      .default = NA,
      .ptype = logical()
    )

  return(converted_vector)
}

#' Convert a character vector to numeric, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#' @param x A character vector
#'
#' @return A numeric vector
#' @seealso [as_integer_shh()] [as_logical_shh()]
#'
#' @noRd
#'
#' @examples
#' as_double_shh(c("1.1", "2.2", "3.3"))
as_numeric_shh <- function(x) {
  if (!is.character(x)) {
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.numeric(x))
}





#' Function to set the user agent to a getRad specific one in an httr2 request
#'
#' @param req an `httr2` request
#'
#' @returns an `httr2` request
#' @noRd
req_user_agent_getrad <- function(req) {
  httr2::req_user_agent(req, string = getOption("getRad.user_agent"))
}


# Create an .onload function to set package options during load
# getRad.key_prefix is the default prefix used when setting or getting secrets using keyring
# getRad.user_agent is the string used as a user agent for the http calls generated in this package
#   It incorporates the package version using `getNamespaceVersion`
.onLoad <- function(libname, pkgname) { # nolint

  op <- options()
  op.getRad <- list(
    getRad.key_prefix = "getRad_",
    getRad.user_agent = paste("R package getRad", getNamespaceVersion("getRad"))
  )
  toset <- !(names(op.getRad) %in% names(op))
  if (any(toset)) options(op.getRad[toset])
  rlang::run_on_load()
  invisible()
}
rlang::on_load(rlang::local_use_cli(inline = TRUE))
