
#' Create an .onload function to set package options during load
#'
#' - getRad.key_prefix is the default prefix used when setting or getting
#' secrets using keyring.
#' - getRad.user_agent is the string used as a user agent for the http calls
#' generated in this package. It incorporates the package version using
#' `getNamespaceVersion`.
#' - getRad.max_cache_age_seconds is the default max cache age for the httr2
#' cache in seconds.
#' - getRad.max_cache_size_bytes is the default max cache size for the httr2
#' cache in bytes.
#' @noRd
.onLoad <- function(libname, pkgname) { # nolint
  op <- options()
  op.getRad <- list(
    getRad.key_prefix = "getRad_",
    getRad.user_agent = paste("R package getRad", getNamespaceVersion("getRad")),
    getRad.aloft_data_url = "https://aloftdata.s3-eu-west-1.amazonaws.com",
    getRad.nexrad_data_url = "https://noaa-nexrad-level2.s3.amazonaws.com",
    getRad.cache = cachem::cache_mem(max_size = 128 * 1024^2, max_age = 60^2 * 24)
  )
  toset <- !(names(op.getRad) %in% names(op))
  if (any(toset)) options(op.getRad[toset])
  rlang::run_on_load()
  invisible()
}
rlang::on_load(rlang::local_use_cli(inline = TRUE))
