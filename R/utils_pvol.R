#' Read and merge pvol files
#'
#' Several countries have pvol files per parameter this helper function reads them.
#' It combines them all into one pvol, and checks the attributes are equal
#'
#' @param urls A character vector with urls to h5 files to read
#' @param ... arguments to bioRad::read_pvolfile
#' @param call
#'
#' @returns a pvol
#' @noRd
#'
read_pvol_from_url_per_param <- function(
  urls,
  ...,
  call = rlang::caller_env()
) {
  withr::with_tempdir({
    polar_volumes_tibble <- data.frame(url = urls) |>
      dplyr::mutate(
        req = purrr::map(url, function(x) {
          httr2::request(x) |>
            httr2::req_options(ssl_verifypeer = 0) |> # SK seems to have verification error
            req_user_agent_getrad()
        }),
        # use .data to prevent note about no visible binding for global variable
        resp = httr2::req_perform_parallel(
          .data$req,
          paths = replicate(
            length(.data$req),
            tempfile(fileext = ".h5", tmpdir = getwd())
          )
        ),
        tempfile = purrr::map_chr(.data$resp, "body"),
        pvol = purrr::map(tempfile, bioRad::read_pvolfile, ...),
        remove = purrr::map(tempfile, file.remove)
      )
  })
  # Check if all parameter have same attributes
  list_of_attribute_tables <- purrr::map(
    purrr::chuck(polar_volumes_tibble, "pvol"),
    bioRad::attribute_table
  )
  all_params_same_attributes <- all(unlist(lapply(
    lapply(list_of_attribute_tables[-1], dplyr::select, -"param"),
    all.equal,
    dplyr::select(list_of_attribute_tables[[1]], -"param")
  )))
  if (!all_params_same_attributes) {
    cli::cli_abort(
      "Not all polar volumes have the same attributes",
      class = "getRad_error_differing_attributes",
      call = call
    )
  }
  pvol <- Reduce(
    function(x, y) {
      x$scans <- mapply(
        function(i, j) {
          i$params <- c(i$params, j$params)
          i
        },
        x$scans,
        y$scans,
        SIMPLIFY = FALSE
      )
      x
    },
    polar_volumes_tibble$pvol
  )
  pvol
}
