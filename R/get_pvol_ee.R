get_pvol_ee <- function(radar, time, ..., call = rlang::caller_env()) {
  json_list <- # This list object creates the correct json request
    list(
      filter = list(
        and = list(
          children = list(
            list(
              and = list(
                children = list(
                  list(
                    isEqual = list(
                      field = "$contentType",
                      value = "0102FB01"
                    )
                  ),
                  list(
                    isEqual = list(
                      field = "Phenomenon",
                      value = "VOL"
                    )
                  )
                )
              )
            ),
            list(
              and = list(
                children = list(
                  list(
                    and = list(
                      children = list(
                        list(
                          greaterThanOrEqual = list(
                            field = "Timestamp",
                            value = strftime(time, "%Y-%m-%dT%H:%M:%OS6%z")
                          )
                        ),
                        list(
                          lessThanOrEqual = list(
                            field = "Timestamp",
                            value = strftime(time, "%Y-%m-%dT%H:%M:%OS6%z")
                          )
                        )
                      )
                    )
                  ),
                  list(
                    isEqual = list(
                      field = "Radar",
                      value = radar_recode(
                        radar,
                        "eesur" = "S\u00FCrgavere radar (SUR)",
                        "eehar" = "Harku radar (HAR)",
                        call = call
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  files <- httr2::request(
    "https://avaandmed.keskkonnaportaal.ee/_vti_bin/RmApi.svc/active/items/query"
  ) |>
    req_user_agent_getrad() |>
    httr2::req_body_json(json_list) |>
    req_retry_getrad() |>
    httr2::req_perform(error_call = call) |>
    httr2::resp_body_json()
  if (files$numFound == 0 || length(files$documents) != 1) {
    cli::cli_abort(
      "The expected number of files is not found.",
      class = "getRad_error_get_pvol_ee_differing_n_files"
    )
  }
  pvol <- withr::with_tempfile("file", fileext = ".h5", {
    req <- httr2::request(
      "https://avaandmed.keskkonnaportaal.ee/_vti_bin/RmApi.svc/active/items/"
    ) |>
      req_user_agent_getrad() |>
      httr2::req_url_path_append(files$documents[[1]]$id) |>
      httr2::req_url_path_append("files/0") |>
      req_retry_getrad() |>
      httr2::req_perform(path = file, error_call = call)
    bioRad::read_pvolfile(req$body, ...)
  })
  return(pvol)
}
