test_that("Pvol for the Netherlands can be downloaded", {
  skip_if(Sys.which("KNMI_vol_h5_to_ODIM_h5") == "")
  skip_if_offline(host = "api.dataplatform.knmi.nl")

  # make sure local env is used by keyring so that api key can be set
  withr::local_options(list(
    "keyring_backend" = "env"
  ))
  # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
  withr::local_envvar(
    list(
      "getRad_nl_api_key" = "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6ImVlNDFjMWI0MjlkODQ2MThiNWI4ZDViZDAyMTM2YTM3IiwiaCI6Im11cm11cjEyOCJ9"
    )
  )
  time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki")
  expect_s3_class(
    get_pvol("nlhrw", time, param = "all"),
    "pvol"
  )
  pvol <- expect_s3_class(get_pvol("nlhrw", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    lubridate::floor_date(pvol$datetime, "1 mins"),
    lubridate::with_tz(time, "UTC")
  )
})


test_that("Pvol for the Netherlands can be downloaded. Incorrect converter results in failure.", {
  skip_if_offline(host = "api.dataplatform.knmi.nl")

  # make sure local env is used by keyring so that api key can be set
  withr::local_options(list(
    "keyring_backend" = "env",
    "getRad.nl_converter" = "ls"
  ))
  # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
  withr::local_envvar(
    list(
      "getRad_nl_api_key" = "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6ImVlNDFjMWI0MjlkODQ2MThiNWI4ZDViZDAyMTM2YTM3IiwiaCI6Im11cm11cjEyOCJ9"
    )
  )
  time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki")
  expect_error(
    get_pvol("nlhrw", time, param = "all"),
    class = "getRad_error_dutch_converter_failed"
  )
})

test_that("failure to find converter", {
  withr::with_envvar(c("PATH" = ""), {
    # make sure local env is used by keyring so that api key can be set
    withr::local_options(list(
      "keyring_backend" = "env"
    ))
    # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
    withr::local_envvar(
      list(
        "getRad_nl_api_key" = "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6ImVlNDFjMWI0MjlkODQ2MThiNWI4ZDViZDAyMTM2YTM3IiwiaCI6Im11cm11cjEyOCJ9"
      )
    )
    expect_error(
      get_pvol(
        "nlhrw",
        time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki"),
        param = "all"
      ),
      class = "getRad_error_no_nl_converter_found"
    )
  })
})

test_that("The Netherlands non existing radar", {
  withr::local_options(list(
    # use random binary so checking for converter step is passed
    "getRad.nl_converter" = "ls"
  ))
  expect_error(
    pvol <- get_pvol(
      "nlaaa",
      time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki")
    ),
    class = "getRad_error_radar_not_found"
  )
})
test_that("Pvol for the Netherlands authenication failure", {
  skip_if_offline(host = "api.dataplatform.knmi.nl")
  # make sure local env is used by keyring so that api key can be set
  withr::local_options(list(
    "keyring_backend" = "env",
    # use random binary so checking for converter step is passed
    "getRad.nl_converter" = "ls"
  ))
  # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
  withr::local_envvar(list("getRad_nl_api_key" = "wrongkey"))
  expect_error(
    pvol <- get_pvol(
      "nlhrw",
      time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki"),
      param = "all"
    ),
    class = "getRad_error_get_pvol_nl_authorization_failure"
  )
})
