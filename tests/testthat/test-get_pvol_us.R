time <- lubridate::ymd_hms("2024-05-12 04:10:00", tz = "UTC")

test_that("get_pvol_us downloads and reads a pvol", {
    skip_if_offline()
    pvol <- getRad::get_pvol("KABR", time)
    expect_true(inherits(pvol, "pvol"))
})

nexrad_stations <- read_fwf(
  "~/Downloads/nexrad-stations.txt",          # keep raw txt in data-raw/
  fwf_cols(icao = c(10, 13)),              # start = 10, end = 13  (1-indexed)
  skip = 2,                                # drop the header + dashes
  col_types = "c",                         # all character
  trim_ws = TRUE
) |>
  filter(grepl("^[A-Z]{4}$", icao))        # sanity check
