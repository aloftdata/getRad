time <- Sys.time() - lubridate::hours(1)
time_floor_utc <- time |>
  lubridate::with_tz("UTC") |>
  lubridate::floor_date("5 mins")

test_that("Pvol can be downloaded from ord", {
  skip_if_offline("s3.waw3-1.cloudferro.com")
  pvol <- expect_s3_class(get_pvol("fiuta", time, use_opera_ord = T), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, time_floor_utc)
  pvol <- expect_s3_class(get_pvol("nlhrw", time, use_opera_ord = T), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, time_floor_utc)

  pvol <- expect_s3_class(get_pvol("frabb", time, use_opera_ord = T), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, time_floor_utc)
})
test_that("expected warnings from ord", {
  skip_if_offline("s3.waw3-1.cloudferro.com")
  expect_error(
    get_pvol("iedub", time, use_opera_ord = T),
    class = "getRad_warn_ord_irish_merging"
  )
  expect_error(
    get_pvol("nlhrw", time + lubridate::hours(3), use_opera_ord = T),
    class = "getRad_error_ord_no_keys_found"
  )
  expect_error(
    get_pvol("nlhrv", time + lubridate::hours(3), use_opera_ord = T),
    class = "getRad_error_ord_no_keys_found"
  )
  expect_error(
    get_pvol("nlhrv", time - lubridate::hours(30), use_opera_ord = T),
    class = "getRad_error_ord_no_keys_found"
  )
})
test_that("expected warnings from ord", {
  skip_if_offline("s3.waw3-1.cloudferro.com")
  expect_warning(
    get_pvol("mtgud", time, use_opera_ord = T),
    class = "getRad_warn_ord_conflicting_attributes"
  )
  expect_warning(
    suppressWarnings(
      get_pvol("plpas", time, use_opera_ord = T),
      classes = "getRad_warn_ord_conflicting_attributes"
    ),
    class = "getRad_warn_ord_polish_scans"
  )
})

test_that("internal merging functions", {
  pv <- get_pvol("fikor", as.POSIXct("2025-3-6") - 7200) |>
    dplyr::select(DBZH, VRADH)
  expect_identical(
    merge_pvols(
      merge_list <- list(pv |> dplyr::select(DBZH), pv |> dplyr::select(VRADH))
    ),
    pv
  )
  expect_error(
    class = 'getRad_error_ord_pvol_multi_geo',
    merge_pvols(
      modifyList(
        merge_list |> setNames(c('a', 'b')),
        list(a = list(geo = list(lat = 2)))
      ) |>
        unname()
    )
  )
  expect_error(
    class = 'getRad_error_ord_pvol_multi_time',
    merge_pvols(
      modifyList(
        merge_list |> setNames(c('a', 'b')),
        list(b = list(datetime = merge_list[[1]]$datetime + 300))
      ) |>
        unname()
    )
  )
  expect_error(
    class = 'getRad_error_ord_pvol_multi_radar',
    merge_pvols(
      modifyList(
        merge_list |> setNames(c('a', 'b')),
        list(b = list(radar = "fibor"))
      ) |>
        unname()
    )
  )
  merge_list_scan <- list(
    a = merge_list[[1]]$scans[[1]],
    b = merge_list[[2]]$scans[[1]]
  )
  expect_error(
    class = 'getRad_error_ord_scan_multi_geo',
    merge_scans(
      modifyList(
        merge_list_scan,
        list(b = list(geo = list(height = -1)))
      ) |>
        unname()
    )
  )
  expect_error(
    class = 'getRad_error_ord_scan_multi_radar',
    merge_scans(
      modifyList(
        merge_list_scan,
        list(b = list(radar = "aa"))
      ) |>
        unname()
    )
  )
  expect_error(
    class = 'getRad_error_ord_scan_multi_time',
    merge_scans(
      modifyList(
        merge_list_scan,
        list(b = list(datetime = pv$datetime - 300))
      ) |>
        unname()
    )
  )

  expect_error(
    class = 'getRad_error_ord_scan_duplicated_param',
    merge_scans(
      modifyList(
        merge_list_scan,
        list(b = list(params = merge_list_scan$a$params))
      ) |>
        unname()
    )
  )
})

test_that("Local long test", {
  skip_if_offline()
  skip_on_ci()

  countries <- aws.s3::get_bucket(
    "openradar-24h",
    prefix = glue::glue(
      '{format(Sys.Date(),"%Y/%m/%d", tz="UTC")}/'
    ),
    base_url = "s3.waw3-1.cloudferro.com",
    region = "",
    parse_response = F,
    delimiter = '/',
    max = Inf
  ) |>
    httr::content(as = "parsed", encoding = "UTF-8") |>
    xml2::xml_ns_strip() |>
    xml2::xml_find_all("CommonPrefixes/Prefix|Contents/Key") |>
    xml2::xml_text() |>
    strsplit("/") |>
    purrr::map_chr(tail, 1) |>
    tolower()

  radars <- list(
    BE = c("behel", "bejab", "bewid"),
    CH = c("chalb", "chdol", "chlem", "chppm", "chwei"),
    CZ = c("czbrd", "czska"),
    DE = c(
      "deasb",
      "deboo",
      "dedrs",
      "deeis",
      "deess",
      "defbg",
      "defld",
      "dehnr",
      "deisn",
      "demem",
      "deneu",
      "denhb",
      "deoft",
      "depro",
      "deros",
      "detur",
      "deumd"
    ),
    DK = c("dkbor", "dkrom", "dksam", "dksin", "dkste"),
    EE = "eesur",
    ES = c(
      "esahr",
      "esatn",
      "esbnv",
      "esclg",
      "esgld",
      "eslid",
      "esnjr",
      "espdg",
      "essft",
      "essse",
      "estjv"
    ),
    FI = c(
      "fianj",
      "fikan",
      "fikau",
      "fikes",
      "fikor",
      "fikuo",
      "filuo",
      "finur",
      "fipet",
      "fiuta",
      "fivih",
      "fivim"
    ),
    FR = c(
      "frabb",
      "fraja",
      "frale",
      "frave",
      "frbla",
      "frbol",
      "frbor",
      "frbou",
      "frcae",
      "frcol",
      "frgre",
      "frmcl",
      "frmom",
      "frmtc",
      "frnan",
      "frnim",
      "frniz",
      "fropo",
      "frpla",
      "frtou",
      "frtre",
      "frtro"
    ),
    HR = c("hrbil", "hrdeb", "hrgra", "hrpun", "hrulj"),
    IE = c("iedub", "iedub", "iesha"),
    IS = c("isbjo", "iskef", "isska"),
    LT = c("ltlau", "ltvil"),
    MT = "mtgud",
    NL = c("nldhl", "nlhrw"),
    NO = c(
      "noand",
      "nober",
      "nobml",
      "nohas",
      "nohfj",
      "nohgb",
      "nohur",
      "norsa",
      "norsg",
      "norst",
      "nosmn",
      "nosta"
    ),
    PL = c(
      "plbrz",
      "plgdy",
      "plgsa",
      "plleg",
      "plpas",
      "plpoz",
      "plram",
      "plrze",
      "plswi",
      "pluzr"
    ),
    RO = c("robar", "robob", "robuc", "rocra", "romed", "roora", "rotim"),
    SE = c(
      "seang",
      "seatv",
      "sebaa",
      "sehem",
      "sehuv",
      "sekaa",
      "sekrn",
      "sella",
      "seoer",
      "seosd",
      "sevax"
    ),
    SI = c("silis", "sipas")
  )
  time <- Sys.time() - 3600
  for (r in purrr::map_chr(within(radars, rm(IE)), head, 1)) {
    cli::cli_h1("{r}")
    withr::with_options(c(warn = 1), {
      p <- get_pvol(r, time, use_opera_ord = T)
      expect_s3_class(p, 'pvol')
      p |> print()
      cli::cli_inform(
        "{bioRad::attribute_table(p, 'param') |> unlist()|> unique() |> sort()}"
      )
    })
  }
})
