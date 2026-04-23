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

test_that("Local long test", {
  skip_if_offline()
  skip_on_ci()
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
