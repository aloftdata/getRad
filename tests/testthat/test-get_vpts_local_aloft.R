withr::with_tempdir({
  dir <- "./"
  skip_if_offline("aloftdata.s3-eu-west-1.amazonaws.com")
  dir.create(file.path(dir, "local_tests", "bewid", "2016"), recursive = T)
  dir.create(file.path(dir, "local_tests", "bejab", "2016"), recursive = T)
  local_dir <- file.path(dir, "local_tests")
  files <- c(
    "bewid/2016/bewid_vpts_20160201.csv",
    "bewid/2016/bewid_vpts_20160202.csv",
    "bewid/2016/bewid_vpts_20160203.csv",
    "bejab/2016/bejab_vpts_20160201.csv",
    "bejab/2016/bejab_vpts_20160202.csv"
  )
  for (i in files) {
    download.file(
      paste0("https://aloftdata.s3-eu-west-1.amazonaws.com/uva/daily/", i),
      file.path(dir, "local_tests", i),
      quiet = TRUE
    )
  }
  test_that("can read data", {
    withr::with_options(
      c(
        "getRad.vpts_local_path_format_aloft" = "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"
      ),
      {
        expect_s3_class(
          get_vpts("bewid", as.Date("2016-2-1"), path = local_dir),
          class = "vpts"
        )
        expect_s3_class(
          get_vpts(
            "bewid",
            as.Date("2016-2-1"),
            path = local_dir,
            return_type = "tibble"
          ),
          class = "tbl_df"
        )
        expect_type(
          ret <- get_vpts(
            c("bewid", "bejab"),
            as.Date("2016-2-1"),
            path = local_dir
          ),
          "list"
        )
        expect_length(ret, 2L)
        expect_s3_class(ret[[1]], class = "vpts")
        expect_s3_class(ret[[2]], class = "vpts")
        expect_s3_class(ret[[1]]$attributes$references, "bibentry")
        expect_s3_class(ret[[2]]$attributes$references, "bibentry")
        expect_length(ret[[2]]$attributes$references, 2)
        expect_identical(
          ret[[2]]$attributes$references[1],
          vptsReferences['baltrad']
        )
      }
    )
  })

  test_that("result for different sources is the same", {
    withr::with_options(
      c(
        "getRad.vpts_local_path_format_aloft" = "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"
      ),
      {
        ref <- get_vpts("bewid", as.Date("2016-2-1"), path = local_dir)
        expect_identical(
          ref,
          get_vpts(
            "bewid",
            as.Date("2016-2-1"),
            source = "baltrad",
            path = local_dir
          )
        )
        ref$attributes$references[1] <- vptsReferences["uva"]
        names(ref$attributes$references)[1] <- "uva"
        expect_identical(
          ref,
          get_vpts(
            "bewid",
            as.Date("2016-2-1"),
            source = "uva",
            path = local_dir
          )
        )
        ref$attributes$references[1] <- vptsReferences["ecog-04003"]
        names(ref$attributes$references)[1] <- "ecog-04003"

        expect_identical(
          ref,
          get_vpts(
            "bewid",
            as.Date("2016-2-1"),
            source = "ecog-04003",
            path = local_dir
          )
        )
      }
    )
  })

  test_that("errors outside of range or directory with corect format", {
    withr::with_options(
      c(
        "getRad.vpts_local_path_format_aloft" = "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"
      ),
      {
        expect_error(
          get_vpts("bewid", as.Date("2016-2-1"), path = dir),
          class = "getRad_error_files_not_in_source_dir"
        )
        expect_error(
          get_vpts("bewid", as.Date("2016-3-1"), path = local_dir),
          class = "getRad_error_files_not_in_source_dir"
        )
        expect_warning(
          ret <- get_vpts(
            "bewid",
            as.Date("2016-2-1") + -3:1,
            path = local_dir
          ),
          class = "getRad_warning_some_files_not_in_source_dir"
        )
        expect_s3_class(ret, "vpts")

        expect_warning(
          ret <- get_vpts(
            c("bewid", "behav"),
            as.Date("2016-2-1"),
            path = local_dir
          ),
          class = "getRad_warning_some_files_not_in_source_dir"
        )
        expect_s3_class(ret, "vpts")

        expect_true(all(grepl(
          "local_tests/behav/2016/behav_vpts_2016020[1|2].csv",
          rlang::catch_cnd(
            get_vpts(
              c("bewid", "behav"),
              as.Date("2016-2-1"),
              path = local_dir
            ),
            class = "getRad_warning_some_files_not_in_source_dir"
          )$missing_files
        )))
      }
    )
  })

  test_that("error on non existing files", {
    expect_error(
      get_vpts("bewid", as.Date("2016-2-1"), path = local_dir),
      class = "getRad_error_files_not_in_source_dir"
    )
    expect_error(
      get_vpts("bewid", as.Date("2016-2-10"), path = local_dir),
      class = "getRad_error_files_not_in_source_dir"
    )
    # test if by default the aloft file structure is used
    expect_error(
      get_vpts("bewid", as.Date("2016-2-10"), path = local_dir),
      "local_tests/bewid/2016/bewid_vpts_201602.csv.gz"
    )
  })
  test_that("datetime_intervalworksas expected", {
    withr::with_options(
      c(
        "getRad.vpts_local_path_format_aloft" = "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"
      ),
      {
        int_one <- lubridate::as.interval(
          as.POSIXct("2016-2-1 10:46"),
          as.POSIXct("2016-2-1 18:46")
        )
        int_two <- lubridate::as.interval(
          as.POSIXct("2016-2-1 10:46"),
          as.POSIXct("2016-2-2 18:46")
        )

        expect_s3_class(
          vpts_one <- get_vpts("bewid", int_one, path = local_dir),
          "vpts"
        )
        expect_s3_class(
          vpts_two <- get_vpts("bewid", int_two, path = local_dir),
          "vpts"
        )
        expect_all_true(vpts_one$datetime %within% int_one)
        expect_all_true(vpts_two$datetime %within% int_two)
        expect_all_true(vpts_one$datetime %in% vpts_two$datetime)
        expect_length(vpts_one$datetime, 88)
        expect_length(vpts_two$datetime, 352)
      }
    )
  })
})
