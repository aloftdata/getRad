dir <- tempdir()
skip_if_offline("aloftdata.s3-eu-west-1.amazonaws.com")
dir.create(file.path(dir, "local_tests", "bewid", "2016"), recursive = T)
dir.create(file.path(dir, "local_tests", "bejab", "2016"), recursive = T)
local_dir <- file.path(dir, "local_tests")
files <- c(
  "bewid/2016/bewid_vpts_20160201.csv",
  "bewid/2016/bewid_vpts_20160202.csv",
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
      "getRad.vpts_local_path_format" = "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"
    ),
    {
      expect_s3_class(
        get_vpts("bewid", as.Date("2016-2-1"), source = local_dir),
        class = "vpts"
      )
      expect_type(
        ret <- get_vpts(
          c("bewid", "bejab"),
          as.Date("2016-2-1"),
          source = local_dir
        ),
        "list"
      )
      expect_length(ret, 2L)
      expect_s3_class(ret[[1]], class = "vpts")
      expect_s3_class(ret[[2]], class = "vpts")
    }
  )
})


test_that("errors outside of range or directory with corect format", {
  withr::with_options(
    c(
      "getRad.vpts_local_path_format" = "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"
    ),
    {
      expect_error(
        get_vpts("bewid", as.Date("2016-2-1"), source = dir),
        class = "getRad_error_files_not_in_source_dir"
      )
      expect_error(
        get_vpts("bewid", as.Date("2016-3-1"), source = local_dir),
        class = "getRad_error_files_not_in_source_dir"
      )
      expect_warning(
        ret <- get_vpts(
          "bewid",
          as.Date("2016-2-1") + -3:1,
          source = local_dir
        ),
        class = "getRad_warning_some_files_not_in_source_dir"
      )
      expect_s3_class(ret, "vpts")

      expect_warning(
        ret <- get_vpts(
          c("bewid", "behav"),
          as.Date("2016-2-1"),
          source = local_dir
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
            source = local_dir
          ),
          class = "getRad_warning_some_files_not_in_source_dir"
        )$missing_files
      )))
    }
  )
})

test_that("error on non existing files", {
  expect_error(
    get_vpts("bewid", as.Date("2016-2-1"), source = local_dir),
    class = "getRad_error_files_not_in_source_dir"
  )
  expect_error(
    get_vpts("bewid", as.Date("2016-2-10"), source = local_dir),
    class = "getRad_error_files_not_in_source_dir"
  )
  # test if by default the aloft file structure is used
  expect_error(
    get_vpts("bewid", as.Date("2016-2-10"), source = local_dir),
    "local_tests/bewid/2016/bewid_vpts_201602.csv.gz"
  )
})
unlink(local_dir)
