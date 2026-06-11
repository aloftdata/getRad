# Get vertical profile time series (VPTS) data from supported sources

Gets vertical profile time series data from supported sources and
returns it as a (list of) of [vpts
objects](http://adriaandokter.com/bioRad/reference/summary.vpts.md) or a
[`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

## Usage

``` r
get_vpts(
  radar,
  datetime,
  source = c("baltrad", "uva", "ecog-04003", "rmi", "birdcast", "dark_ecology"),
  return_type = c("vpts", "tibble"),
  ...,
  path = NULL
)
```

## Arguments

- radar:

  Name of the radar (odim code) as a character string (e.g. `"nlhrw"` or
  `"fikor"`).

- datetime:

  Either:

  - A [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) datetime
    (or `character` representation), for which the data file is
    downloaded.

  - A [`Date`](https://rdrr.io/r/base/Dates.html) date (or `character`
    representation), for which all data files are downloaded.

  - A vector of datetimes or dates, between which all data files are
    downloaded.

  - A
    [`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html),
    between which all data files are downloaded.

- source:

  Source of the data. One of `"baltrad"`, `"uva"`, `"ecog-04003"`,
  `"rmi"`, `"dark_ecology"` or `"birdcast"`. Only one source can be
  queried at a time. If not provided, `"baltrad"` is used.

- return_type:

  Type of object that should be returned. Either:

  - `"vpts"`: vpts object(s) (default).

  - `"tibble"`: a
    [`dplyr::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

- ...:

  Optional arguments, to
  [`bioRad::read_cajun()`](http://adriaandokter.com/bioRad/reference/read_cajun.md)
  when reading `"dark_ecology"` data.

- path:

  A local directory where data are read from. If specified the file
  structure is taken from the `source` argument. See details for an
  explanation of the file format.

## Value

Either a vpts object, a list of vpts objects or a tibble. See
[bioRad::summary.vpts](http://adriaandokter.com/bioRad/reference/summary.vpts.md)
for details.

## Details

For more details on supported sources, see
[`vignette("supported_sources")`](https://aloftdata.github.io/getRad/articles/supported_sources.md).

In case data is read from a directory, file in the directory should be
structures like they are in the monthly folders of the aloft repository.
To specify an alternative structure the
`"getRad.vpts_local_path_format_aloft"` option can be used. This can,
for example, be used to read daily data. Some example options for the
glue formatters are:

- `"{radar}/{year}/{radar}_vpts_{year}{month}.csv.gz"`: The default
  format, the same structure as the monthly directories in the aloft
  repository. Or as contained in the `tgz` files in the aloft zenodo
  repository.

- `"{substr(radar, 1,2)}/{radar}/{year}/{radar}_vpts_{year}{month}.csv.gz"`:
  The format as in the files in the zenodo aloft repository

- `"{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"`: The format as
  daily data is stored in aloft data

A similar option (`"getRad.vpts_local_path_format_aloft"`) exist for
reading dark ecology data. The default value here is
`"getRad.vpts_local_path_format_aloft"`. Here the option does refer to
the directories where the dark ecology files should be searched.

Besides the examples above there is a `date` object available for
formatting. Note that `day` and `month` are zero padded character
strings in the glue formating.

## Examples

``` r
if (FALSE) { # interactive()
# Get VPTS data for a single radar and date
get_vpts(radar = "bejab", datetime = "2023-01-01", source = "baltrad")
get_vpts(radar = "bejab", datetime = "2020-01-19", source = "rmi")

# Get VPTS data for multiple radars and a single date
get_vpts(
  radar = c("dehnr", "deflg"),
  datetime = lubridate::ymd("20171015"),
  source = "baltrad"
)

# Get VPTS data for a single radar and a date range
get_vpts(
  radar = "bejab",
  datetime = lubridate::interval(
    lubridate::ymd_hms("2023-01-01 00:00:00"),
    lubridate::ymd_hms("2023-01-02 00:14:00")
  ),
  source = "baltrad"
)
get_vpts("bejab", lubridate::interval("20210101", "20210301"))

# Get VPTS data for a single radar, date range and non-default source
get_vpts(radar = "bejab", datetime = "2016-09-29", source = "ecog-04003")

# Return a tibble instead of a vpts object
get_vpts(
  radar = "chlem",
  datetime = "2023-03-10",
  source = "baltrad",
  return_type = "tibble"
)
#' Get VPTS data from the public BirdCast NEXRAD archive
get_vpts(radar = "KABR", datetime = "2023-01-01", source = "birdcast")
}
```
