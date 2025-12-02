# Get vertical profile time series (VPTS) data from supported sources

Gets vertical profile time series data from supported sources and
returns it as a (list of) of [vpts
objects](http://adriaandokter.com/bioRad/reference/summary.vpts.md) or a
[`dplyr::tibble()`](https://dplyr.tidyverse.org/reference/reexports.html).

## Usage

``` r
get_vpts(
  radar,
  datetime,
  source = c("baltrad", "uva", "ecog-04003", "rmi"),
  return_type = c("vpts", "tibble")
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

  Source of the data. One of `"baltrad"`, `"uva"`, `"ecog-04003"` or
  `"rmi"`. Only one source can be queried at a time. If not provided,
  `"baltrad"` is used.

- return_type:

  Type of object that should be returned. Either:

  - `"vpts"`: vpts object(s) (default).

  - `"tibble"`: a
    [`dplyr::tibble()`](https://dplyr.tidyverse.org/reference/reexports.html).

## Value

Either a vpts object, a list of vpts objects or a tibble. See
[bioRad::summary.vpts](http://adriaandokter.com/bioRad/reference/summary.vpts.md)
for details.

## Details

For more details on supported sources, see
[`vignette("supported_sources")`](https://aloftdata.github.io/getRad/articles/supported_sources.md).

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
}
```
