# Get VPTS data from the Aloft bucket

Gets VPTS data from the Aloft bucket.

## Usage

``` r
get_vpts_aloft(
  radar_odim_code,
  rounded_interval,
  source = c("baltrad", "uva", "ecog-04003"),
  coverage = get_vpts_coverage_aloft()
)
```

## Arguments

- radar_odim_code:

  Radar ODIM code.

- rounded_interval:

  Interval to fetch data for, rounded to nearest day.

- source:

  Source of the data. One of `baltrad`, `uva` or `ecog-04003`.

- coverage:

  A data frame containing the coverage of the Aloft bucket. If not
  provided, it will be fetched from via the internet.

## Value

A tibble with VPTS data.

## Details

By default, data from the [Aloft bucket](https://aloftdata.eu/browse/)
are retrieved from <https://aloftdata.s3-eu-west-1.amazonaws.com>. This
can be changed by setting `options(getRad.aloft_data_url)` to any
desired url.

## Inner working

- Constructs the S3 paths for the VPTS files based on the input.

- Performs parallel HTTP requests to fetch the VPTS CSV data.

- Parses the response bodies with some assumptions about the column
  classes.

- Adds a column with the radar source.

- Overwrites the radar column with the radar_odim_code, all other values
  for this column are considered in error.
