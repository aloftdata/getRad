# Get VPTS file coverage from supported sources

Gets the VPTS file coverage from supported sources per radar and date.

## Usage

``` r
get_vpts_coverage(source = c("baltrad", "uva", "ecog-04003", "rmi"), ...)
```

## Arguments

- source:

  Source of the data. One or more of `"baltrad"`, `"uva"`,
  `"ecog-04003"` or `"rmi"`. If not provided, `"baltrad"` is used.
  Alternatively `"all"` can be used if data from all sources should be
  returned.

- ...:

  Arguments passed on to internal functions.

## Value

A `data.frame` or `tibble` with at least three columns, `source`,
`radar` and `date` to indicate the combination for which data exists.

## Examples

``` r
if (FALSE) { # interactive()
get_vpts_coverage()
}
```
