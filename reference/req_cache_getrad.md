# Function to set the cache for a getRad specific httr2 request

Function to set the cache for a getRad specific httr2 request

## Usage

``` r
req_cache_getrad(
  req,
  use_cache = TRUE,
  max_age = getOption("getRad.max_cache_age_seconds", default = 6 * 60 * 60),
  max_n = getOption("getRad.max_cache_n", default = Inf),
  max_size = getOption("getRad.max_cache_size_bytes", default = 1024 * 1024 * 1024),
  ...
)
```

## Arguments

- req:

  `httr2` request.

- use_cache:

  Logical indicating whether to use the cache. Default is `TRUE`. If
  `FALSE` the cache is ignored and the file is fetched anew. This can
  also be useful if you want to force a refresh of the cache.

- max_n, max_age, max_size:

  Automatically prune the cache by specifying one or more of:

  - `max_age`: to delete files older than this number of seconds.

  - `max_n`: to delete files (from oldest to newest) to preserve at most
    this many files.

  - `max_size`: to delete files (from oldest to newest) to preserve at
    most this many bytes.

  The cache pruning is performed at most once per minute.

- ...:

  Additional arguments passed to
  [`httr2::req_cache()`](https://httr2.r-lib.org/reference/req_cache.html).
