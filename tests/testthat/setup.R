op <- options(getRad.progress = FALSE)

withr::defer(options(op), teardown_env())
