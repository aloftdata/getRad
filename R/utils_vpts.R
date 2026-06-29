add_reference_vpts <- function(x, source) {
  x$attributes$references <- c(vptsReferences[source], citation("getRad"))
  x
}
