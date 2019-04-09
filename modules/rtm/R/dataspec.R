#' PROSPECT model driver data
#'
#' `dataspec_X` returns the specific absorption coefficients for
#' PROSPECT parameters of the corresponding version. `refractive_X`
#' returns the refractive index values.
#' 
#' @rdname dataspec
#' @return For `refractive`, a 2101 (400-2500 nm) vector of refractive
#'   index values. For `dataspec` a matrix (2101 x N) of specific
#'   absorption coefficients.
#' @export
dataspec_4 <- function() {
  out <- .Fortran("getdataspec4", matrix(0, 2101, 3))[[1]]
  colnames(out) <- paste("k", c("Cab", "Cw", "Cm"),
                         sep = "_")
  out
}

#' @rdname dataspec
dataspec_5 <- function() {
  out <- .Fortran("getdataspec5", matrix(0, 2101, 5))[[1]]
  colnames(out) <- paste("k", c("Cab", "Car", "Brown", "Cw", "Cm"),
                         sep = "_")
  out
}

#' @rdname dataspec
dataspec_d <- function() {
  out <- .Fortran("getdataspec5", matrix(0, 2101, 5))[[1]]
  colnames(out) <- paste("k", c("Cab", "Car", "Canth", "Brown", "Cw", "Cm"),
                         sep = "_")
  out
}

#' @rdname dataspec
refractive_45 <- function() {
  .Fortran("getrefractive45", numeric(2101))[[1]]
}

#' @rdname dataspec
refractive_d <- function() {
  .Fortran("getrefractived", numeric(2101))[[1]]
}
