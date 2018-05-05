#' Tidy spectra
#'
#' Convert spectra to a long tidy `tbl_df`.
#'
#' Return object has the following columns.
#' 
#' * `col_name` -- Matches the column name of the original spectra
#' * `col_rep` -- The count of a column name in the spectra, useful for 
#' identifying repeat column names.
#' * `spectra_type` -- Type of spectra
#' * `wavelength` -- Wavelength, in nm
#' * `value` -- The spectra value
#'
#' @param spectra Object of class `spectra`.
#' @return Long `tbl_df`. See Details.
#' @export
tidy_spectra <- function(spectra) {
  stopifnot(
    is_spectra(spectra),
    requireNamespace("tibble"),
    requireNamespace("tidyr"),
    requireNamespace("dplyr")
  )
  waves <- wavelengths(spectra)
  types <- spectra_types(spectra)
  col_names <- colnames(spectra)
  col_reps <- vec_reps(col_names)
  new_names <- paste(col_names, col_reps, sep = ".")
  colnames(spectra) <- new_names
  types_df <- tibble::tibble(
    col_name = col_names,
    col_rep = col_reps,
    new_name = new_names,
    spectra_type = types
  )
  class(spectra) <- "matrix"
  tibble::as_tibble(spectra) %>%
    tibble::add_column(wavelength = waves) %>%
    tidyr::gather(new_name, value, -wavelength) %>%
    dplyr::left_join(types_df, "new_name") %>%
    dplyr::select(col_name, col_rep, spectra_type, wavelength, value)
}

#' Index repeated elements of a vector
#'
#' For example, vector `a,a,b,b,a,c` would have indices `1,2,1,2,3,1`.
#'
#' @param x Vector whose contents should be indexed
#' @return Vector of integer indices equal to length of `x`
#' @export
vec_reps <- function(x) {
  ave(rep(TRUE, length(x)), x, FUN = cumsum)
}
