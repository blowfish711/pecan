#' Combine spectra by wavelength
#'
#' @param ... Spectra to combine 
#' @export
cbind.spectra <- function(...) {
  dots <- list(...)
  lens <- vapply(dots, ncol, numeric(1))
  waves <- lapply(dots, wavelengths)
  new_waves <- Reduce(union, waves)
  types <- lapply(dots, spectra_types)
  types2 <- Map(rep_if_one, types, lens)
  new_types <- Reduce(c, types2)
  names_list <- lapply(dots, colnames)
  m <- spectra(
    matrix(NA_real_, length(new_waves), sum(lens)),
    wavelengths = new_waves,
    type = new_types
  )
  colnames(m) <- as.character(seq(1, sum(lens)))
  j <- 1
  for (i in seq_along(dots)) {
    jseq <- seq(j, j + lens[i] - 1)
    m[[waves[[i]], jseq]] <- dots[[i]]
    if (!is.null(names_list[[i]])) {
      colnames(m)[jseq] <- names_list[[i]]
    }
    j <- j + lens[i]
  }
  m
}

rep_if_one <- function(x, n) {
  if (length(x) == 1) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop(
      "x must be length 1 or n. ",
      "Current x has length ", length(x),
      " but n value is ", n, "."
    )
  }
}
