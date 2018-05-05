context("Tidy spectra")

test_that(
  "Vector repeats works as expected",
  {
    x1 <- c("a", "a", "b", "a", "b", "b", "c", "d")
    target <- c(1, 2, 1, 3, 2, 3, 1, 1)
    expect_equal(vec_reps(x1), target)
    x2 <- rep("a", 10)
    expect_equal(vec_reps(x2), 1:10)
    x3 <- letters[1:15]
    expect_equal(vec_reps(x3), rep(1, 15))
  }
)

test_that(
  "Tidy spectra works",
  {
    s <- prospect(defparam("prospect_5"), 5)
    spec <- cbind(s, s, s, s, s)
    tidys <- tidy_spectra(spec)
    expect_equal(nrow(tidys), 4202 * 5)
    expect_equal(ncol(tidys), 5)
    expect_equal(unique(tidys$spectra_type), c("R", "T"))
    expect_equal(unique(tidys$col_name), c("reflectance", "transmittance"))
    expect_equal(unique(tidys$col_rep), 1:5)
  }
)
