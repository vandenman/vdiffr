context("Partially mismatched plots")

options("vdiffr.partial_match"           = TRUE)
options("vdiffr.partial_match_is_ok"     = TRUE)
options("vdiffr.partial_match_tolerance" = 1)

p1_orig <- ggplot2::ggplot(mtcars, ggplot2::aes(disp)) + ggplot2::geom_histogram()
p1_fail <- p1_orig + ggplot2::geom_point(x=100, y = 1)

test_that("mismatches are hard failures when NOT_CRAN is set", {
  options("vdiffr.partial_match_is_ok"     = FALSE)
  expect_doppelganger("myplot", p1_fail, "")
})

options("vdiffr.partial_match_is_ok"     = FALSE)
test_that("mismatches are hard failures", {
  options("vdiffr.partial_match_is_ok"     = TRUE)
  expect_doppelganger("myplot", p1_fail, "")
})

