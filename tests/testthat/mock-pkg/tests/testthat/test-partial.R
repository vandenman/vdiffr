context("Partially mismatched plots")

options("vdiffr.partial_match" = TRUE)

p1_orig <- ggplot2::ggplot(mtcars, ggplot2::aes(disp)) + ggplot2::geom_histogram()

# run manage cases with
# p1_fail <- p1_orig

# test cases with
p1_fail <- p1_orig + ggplot2::geom_point(x=100, y = 1)

test_that("tolerance doesn't matter if partial match is not allowed", {

  options("vdiffr.partial_match_is_ok"     = FALSE)
  options("vdiffr.partial_match_tolerance" = 100)
  # debugonce(expect_doppelganger)
  expect_error(expect_doppelganger("partial_1", p1_fail, ""))
})

test_that("tolerance too low", {

  options("vdiffr.partial_match_is_ok"     = TRUE)
  options("vdiffr.partial_match_tolerance" = 1)

  expect_error(expect_doppelganger("partial_2", p1_fail, ""))
})

test_that("increasing tolerance passes test", {

  options("vdiffr.partial_match_is_ok"     = TRUE)
  options("vdiffr.partial_match_tolerance" = 100)

  expect_doppelganger("partial_3", p1_fail, "")
})

test_that("adjusting distance function works", {

  dist_fun <- function(imgA, imgB) -2

  options("vdiffr.partial_match_is_ok"     = TRUE)
  options("vdiffr.partial_match_tolerance" = -1)
  options("vdiffr.partial_match_fun" = dist_fun)

  expect_doppelganger("partial_4", p1_fail, "")
})

test_that("adjusting expect_doppelganger takes precedence over options", {

  options_fun <- function(imgA, imgB) stop("I should not be called")
  arg_fun <- function(imgA, imgB) 0

  options("vdiffr.partial_match_is_ok"     = TRUE)
  options("vdiffr.partial_match_tolerance" = 1)
  options("vdiffr.partial_match_fun"       = options_fun)

  expect_doppelganger("partial_5", p1_fail, "", partial_fun = arg_fun)
})

options("vdiffr.partial_match"           = FALSE)
options("vdiffr.partial_match_is_ok"     = FALSE)
options("vdiffr.partial_match_tolerance" = 0)
options("vdiffr.partial_match_fun"       = vdiffr:::compare_abs_difference)
