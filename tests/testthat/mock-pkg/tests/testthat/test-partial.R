context("Partially mismatched plots")

# toggle this in combination with devtools::test()
options("vdiffr.partial_match_is_ok"           = TRUE)

plot_original <- ggplot2::ggplot(mtcars, ggplot2::aes(disp)) + ggplot2::geom_histogram()
plot_test <- plot_original + ggplot2::geom_point(x=100, y = 1)

test_that("tolerance doesn't matter if partial match is not allowed", {

  options("vdiffr.partial_match"           = FALSE)
  options("vdiffr.partial_match_tolerance" = 100)

  expect_doppelganger("myplot_partial", plot_test, "")
})

test_that("tolerance too low", {

  options("vdiffr.partial_match" = TRUE)
  options("vdiffr.partial_match_tolerance" = 1)

  expect_doppelganger("myplot_partial", plot_test, "")
})

test_that("increasing tolerance passes test", {

  options("vdiffr.partial_match" = TRUE)
  options("vdiffr.partial_match_tolerance" = 100)

  expect_doppelganger("myplot_partial", plot_test, "")
})

test_that("adjusting distance function works", {

  dist_fun <- function(...) -2
  options("vdiffr.partial_match"           = TRUE)
  options("vdiffr.partial_match_tolerance" = -1)
  options("vdiffr.partial_match_fun"       = dist_fun)

  expect_doppelganger("myplot_partial", plot_test, "")
})

test_that("adjusting expect_doppelganger takes precedence over options", {

  options_fun <- function(...) stop("I should not be called")
  count_arg_fun_called <<- FALSE
  arg_fun <- function(...) {
    count_arg_fun_called <<- TRUE
    0
  }

  options("vdiffr.partial_match"           = TRUE)
  options("vdiffr.partial_match_tolerance" = 1)
  options("vdiffr.partial_match_fun"       = options_fun)

  expect_doppelganger("myplot_partial", plot_test, "", partial_fun = arg_fun)
  # ensure that arg_fun was called
  expect_true(count_arg_fun_called)
})

# reset all options
options("vdiffr.partial_match"           = FALSE)
options("vdiffr.partial_match_is_ok"     = FALSE)
options("vdiffr.partial_match_tolerance" = 0)
options("vdiffr.partial_match_fun"       = vdiffr:::compare_abs_difference)
