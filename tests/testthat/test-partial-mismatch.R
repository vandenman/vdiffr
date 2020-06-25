context("Test partial (mis)matches")

test_that("failing partial matches are detected", {

  test_file <- "test-partial.R"
  test_names <- c(
    "tolerance doesn't matter if partial match is not allowed",
    "tolerance too low"
  )

  for (test_name in test_names) {

    result <- subset_results(test_results, test_file, test_name)[[1]]
    expect_match(result$message, "Figures don't match: myplot-partial.svg\n")
    expect_is(result, "expectation_failure")

  }

})

test_that("passing partial matches are detected", {

  test_file <- "test-partial.R"
  test_names <- c(
    "adjusting distance function works",
    "increasing tolerance passes test",
    "adjusting expect_doppelganger takes precedence over options"
  )
  
  for (test_name in test_names) {

    result <- subset_results(test_results, test_file, test_name)[[1]]
    expect_is(result, "expectation_success")

  }

})