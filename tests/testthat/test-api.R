test_that("model_run returns risk for the sample cohort", {
  out <- model_run(get_sample_input())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4L)
  expect_true(all(c("risk", "risk_percent") %in% names(out)))
  expect_true(all(out$risk > 0 & out$risk < 1))
  expect_equal(out$risk_percent, round(100 * out$risk, 2))
})

test_that("model_run accepts a single-person list and the default", {
  out <- model_run(get_default_input())
  expect_equal(nrow(out), 1L)
  expect_true(out$risk > 0 && out$risk < 1)
  expect_equal(model_run(NULL)$risk, out$risk)  # NULL -> default
})

test_that("model_run validates the input columns", {
  bad <- get_default_input()
  bad$smoking <- 1
  expect_error(model_run(bad), "Unknown input variable")

  short <- get_default_input()
  short$age <- NULL
  expect_error(model_run(short), "Missing required variable")
})

test_that("get_sample_input(n) limits rows and validates n", {
  expect_equal(nrow(get_sample_input(2)), 2L)
  expect_error(get_sample_input(0), "positive integer")
})
