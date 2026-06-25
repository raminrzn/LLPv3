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

test_that("model_run ignores unknown fields but requires the core predictors", {
  ok <- get_default_input()
  ok$some_extra <- 1                     # extra field -> ignored, not fatal
  expect_no_error(model_run(ok))

  short <- get_default_input()
  short$age <- NULL
  expect_error(model_run(short), "Missing required variable")
})

test_that("model_run accepts unwrapped named args (do.call style)", {
  d <- get_default_input()
  expect_equal(model_run(d)$risk, do.call(model_run, d)$risk)
})

test_that("model_run accepts aliases (gender/female, smkyears, pneu, ...)", {
  viaAlias <- model_run(list(age = 65, gender = "male", smkyears = 45,
                             famhx = 0, pneu = 1, asb = 0, phist = 0))
  viaCanon <- model_run(list(age = 65, sex = "male", smoking_duration = 45,
                             family_hist_lung_cancer = 0, pneumonia = 1,
                             asbestos = 0, prior_cancer = 0))
  expect_equal(viaAlias$risk, viaCanon$risk)

  # female=0 (alias for sex) should equal sex="male"
  f0 <- model_run(list(age = 65, female = 0, smoking_duration = 40,
                       family_hist_lung_cancer = 0, pneumonia = 0,
                       asbestos = 0, prior_cancer = 0))
  m  <- model_run(list(age = 65, sex = "male", smoking_duration = 40,
                       family_hist_lung_cancer = 0, pneumonia = 0,
                       asbestos = 0, prior_cancer = 0))
  expect_equal(f0$risk, m$risk)
})

test_that("get_sample_input(n) limits rows and validates n", {
  expect_equal(nrow(get_sample_input(2)), 2L)
  expect_error(get_sample_input(0), "positive integer")
})
