# The ModelsCloud platform calls <pkg>::gateway(func = "model_run", ...).
default_risk <- model_run(get_default_input())$risk

test_that("gateway dispatches func='model_run' and returns JSON", {
  js <- gateway(func = "model_run", model_input = get_default_input())
  expect_type(js, "character")
  parsed <- jsonlite::fromJSON(js)
  expect_true("risk" %in% names(parsed))
  expect_equal(parsed$risk, default_risk, tolerance = 1e-9)
})

test_that("gateway defaults func to model_run and strips control fields", {
  js <- gateway(model_input = get_default_input(), api_key = "x", session_id = "y")
  expect_equal(jsonlite::fromJSON(js)$risk, default_risk, tolerance = 1e-9)
})

test_that("gateway handles no-arg dispatch (get_default_input)", {
  parsed <- jsonlite::fromJSON(gateway(func = "get_default_input"))
  expect_true("age" %in% names(parsed))
})

test_that("gateway accepts an unwrapped + aliased payload", {
  js <- gateway(func = "model_run", age = 65, gender = "male", smkyears = 45,
                famhx = 0, pneu = 1, asb = 0, phist = 0)
  expect_equal(round(jsonlite::fromJSON(js)$risk, 4), 0.039)
})
