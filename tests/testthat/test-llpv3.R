# Reference values produced by the official Liverpool LLPv3 calculator
# (liverpoollungproject.org.uk/MLRV3) for these exact inputs. Columns:
# gender (0=male,1=female), age, smoking years, family hx (0/1/2),
# pneumonia, asbestos, prior cancer, expected 5-year risk (%).
ref <- read.table(text = "
0 50  0 0 0 0 0  0.03
0 65 45 0 1 0 0  3.90
1 72 25 1 0 0 0  2.47
1 67 65 2 1 0 1 11.51
0 58 10 0 0 1 0  0.39
0 78 65 1 1 1 1 45.88
1 55 25 1 0 0 0  0.83
0 63 45 0 0 0 1  3.62
1 70 45 2 0 0 0  3.70
0 60 25 0 1 0 0  0.94
", col.names = c("gender", "age", "smk", "fam", "pneu", "asb", "prev", "pct"))

test_that("llpv3 reproduces the official calculator", {
  got <- llpv3(
    age = ref$age,
    sex = ifelse(ref$gender == 1, "female", "male"),
    smoking_duration = ref$smk,
    family_hist_lung_cancer = ref$fam,
    pneumonia = ref$pneu,
    asbestos = ref$asb,
    prior_cancer = ref$prev
  )
  expect_equal(round(100 * got, 2), ref$pct, tolerance = 1e-8)
})

test_that("sex accepts strings, aliases and numeric codes", {
  args <- list(age = 65, smoking_duration = 40, family_hist_lung_cancer = 0,
               pneumonia = 0, asbestos = 0, prior_cancer = 0)
  m1 <- do.call(llpv3, c(list(sex = "male"), args))
  m2 <- do.call(llpv3, c(list(sex = "M"), args))
  m3 <- do.call(llpv3, c(list(sex = 0), args))
  f1 <- do.call(llpv3, c(list(sex = "female"), args))
  f2 <- do.call(llpv3, c(list(sex = 1), args))
  expect_equal(m1, m2); expect_equal(m1, m3)
  expect_equal(f1, f2)
  expect_false(isTRUE(all.equal(m1, f1)))
})

test_that("family history accepts strings and codes; risk rises with factors", {
  base <- llpv3(age = 65, sex = "male", smoking_duration = 40,
                family_hist_lung_cancer = 0, pneumonia = 0,
                asbestos = 0, prior_cancer = 0)
  early <- llpv3(age = 65, sex = "male", smoking_duration = 40,
                 family_hist_lung_cancer = "early", pneumonia = 0,
                 asbestos = 0, prior_cancer = 0)
  late  <- llpv3(age = 65, sex = "male", smoking_duration = 40,
                 family_hist_lung_cancer = 2, pneumonia = 0,
                 asbestos = 0, prior_cancer = 0)
  expect_gt(early, late)   # early-onset confers higher risk than late-onset
  expect_gt(late, base)
})

test_that("invalid inputs are rejected", {
  expect_error(
    llpv3(age = 65, sex = "robot", smoking_duration = 40),
    "Unrecognised .sex."
  )
  expect_error(
    llpv3(age = 65, sex = "male", smoking_duration = 40,
          family_hist_lung_cancer = 9),
    "family_hist_lung_cancer"
  )
})

test_that("never-smokers contribute no smoking-duration term", {
  r <- llpv3(age = 60, sex = "female", smoking_duration = 0,
             family_hist_lung_cancer = 0, pneumonia = 0,
             asbestos = 0, prior_cancer = 0)
  expect_true(r > 0 && r < 0.01)
})
