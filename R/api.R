# ---------------------------------------------------------------------------
# ModelsCloud API surface for LLPv3.
#
#   - model_run(model_input)  : synchronous; 5-year risk for each input row
#   - get_sample_input(n)     : an example cohort
#   - get_default_input()     : one baseline person to modify
# ---------------------------------------------------------------------------

# Predictor columns accepted by the API (single source of truth for validation).
.llpv3_vars <- c(
  "age", "sex", "smoking_duration", "family_hist_lung_cancer",
  "pneumonia", "asbestos", "prior_cancer"
)

# Accepted aliases (alias -> canonical), so common payloads work as-is.
# `female` maps to `sex` and shares its 0 = male / 1 = female coding.
.llpv3_alias <- c(
  gender = "sex", female = "sex",
  smkyears = "smoking_duration", smoking_years = "smoking_duration",
  duration_smoking = "smoking_duration", duration = "smoking_duration",
  famhx = "family_hist_lung_cancer", fam_hist = "family_hist_lung_cancer",
  family_history = "family_hist_lung_cancer",
  pneu = "pneumonia",
  asb = "asbestos",
  phist = "prior_cancer", prevcancer = "prior_cancer",
  prev_cancer = "prior_cancer", cancer_history = "prior_cancer"
)

# Normalise an incoming payload: accept either a wrapped `model_input` object or
# fields passed directly as named arguments (`dots`); rename known aliases to
# canonical names; drop unrecognised extra fields. Returns NULL when no input at
# all was supplied.
.llpv3_normalize <- function(model_input, dots) {
  if (is.null(model_input)) {
    if (length(dots) == 0) return(NULL)
    model_input <- dots
  }
  df <- as.data.frame(model_input, stringsAsFactors = FALSE)
  for (a in intersect(names(df), names(.llpv3_alias))) {
    canon <- .llpv3_alias[[a]]
    if (!canon %in% names(df)) names(df)[match(a, names(df))] <- canon
  }
  df[, intersect(names(df), .llpv3_vars), drop = FALSE]
}

#' Run the LLPv3 model (ModelsCloud entry point)
#'
#' Scores one or more people and returns the input augmented with the predicted
#' 5-year lung cancer risk.
#'
#' @details
#' Inputs may arrive either **wrapped** under `model_input`
#' (`model_run(model_input = list(age = 65, ...))` or a data frame) or
#' **unwrapped** as direct named arguments (`model_run(age = 65, ...)`, the form
#' produced by a raw `do.call(model_run, funcInput)`). Common aliases are mapped
#' to canonical names (`gender`/`female` -> `sex`, `smkyears` ->
#' `smoking_duration`, `famhx` -> `family_hist_lung_cancer`, `pneu` ->
#' `pneumonia`, `asb` -> `asbestos`, `phist` -> `prior_cancer`). Unrecognised
#' extra fields are ignored.
#'
#' @param model_input A named list (one person) or data frame (one row per
#'   person) whose columns are the LLPv3 predictors. See [llpv3()] for the
#'   meaning and coding of each field, or call [get_sample_input()] /
#'   [get_default_input()] for ready-made examples. If `NULL` and no fields are
#'   supplied via `...`, the model's [get_default_input()] is used.
#' @param ... Alternative to `model_input`: the predictor fields supplied
#'   directly as named arguments (e.g. from an unwrapped API call).
#'
#' @return A data frame: the input columns plus `risk` (5-year probability in
#'   `[0, 1]`) and `risk_percent` (percentage, rounded to two decimals).
#'
#' @seealso [llpv3()], [get_sample_input()], [get_default_input()]
#' @examples
#' model_run(get_sample_input())
#' model_run(get_default_input())
#' # Unwrapped + aliases:
#' model_run(age = 65, gender = "male", smkyears = 45, famhx = 0, pneu = 1,
#'           asb = 0, phist = 0)
#' @export
model_run <- function(model_input = NULL, ...) {
  df <- .llpv3_normalize(model_input, list(...))
  if (is.null(df)) df <- as.data.frame(get_default_input(), stringsAsFactors = FALSE)

  missing <- setdiff(.llpv3_vars, names(df))
  if (length(missing) > 0) {
    stop("Missing required variable(s): ", paste(missing, collapse = ", "),
         ". Accepted names (incl. aliases) are documented in ?model_run.",
         call. = FALSE)
  }

  df$risk <- llpv3(
    age                     = df$age,
    sex                     = df$sex,
    smoking_duration        = df$smoking_duration,
    family_hist_lung_cancer = df$family_hist_lung_cancer,
    pneumonia               = df$pneumonia,
    asbestos                = df$asbestos,
    prior_cancer            = df$prior_cancer
  )
  df$risk_percent <- round(100 * df$risk, 2)
  df
}

#' Example LLPv3 input cohort
#'
#' Returns a small data frame of example people that can be passed straight to
#' [model_run()].
#'
#' @param n Optional positive integer; if supplied, the first `n` rows are
#'   returned. Defaults to all rows.
#' @return A data frame of example people with the LLPv3 predictor columns.
#' @seealso [model_run()], [get_default_input()]
#' @examples
#' get_sample_input()
#' get_sample_input(n = 2)
#' @export
get_sample_input <- function(n = NULL) {
  df <- data.frame(
    age                     = c(65, 72, 58, 69),
    sex                     = c("male", "female", "male", "female"),
    smoking_duration        = c(45, 30, 0, 52),
    family_hist_lung_cancer = c(0, 1, 0, 2),
    pneumonia               = c(1, 0, 0, 1),
    asbestos                = c(0, 0, 1, 0),
    prior_cancer            = c(0, 0, 0, 1),
    stringsAsFactors        = FALSE
  )
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1L || n < 1L) {
      stop("`n` must be a single positive integer.", call. = FALSE)
    }
    df <- utils::head(df, n)
  }
  df
}

#' Default LLPv3 input
#'
#' Returns a single baseline person as a named list, ready to modify and pass
#' to [model_run()].
#'
#' @return A named list of default predictor values.
#' @seealso [model_run()], [get_sample_input()]
#' @examples
#' person <- get_default_input()
#' person$age <- 70
#' person$smoking_duration <- 50
#' model_run(person)
#' @export
get_default_input <- function() {
  list(
    age                     = 65,
    sex                     = "male",
    smoking_duration        = 40,
    family_hist_lung_cancer = 0,
    pneumonia               = 0,
    asbestos                = 0,
    prior_cancer            = 0
  )
}
