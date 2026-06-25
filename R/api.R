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

#' Run the LLPv3 model (ModelsCloud entry point)
#'
#' Scores one or more people and returns the input augmented with the predicted
#' 5-year lung cancer risk.
#'
#' @param model_input A named list (one person) or data frame (one row per
#'   person) whose columns are the LLPv3 predictors. See [llpv3()] for the
#'   meaning and coding of each field, or call [get_sample_input()] /
#'   [get_default_input()] for ready-made examples. If `NULL`, the model's
#'   [get_default_input()] is used.
#'
#' @return A data frame: the input columns plus `risk` (5-year probability in
#'   `[0, 1]`) and `risk_percent` (percentage, rounded to two decimals).
#'
#' @seealso [llpv3()], [get_sample_input()], [get_default_input()]
#' @examples
#' model_run(get_sample_input())
#' model_run(get_default_input())
#' @export
model_run <- function(model_input = NULL) {
  if (is.null(model_input)) model_input <- get_default_input()

  unknown <- setdiff(names(model_input), .llpv3_vars)
  if (length(unknown) > 0) {
    stop("Unknown input variable(s): ", paste(unknown, collapse = ", "),
         ". Accepted: ", paste(.llpv3_vars, collapse = ", "), call. = FALSE)
  }

  df <- as.data.frame(model_input, stringsAsFactors = FALSE)

  missing <- setdiff(.llpv3_vars, names(df))
  if (length(missing) > 0) {
    stop("Missing required variable(s): ", paste(missing, collapse = ", "),
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
