# ---------------------------------------------------------------------------
# LLPv3 — Liverpool Lung Project risk model, version 3 (core)
#
# Field JK, Vulkan D, Davies MPA, Duffy SW, Gabe R. Liverpool Lung Project
# lung cancer risk stratification model: calibration and prospective
# validation. Thorax. 2021;76(2):161-168. doi:10.1136/thoraxjnl-2020-215158
#
# Predicts the 5-year probability of lung cancer. The linear predictor is an
# age- and sex-specific baseline log-odds (alpha) plus a sum of risk-factor
# log-odds (bx); risk = logistic(alpha + bx).
#
# Coefficients and the age/sex baseline table were extracted from the official
# Liverpool calculator (liverpoollungproject.org.uk/MLRV3, Scripts/RiskModel.js)
# and independently cross-checked against WeiLab4Research/AIConsult-LC.
#
# The model was developed in a Caucasian population and is intended for ages
# 50-79 (the published calculator restricts to this range).
# ---------------------------------------------------------------------------

# Risk-factor log-odds (the "bx" terms).
.llpv3_beta <- c(
  pneumonia    = 0.6025,   # prior pneumonia
  asbestos     = 0.6343,   # occupational asbestos exposure
  prior_cancer = 0.6754,   # prior (non-lung) malignancy
  fam_early    = 0.7034,   # family hx of lung cancer, onset < 60y
  fam_late     = 0.1677,   # family hx of lung cancer, onset >= 60y
  smk_1_19     = 0.7692,   # smoking duration  1-19 years
  smk_20_39    = 1.4516,   #                  20-39 years
  smk_40_59    = 2.5072,   #                  40-59 years
  smk_60p      = 2.72434   #                  >= 60 years
)

# Age/sex baseline log-odds (alpha) by 5-year band, ages 40-44 ... 80-84.
# Columns are the nine bands; rows are male and female.
.llpv3_alpha <- list(
  male   = c(-9.84, -8.94, -8.09, -7.41, -6.75, -6.34, -6.09, -5.61, -5.46),
  female = c(-10.37, -8.53, -7.93, -6.97, -6.69, -6.46, -5.96, -5.70, -5.89)
)

# 5-year age band index (1 = 40-44, ..., 9 = 80-84), clamped to the table range.
.llpv3_band <- function(age) {
  b <- floor((age - 40) / 5) + 1
  pmin(pmax(b, 1L), 9L)
}

# Age/sex-interpolated baseline: linear blend between the band containing `age`
# and the band containing `age + 5`, weighted by position within the 5-year band
# (matches the official calculator's alphaXY computation exactly).
.llpv3_alpha_xy <- function(age, female) {
  tbl <- ifelse(female == 1, "female", "male")
  a_lo <- mapply(function(a, s) .llpv3_alpha[[s]][.llpv3_band(a)],     age, tbl)
  a_hi <- mapply(function(a, s) .llpv3_alpha[[s]][.llpv3_band(a + 5)], age, tbl)
  y <- age %% 5
  ((5 - y - 0.5) * a_lo + (y + 0.5) * a_hi) / 5
}

# Map a sex vector ("male"/"female"/aliases or numeric 0=male, 1=female) to the
# female indicator used by the baseline table.
.llpv3_female <- function(sex) {
  if (is.numeric(sex)) {
    if (any(!sex %in% c(0, 1), na.rm = TRUE)) {
      stop("Numeric `sex` must be 0 (male) or 1 (female).", call. = FALSE)
    }
    return(as.integer(sex))
  }
  key <- tolower(trimws(as.character(sex)))
  out <- ifelse(key %in% c("male", "m", "man"), 0L,
         ifelse(key %in% c("female", "f", "woman"), 1L, NA_integer_))
  if (any(is.na(out))) {
    stop("Unrecognised `sex` value(s): ",
         paste(unique(sex[is.na(out)]), collapse = ", "),
         '. Use "male"/"female" or 0/1.', call. = FALSE)
  }
  out
}

# Smoking-duration band log-odds. Bands follow [1,20), [20,40), [40,60),
# [60, Inf); a duration < 1 year (never-smoker) contributes 0.
.llpv3_smoke_beta <- function(duration) {
  b <- .llpv3_beta
  cuts <- cut(duration, c(-Inf, 1, 20, 40, 60, Inf), right = FALSE,
              labels = c("0", "1_19", "20_39", "40_59", "60p"))
  vals <- c("0" = 0, "1_19" = b[["smk_1_19"]], "20_39" = b[["smk_20_39"]],
            "40_59" = b[["smk_40_59"]], "60p" = b[["smk_60p"]])
  unname(vals[as.character(cuts)])
}

# Family-history log-odds: 0 none, 1 early onset (<60y), 2 late onset (>=60y);
# string aliases "none"/"early"/"late" are also accepted.
.llpv3_family_beta <- function(family_hist_lung_cancer) {
  fh <- family_hist_lung_cancer
  if (!is.numeric(fh)) {
    key <- tolower(trimws(as.character(fh)))
    fh <- ifelse(key %in% c("none", "no", "0"), 0L,
          ifelse(key %in% c("early", "early onset", "1"), 1L,
          ifelse(key %in% c("late", "late onset", "2"), 2L, NA_integer_)))
    if (any(is.na(fh))) {
      stop("Unrecognised `family_hist_lung_cancer` value(s). Use 0/1/2 or ",
           '"none"/"early"/"late".', call. = FALSE)
    }
  }
  if (any(!fh %in% c(0, 1, 2), na.rm = TRUE)) {
    stop("`family_hist_lung_cancer` must be 0 (none), 1 (early onset <60y) ",
         "or 2 (late onset >=60y).", call. = FALSE)
  }
  b <- .llpv3_beta
  unname(c(`0` = 0, `1` = b[["fam_early"]], `2` = b[["fam_late"]])[as.character(fh)])
}

#' LLPv3 5-year lung cancer risk
#'
#' Computes the 5-year probability of lung cancer using version 3 of the
#' Liverpool Lung Project risk model (Field et al., *Thorax* 2021). All
#' arguments are vectorised and recycled to a common length.
#'
#' @details
#' The risk is `logistic(alpha + bx)` where `alpha` is an age- and sex-specific
#' baseline log-odds (linearly interpolated within 5-year age bands) and `bx`
#' is the sum of log-odds contributions from smoking duration, prior pneumonia,
#' asbestos exposure, prior (non-lung) cancer, and family history of lung
#' cancer. Risk-factor log-odds:
#'
#' * Prior pneumonia `0.6025`; asbestos exposure `0.6343`; prior cancer `0.6754`.
#' * Family history of lung cancer: early onset (<60y) `0.7034`,
#'   late onset (>=60y) `0.1677`.
#' * Smoking duration: 1-19y `0.7692`, 20-39y `1.4516`, 40-59y `2.5072`,
#'   >=60y `2.72434`.
#'
#' The model was developed in a Caucasian population and is intended for ages
#' 50-79; inputs outside this range are scored using the nearest age band but
#' should be interpreted with caution.
#'
#' @param age Age in years.
#' @param sex `"male"`/`"female"` (case-insensitive; aliases `m`/`f`) or numeric
#'   `0` (male) / `1` (female).
#' @param smoking_duration Total years smoked (`0` for never-smokers).
#' @param family_hist_lung_cancer Family history of lung cancer:
#'   `0` none, `1` early onset (relative diagnosed <60y), `2` late onset
#'   (relative diagnosed >=60y). Strings `"none"`/`"early"`/`"late"` also accepted.
#' @param pneumonia Prior pneumonia (`1` = yes, `0` = no).
#' @param asbestos Occupational asbestos exposure (`1` = yes, `0` = no).
#' @param prior_cancer Prior (non-lung) cancer diagnosis (`1` = yes, `0` = no).
#'
#' @return A numeric vector of 5-year lung cancer probabilities in `[0, 1]`.
#'
#' @references
#' Field JK, Vulkan D, Davies MPA, Duffy SW, Gabe R. Liverpool Lung Project lung
#' cancer risk stratification model: calibration and prospective validation.
#' *Thorax.* 2021;76(2):161-168. \doi{10.1136/thoraxjnl-2020-215158}
#'
#' @examples
#' # A 65-year-old male, 45 years smoking, prior pneumonia
#' llpv3(age = 65, sex = "male", smoking_duration = 45,
#'       family_hist_lung_cancer = 0, pneumonia = 1,
#'       asbestos = 0, prior_cancer = 0)
#'
#' # Vectorised over two people
#' llpv3(age = c(60, 72), sex = c("female", "male"),
#'       smoking_duration = c(30, 55), family_hist_lung_cancer = c(0, 1),
#'       pneumonia = c(0, 1), asbestos = c(0, 1), prior_cancer = c(0, 0))
#' @export
llpv3 <- function(age, sex, smoking_duration, family_hist_lung_cancer = 0,
                  pneumonia = 0, asbestos = 0, prior_cancer = 0) {

  n <- max(length(age), length(sex), length(smoking_duration),
           length(family_hist_lung_cancer), length(pneumonia),
           length(asbestos), length(prior_cancer))
  rep_to_n <- function(x) {
    if (length(x) == 1L) return(rep(x, n))
    if (length(x) != n)
      stop("All inputs must have length 1 or a common length.", call. = FALSE)
    x
  }
  age          <- as.numeric(rep_to_n(age))
  female       <- .llpv3_female(rep_to_n(sex))
  duration     <- as.numeric(rep_to_n(smoking_duration))
  fam_beta     <- .llpv3_family_beta(rep_to_n(family_hist_lung_cancer))
  pneumonia    <- as.numeric(rep_to_n(pneumonia))
  asbestos     <- as.numeric(rep_to_n(asbestos))
  prior_cancer <- as.numeric(rep_to_n(prior_cancer))

  b <- .llpv3_beta
  bx <- .llpv3_smoke_beta(duration) +
    fam_beta +
    b[["pneumonia"]]    * pneumonia +
    b[["asbestos"]]     * asbestos +
    b[["prior_cancer"]] * prior_cancer

  eta <- .llpv3_alpha_xy(age, female) + bx
  unname(exp(eta) / (1 + exp(eta)))
}
