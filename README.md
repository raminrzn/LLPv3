# LLPv3 <img src="https://img.shields.io/badge/lifecycle-stable-brightgreen.svg" align="right"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/raminrzn/LLPv3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/raminrzn/LLPv3/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

A self-contained R implementation of **version 3 of the Liverpool Lung Project
(LLPv3)** risk model, which predicts the **5-year probability of lung cancer**
(Field et al., *Thorax*, 2021).

The implementation reproduces the **official Liverpool calculator**
([liverpoollungproject.org.uk/MLRV3](https://liverpoollungproject.org.uk/MLRV3/MLRCalculation.html))
**exactly** (verified to 0 difference across 3,120 input combinations). It has
**no dependencies** beyond base R and exposes two layers:

1. **`llpv3()`** — a vectorised model function for interactive / batch use in R.
2. The **ModelsCloud API** (`model_run()`, `get_sample_input()`,
   `get_default_input()`) for hosting on the
   [ModelsCloud](https://modelscloud.resp.core.ubc.ca/) platform.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("raminrzn/LLPv3")
```

---

## Quick start

```r
library(LLPv3)

# A 65-year-old man, 45 years of smoking, prior pneumonia
llpv3(age = 65, sex = "male", smoking_duration = 45,
      family_hist_lung_cancer = 0, pneumonia = 1,
      asbestos = 0, prior_cancer = 0)
#> [1] 0.039   (i.e. 3.9% over 5 years)

# Score a cohort via the ModelsCloud API
model_run(get_sample_input())
```

---

## The model

LLPv3 is a logistic model: `risk = logistic(alpha + bx)`, where

* **`alpha`** is an **age- and sex-specific baseline** log-odds, linearly
  interpolated between 5-year age bands, and
* **`bx`** is the sum of risk-factor log-odds below.

### Risk-factor coefficients (log-odds)

| Factor | Coefficient |
|---|---:|
| Prior pneumonia | 0.6025 |
| Asbestos exposure | 0.6343 |
| Prior (non-lung) cancer | 0.6754 |
| Family hx of lung cancer — early onset (<60y) | 0.7034 |
| Family hx of lung cancer — late onset (≥60y) | 0.1677 |
| Smoking duration 1–19 y | 0.7692 |
| Smoking duration 20–39 y | 1.4516 |
| Smoking duration 40–59 y | 2.5072 |
| Smoking duration ≥60 y | 2.72434 |

### Age/sex baseline (log-odds, by 5-year band)

| Age band | Male | Female |
|--|--:|--:|
| 40–44 | −9.84 | −10.37 |
| 45–49 | −8.94 | −8.53 |
| 50–54 | −8.09 | −7.93 |
| 55–59 | −7.41 | −6.97 |
| 60–64 | −6.75 | −6.69 |
| 65–69 | −6.34 | −6.46 |
| 70–74 | −6.09 | −5.96 |
| 75–79 | −5.61 | −5.70 |
| 80–84 | −5.46 | −5.89 |

The baseline used for a given age is interpolated between the band containing the
age and the band containing `age + 5`, matching the official calculator.

---

## Input coding

| Variable | Meaning | Coding |
|---|---|---|
| `age` | Age in years | numeric (intended 50–79) |
| `sex` | Sex | `"male"`/`"female"` (aliases `m`/`f`) or `0` male / `1` female |
| `smoking_duration` | Total years smoked | numeric (`0` = never-smoker) |
| `family_hist_lung_cancer` | Family hx of lung cancer | `0` none · `1` early onset (<60y) · `2` late onset (≥60y); strings `"none"`/`"early"`/`"late"` also accepted |
| `pneumonia` | Prior pneumonia | `1` yes · `0` no |
| `asbestos` | Asbestos exposure | `1` yes · `0` no |
| `prior_cancer` | Prior (non-lung) cancer | `1` yes · `0` no |

---

## ModelsCloud entry points

| Function | Description |
|---|---|
| `model_run(model_input)` | Score a named list (one person) or data frame; returns input plus `risk` and `risk_percent`. |
| `get_sample_input(n)` | An example cohort. |
| `get_default_input()` | One baseline person to modify. |

```r
library(modelscloud)
connect_to_model("raminrzn/llpv3", access_key = "YOUR_API_KEY")
result <- model_run(get_sample_input())
```

---

## Scope & caveats

* LLPv3 was **developed in a Caucasian population** and is intended for ages
  **50–79**. Inputs outside this range are scored using the nearest age band but
  should be interpreted with caution.
* The official authors note that LLPv3 is for **research use**. It differs from
  LLPv2, which is used in the UK Targeted Lung Health Checks and the UKLS trial.
* This package returns a probability and applies **no** eligibility threshold.
* Not a medical device; not a substitute for clinical judgement.

---

## Reference

> Field JK, Vulkan D, Davies MPA, Duffy SW, Gabe R. Liverpool Lung Project lung
> cancer risk stratification model: calibration and prospective validation.
> *Thorax.* 2021;76(2):161–168.
> doi:[10.1136/thoraxjnl-2020-215158](https://doi.org/10.1136/thoraxjnl-2020-215158)

## License

GPL-3. Model © its original authors; package implementation © Ramin Rezaeianzadeh.
