\name{s_logistic}
\alias{s_logistic}
\title{
Logistic Regression (Stata-style)
}
\description{
Emulates Stata's \code{logistic} command. Fits a logistic regression and prints
odds ratios with their standard errors (delta method), z statistics, p-values and
95 percent confidence intervals, together with the model-fit header (number of
observations, likelihood-ratio chi-squared, its p-value, log likelihood and
McFadden's pseudo R-squared). Character predictors are treated as factors.
}
\usage{
s_logistic(dataframe, outcome, varlist, interactions = NULL,
           coeff.digits = 6, condition, verbose = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{outcome}{the binary dependent (outcome) variable, given without quotes
  (i.e. \code{thin_choroid}) or with quotes. It should be coded 0/1 (or as a
  two-level factor).}
  \item{varlist}{the predictor variable(s), as main effects. A single variable may
  be unquoted (i.e. \code{glaucoma}); several as a vector, unquoted
  (i.e. \code{c(glaucoma, age)}) or quoted. Character predictors are treated as
  factors and expand to indicator terms.}
  \item{interactions}{optional character vector of interaction terms, written in R
  style: \code{"glaucoma:age"} for the interaction only (Stata's
  \code{glaucoma#age}), or \code{"glaucoma*age"} for main effects plus interaction
  (Stata's \code{glaucoma##age}). Every variable named must exist in the
  dataframe.}
  \item{coeff.digits}{number of decimal places for odds ratios, standard errors
  and confidence limits. Default 6, matching Stata's display.}
  \item{condition}{optional. An unquoted logical expression selecting the rows to
  include, in the style of base \code{subset} (i.e. \code{age > 40}). Comparisons
  use \code{==} (not \code{=}).}
  \item{verbose}{if TRUE (the default) the results are printed to the console.}
}
\value{
Returned invisibly: an object of class \code{s_describe} containing the formatted
display, the fitted \code{glm} model, the raw coefficient matrix, the odds ratios
and their confidence intervals, and the model-fit statistics. The \code{s_describe}
class lets it be passed to \code{\link{s_export}}.
}
\note{
Confidence intervals are Wald intervals on the odds-ratio scale, and the standard
errors shown are on the odds-ratio scale (delta method), matching Stata's
\code{logistic}. The intercept (\code{_cons}) is not printed, as in Stata. The
likelihood-ratio test compares the fitted model with a null model fit on the same
observations. Interaction terms are labelled in R's style.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_regress}} For linear regression.

\code{\link{t23}} For univariable and multivariable regression tables across many predictors.

\code{\link{s_export}} To export saved results into one csv file.
}
\examples{
data(drops)

## 1 Simple logistic regression (odds ratio for one predictor)
s_logistic(drops, total_success, age)

## 2 Several predictors
s_logistic(drops, total_success, c(age, sex, md))

## 3 A categorical predictor (expands to indicator terms)
s_logistic(drops, total_success, diffic_level)

## 4 With an interaction
s_logistic(drops, total_success, c(age, sex), interactions = "age:sex")
}