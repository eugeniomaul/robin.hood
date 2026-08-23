\name{s_regress}
\alias{s_regress}
\title{
Linear Regression (Stata-style)
}
\description{
Emulates Stata's \code{regress} command. Fits an ordinary least squares linear
regression and prints the results in Stata's layout: the Source (ANOVA) block and
model-fit statistics, followed by the coefficient table with standard errors,
t statistics, p-values and 95 percent confidence intervals. Character predictors
are treated as factors.
}
\usage{
s_regress(dataframe, outcome, varlist, interactions = NULL,
          coeff.digits = 6, condition, verbose = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{outcome}{the dependent (outcome) variable, given without quotes
  (i.e. \code{mac_total}) or with quotes.}
  \item{varlist}{the predictor variable(s), as main effects. A single variable may
  be unquoted (i.e. \code{age}); several as a vector, unquoted
  (i.e. \code{c(age, sex)}) or quoted. Character predictors are treated as
  factors and expand to indicator terms.}
  \item{interactions}{optional character vector of interaction terms, written in R
  style: \code{"age:sex"} for the interaction only (Stata's \code{age#sex}), or
  \code{"age*sex"} for main effects plus interaction (Stata's \code{age##sex}).
  Every variable named must exist in the dataframe.}
  \item{coeff.digits}{number of decimal places for coefficients, standard errors
  and confidence limits. Default 6, matching Stata's display.}
  \item{condition}{optional. An unquoted logical expression selecting the rows to
  include, in the style of base \code{subset} (i.e. \code{age > 40}). Comparisons
  use \code{==} (not \code{=}).}
  \item{verbose}{if TRUE (the default) the results are printed to the console.}
}
\value{
Returned invisibly: an object of class \code{s_describe} containing the formatted
display, the fitted \code{lm} model, the raw coefficient matrix, the Wald
confidence intervals, and the model-fit statistics. The \code{s_describe} class
lets it be passed to \code{\link{s_export}}.
}
\note{
Confidence intervals are Wald intervals (estimate plus or minus 1.96 standard
errors), matching Stata. Interaction terms are labelled in R's style
(e.g. \code{age:sexmale}).
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_logistic}} For logistic regression.

\code{\link{t23}} For univariable and multivariable regression tables across many predictors.

\code{\link{s_export}} To export saved results into one csv file.
}
\examples{
data(drops)

## 1 Simple linear regression
s_regress(drops, va_log, age)

## 2 Several predictors
s_regress(drops, va_log, c(age, sex, md))

## 3 With an interaction (R style: age:sex)
s_regress(drops, va_log, c(age, sex), interactions = "age:sex")

## 4 Restricted to a subset of rows
s_regress(drops, va_log, age, condition = age > 40)
}