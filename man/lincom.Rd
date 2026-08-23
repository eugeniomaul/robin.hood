\name{lincom}
\alias{lincom}
\title{
Linear Combinations of Model Coefficients (Stata-style)
}
\description{
Emulates Stata's \code{lincom} command. Computes a linear combination of fitted
model coefficients, with its standard error, test statistic, p-value and 95
percent confidence interval. It works with plain R models (\code{lm}, \code{glm})
and with objects created by \code{\link{s_regress}} and \code{\link{s_logistic}},
from which the fitted model is extracted automatically. For logistic models the
result can be reported on the odds-ratio scale.
}
\usage{
lincom(model, betas = NULL, mag = NULL, or = FALSE)
}
\arguments{
  \item{model}{
a fitted model. This may be a plain R model object (such as one produced by
\code{lm} or \code{glm}), or an object created by \code{\link{s_regress}} or
\code{\link{s_logistic}}, in which case the underlying fitted model is extracted
automatically.
}
  \item{betas}{
a character vector with the names of the coefficients entering the linear
combination. The names must match those shown in the coefficient table exactly.
Factor and interaction terms carry R's level-based names (for example
\code{"sexmale"} or \code{"age:sexmale"}). The intercept may be given either as
\code{"(Intercept)"} or, as a convenience for Stata users, as \code{"_cons"} (the
label shown in the printed output of \code{s_regress}). If any name does not match
a coefficient in the model the function stops with an informative message rather
than silently ignoring it.
}
  \item{mag}{
a numeric vector, the same length as \code{betas}, giving the multiplier applied to
each coefficient in the linear combination. (For example, \code{betas = c("age")}
with \code{mag = c(10)} evaluates the effect of a 10-unit increase in age.)
}
  \item{or}{
logical. If FALSE (the default) the linear combination is reported on the model's
natural (linear predictor) scale. If TRUE the estimate and its confidence limits
are exponentiated to the odds-ratio scale, with the standard error computed by the
delta method, mirroring Stata's \code{lincom , or}. This is intended for logistic
models; applying it to a linear model would exponentiate a linear coefficient and
is not meaningful.
}
}
\value{
A list with the following elements:
  \item{linear.combination.for}{a character string describing the linear
  combination that was evaluated.}
  \item{or}{logical, indicating whether results are on the odds-ratio scale.}
  \item{result}{when \code{or = FALSE}, the summary of the linear combination as
  returned by \code{multcomp::glht}. When \code{or = TRUE}, a matrix with the odds
  ratio, its standard error, the z statistic, p-value and confidence limits.}
  \item{ci}{the confidence interval for the linear combination (on the natural
  scale when \code{or = FALSE}, on the odds-ratio scale when \code{or = TRUE}).}
}
\note{
The coefficient names in \code{betas} are those of the fitted model, which are R's
names rather than Stata's. Run the model first and read the coefficient names
directly from the \code{s_regress} or \code{s_logistic} output, then copy them into
\code{betas}. The intercept is the only term with a Stata-style alias
(\code{"_cons"}); all other terms use their R names.

When \code{or = TRUE} the z statistic and p-value are the same as on the linear
scale, since the test is invariant to the exponential transform; only the estimate,
standard error and confidence limits change scale.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_regress}} For linear regression producing a model usable here.

\code{\link{s_logistic}} For logistic regression producing a model usable here.

\code{\link{t23}} For univariable and multivariable regression tables.
}
\examples{
data(drops)

## 1 A linear combination from a plain R model
m <- lm(va_log ~ age + md, data = drops)
lincom(m, betas = c("age"), mag = c(10))         # effect of a 10-year increase

## 2 The same using an s_regress object (model extracted automatically)
fit <- s_regress(drops, va_log, c(age, md))
lincom(fit, betas = c("age"), mag = c(10))

## 3 Combining two coefficients, including the intercept via the _cons alias
lincom(fit, betas = c("_cons", "age"), mag = c(1, 50))

## 4 A logistic model, reported as an odds ratio
fit.log <- s_logistic(drops, total_success, c(age, sex))
lincom(fit.log, betas = c("age"), mag = c(10), or = TRUE)
}