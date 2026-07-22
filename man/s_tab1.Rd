\name{s_tab1}
\alias{s_tab1}
\title{
One-Way Frequency Tables (Stata-style)
}
\description{
Emulates Stata's \code{tab1} command. Produces a separate one-way frequency table
for each variable listed. All variables are treated categorically: character,
numeric, logical and date/time variables are converted to factors for tabulation.
}
\usage{
s_tab1(dataframe, varlist, row = TRUE, miss = FALSE,
       maxlevels = 20, pct.digits = 2, verbose = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{varlist}{the variable(s) to tabulate. A single variable may be given
  unquoted (i.e. \code{sex}); several are given as a vector, either unquoted
  (i.e. \code{c(sex, eye)}) or quoted (i.e. \code{c("sex","eye")}). Each variable
  is tabulated separately as its own one-way table.}
  \item{row}{if TRUE (the default) percentages of the total are displayed
  alongside counts.}
  \item{miss}{if TRUE missing values (NA) are tabulated as a separate category.
  Default FALSE.}
  \item{maxlevels}{the maximum number of levels a variable may have to be
  tabulated. Variables exceeding this are skipped with a message. Default 20.}
  \item{pct.digits}{number of decimal places for percentages. Default 2.}
  \item{verbose}{if TRUE (the default) the tables are printed to the console and
  informative messages (such as skipped variables) are shown.}
}
\value{
Returned invisibly: an object of class \code{s_describe}, a list with elements
\code{tables} and \code{call.info}, in the same structure produced by
\code{\link{s_tab}}. Each entry in \code{tables} contains the formatted
\code{display} table, the raw \code{counts}, and the variable name. The
\code{s_describe} class allows the object to be passed to a future export function
that stacks several such tables into a single spreadsheet.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_tab}} For cross-tabulations and tests between variables.

\code{\link{t1}} To summarize variables in a dataframe without a grouping factor.

\code{\link{codebook}} To explore a dataframe including its variable names and types.
}
\examples{
data(drops)

## 1 One-way table for a single variable
s_tab1(drops, sex)

## 2 One-way tables for several variables at once
s_tab1(drops, c(sex, eye, diffic_level))

## 3 Include missing values as a category
s_tab1(drops, c(sex, eye), miss = TRUE)

## 4 Save the result for later export
tab1.obj <- s_tab1(drops, c(sex, eye, diffic_level))
}