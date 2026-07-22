\name{s_tab}
\alias{s_tab}
\title{
Cross-Tabulation of Variables (Stata-style)
}
\description{
Emulates Stata's \code{tabulate} command. With a single variable it produces a
one-way frequency table. With two or more variables the first is cross-tabulated
against each of the others in turn. Row and/or column percentages can be shown,
and chi-squared and Fisher's exact tests requested. All variables are treated
categorically: character, numeric, logical and date/time variables are converted
to factors for tabulation.
}
\usage{
s_tab(dataframe, varlist, row = TRUE, col = FALSE, chi2 = FALSE,
      exact = FALSE, miss = FALSE, maxlevels = 10,
      pct.digits = 2, verbose = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{varlist}{the variable(s) to tabulate. A single variable may be given
  unquoted (i.e. \code{sex}); several are given as a vector, either unquoted
  (i.e. \code{c(sex, eye)}) or quoted (i.e. \code{c("sex","eye")}). With two or
  more variables the first is the row variable, crossed against each of the rest.}
  \item{row}{if TRUE (the default) row percentages are displayed in two-way
  tables, and percentages of the total are shown in one-way tables.}
  \item{col}{if TRUE column percentages are displayed in two-way tables. Default
  FALSE.}
  \item{chi2}{if TRUE a Pearson chi-squared test is performed for each two-way
  table and its result displayed. Default FALSE.}
  \item{exact}{if TRUE Fisher's exact test is performed for each two-way table.
  For large or sparse tables the function automatically switches to a Fisher test
  with a simulated p-value to avoid excessive computation time, and labels the
  result accordingly. Default FALSE.}
  \item{miss}{if TRUE missing values (NA) are tabulated as a separate category.
  Default FALSE.}
  \item{maxlevels}{the maximum number of levels a variable may have to be
  tabulated. Variables exceeding this are skipped with a message. Default 10.}
  \item{pct.digits}{number of decimal places for percentages. Default 2.}
  \item{verbose}{if TRUE (the default) the table(s) are printed to the console and
  informative messages (such as skipped variables) are shown.}
}
\value{
Returned invisibly: an object of class \code{s_describe}, a list with elements
\code{tables} and \code{call.info}. \code{tables} is a named list with one entry
per tabulation; each entry contains \code{display} (the formatted table as
printed), \code{counts} (the raw count table), \code{row.pct} and \code{col.pct}
(numeric percentage matrices, or NULL), \code{test} (test results, or NULL), and
the row/column variable names. The \code{s_describe} class allows the object to be
passed to a future export function that stacks several such tables into a single
spreadsheet.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_tab1}} For one-way frequency tables of several variables.

\code{\link{t1.by}} To describe variables across a grouping factor with tests.

\code{\link{codebook}} To explore a dataframe including its variable names and types.
}
\examples{
data(drops)

## 1 One-way table of a single variable
s_tab(drops, sex)

## 2 Cross-tabulation, first variable against the rest
s_tab(drops, c(sex, eye))

## 3 With column percentages and a chi-squared test
s_tab(drops, c(sex, eye), col = TRUE, chi2 = TRUE)

## 4 Fisher's exact test, tabulating missing values as a category
s_tab(drops, c(sex, eye), exact = TRUE, miss = TRUE)

## 5 Save the result for later export
tab.obj <- s_tab(drops, c(sex, eye), chi2 = TRUE)
}