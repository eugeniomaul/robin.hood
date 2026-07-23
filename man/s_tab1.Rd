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
       maxlevels = 20, max.groups = 5, condition, group.var,
       pct.digits = 2, verbose = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{varlist}{the variable(s) to tabulate. A single variable may be given
  unquoted (i.e. \code{sex}); several are given as a vector, either unquoted
  (i.e. \code{c(sex, eye)}) or quoted (i.e. \code{c("sex","eye")}). Each variable
  is tabulated separately as its own one-way table.}
  \item{row}{if TRUE (the default) percentages of the total are displayed
  alongside the counts.}
  \item{miss}{if TRUE missing values (NA) are tabulated as a separate category.
  Default FALSE.}
  \item{maxlevels}{the maximum number of levels a tabulated variable may have.
  Variables exceeding this are skipped with a message. Default 20.}
  \item{max.groups}{the maximum number of levels the grouping variable
  (\code{group.var}) may have. If it has more, the function stops with a message
  suggesting you check the variable or raise \code{max.groups}. Default 5.}
  \item{condition}{optional. An unquoted logical expression selecting the rows to
  include, in the style of base \code{subset} (i.e.
  \code{age>60 & !is.na(md)}). Comparisons use \code{==} (not \code{=}). Rows that
  are FALSE or missing are excluded.}
  \item{group.var}{optional. A grouping variable (given unquoted) by which the
  tabulations are split into separate tables, one set per level. If not a factor it
  is converted to one.}
  \item{pct.digits}{number of decimal places for percentages. Default 2.}
  \item{verbose}{if TRUE (the default) the tables are printed to the console and
  informative messages (such as skipped variables) are shown.}
}
\value{
Returned invisibly: an object of class \code{s_describe}, a list with elements
\code{tables} and \code{call.info}, in the same structure produced by
\code{\link{s_tab}}. Each entry in \code{tables} contains the formatted
\code{display} table, the raw \code{counts}, and the variable name. The
\code{s_describe} class allows the object to be passed to \code{\link{s_export}},
which stacks several such tables into a single spreadsheet file.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_tab}} For cross-tabulations and tests between variables.

\code{\link{s_summarize}} For summary statistics of numeric variables.

\code{\link{s_export}} To export several saved results into one csv file.

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

## 4 Restrict to a subset of rows
s_tab1(drops, c(sex, eye), condition = age > 60)

## 5 Split the tables by a grouping variable
s_tab1(drops, c(sex, eye), group.var = diffic_level)

## 6 Save the result for later export
tab1.obj <- s_tab1(drops, c(sex, eye, diffic_level))
}