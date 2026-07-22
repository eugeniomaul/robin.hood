\name{s_summarize}
\alias{s_summarize}
\alias{s_su}
\title{
Summary Statistics for Continuous Variables (Stata-style)
}
\description{
Emulates Stata's \code{summarize} (\code{su}) and \code{tabstat} commands,
producing summary statistics for one or more numeric variables. The set of
statistics can be chosen with a simple \code{detail} level (1, 2 or 3) or listed
explicitly with \code{statistics}. Results can be split by a grouping variable and
laid out with variables in rows or in columns. \code{s_su} is an alias for
\code{s_summarize}.
}
\usage{
s_summarize(dataframe, varlist, detail = 1, cont.digits = 2,
            horizontal = TRUE, condition, group.var, max.levels = 5,
            miss.levels = FALSE, separator = 5, statistics = NULL,
            verbose = TRUE)

s_su(dataframe, varlist, detail = 1, cont.digits = 2,
     horizontal = TRUE, condition, group.var, max.levels = 5,
     miss.levels = FALSE, separator = 5, statistics = NULL,
     verbose = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{varlist}{the numeric variable(s) to summarize. A single variable may be
  given unquoted (i.e. \code{age}); several as a vector, unquoted
  (i.e. \code{c(age, k1, k2)}) or quoted (i.e. \code{c("age","k1")}).}
  \item{detail}{a preset level chosen when \code{statistics} is not supplied.
  \code{1} (the default) gives n, mean, sd, min, max. \code{2} gives n, mean, sd,
  median, p25, p75, min, max, and the Shapiro-Wilk p-value. \code{3} gives all
  available statistics and forces the layout to variables-in-columns /
  statistics-in-rows for readability.}
  \item{cont.digits}{number of decimal places for the statistics. Default 2. The
  Shapiro-Wilk p-value always shows at least 4 decimals.}
  \item{horizontal}{if TRUE (the default) statistics are shown across columns and
  variables down the rows. If FALSE the layout is transposed (statistics down the
  rows, variables across the columns). \code{detail = 3} always uses the
  transposed layout regardless of this argument.}
  \item{condition}{optional. An unquoted logical expression selecting the rows to
  include, in the style of base \code{subset} (i.e.
  \code{age>40 & !is.na(acd)}). Comparisons use \code{==} (not \code{=}). Rows that
  are FALSE or missing are excluded.}
  \item{group.var}{optional. A grouping variable (given unquoted) by which results
  are split into separate tables. If not a factor it is converted to one.}
  \item{max.levels}{the maximum number of levels the grouping variable may have.
  If it has more, the function stops with a message suggesting you check the
  variable or raise \code{max.levels}. Default 5.}
  \item{miss.levels}{if FALSE (the default) observations with a missing value of
  the grouping variable are not given their own table. If TRUE a separate table is
  produced for the missing group.}
  \item{separator}{a horizontal rule is drawn every \code{separator} rows of the
  output for readability. Default 5. Set to 0 to omit the rules.}
  \item{statistics}{an optional character vector of statistics to display. When
  supplied it overrides \code{detail}. Accepted values: \code{n}, \code{mean},
  \code{sd}, \code{se} (standard error of the mean), \code{variance},
  \code{median}, \code{min}, \code{max}, \code{skewness}, \code{kurtosis},
  \code{shapiro.test} (shown with at least 4 decimals), \code{q} (shorthand for
  \code{p25}, \code{p50}, \code{p75}), and the percentiles \code{p1}, \code{p5},
  \code{p10}, \code{p25}, \code{p50}, \code{p75}, \code{p90}, \code{p95},
  \code{p99}. Skewness and kurtosis use Stata's definitions (biased skewness;
  non-excess kurtosis, so a normal distribution gives about 3).}
  \item{verbose}{if TRUE (the default) the table(s) are printed to the console.}
}
\value{
Returned invisibly: an object of class \code{s_describe}, a list with elements
\code{tables} and \code{call.info}, in the same structure produced by
\code{\link{s_tab}}. Each entry in \code{tables} contains the formatted
\code{display} table and a numeric \code{stats.matrix} of the underlying values.
The shared \code{s_describe} class lets the object be passed, alongside
\code{s_tab}/\code{s_tab1} objects, to a future export function that stacks several
results into a single spreadsheet.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_tab}} and \code{\link{s_tab1}} For frequency tables of categorical variables.

\code{\link{t1}} To summarize mixed variable types in a compact table.

\code{\link{codebook}} To explore a dataframe including its variable names and types.
}
\examples{
data(drops)

## 1 Default summary (detail = 1: n, mean, sd, min, max)
s_summarize(drops, c(age, va_log, md))

## 2 A fuller preset (detail = 2 adds median, quartiles, Shapiro-Wilk)
s_summarize(drops, c(age, va_log, md), detail = 2)

## 3 Everything (detail = 3, statistics in rows, variables in columns)
s_summarize(drops, c(age, va_log, md), detail = 3)

## 4 Choose statistics explicitly (overrides detail)
s_summarize(drops, c(age, md), statistics = c("n","mean","median","p25","p75"))

## 5 Split by a grouping variable
s_summarize(drops, c(age, md), detail = 2, group.var = sex)

## 6 Restrict to a subset of rows
s_summarize(drops, c(age, md), condition = age > 40 & !is.na(md))

## 7 s_su is the same function
s_su(drops, c(age, md))
}