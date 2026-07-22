\name{s_drop}
\alias{s_drop}
\title{
Drop Selected Variables and/or Observations from a Dataframe (Stata-style)
}
\description{
Emulates Stata's \code{drop} and \code{drop if} commands. With a \code{varlist}
it removes the listed variables (columns), keeping all others in their original
order. With a \code{condition} it removes the rows for which the condition is TRUE
(Stata's \code{drop if}), keeping the rest. Both may be given in one call, in which
case the matching rows and the listed columns are both removed. By default the
change is applied in place, overwriting the original object in your workspace.
}
\usage{
s_drop(dataframe, varlist, condition, verbose = TRUE, overwrite = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{varlist}{optional. The variable(s) to drop. A single variable may be given
  unquoted (i.e. \code{eye}); several as a vector, unquoted
  (i.e. \code{c(eye, idn)}) or quoted (i.e. \code{c("eye","idn")}). All variables
  not listed are kept, in their original order. If omitted, all columns are kept
  and only the \code{condition} is applied.}
  \item{condition}{optional. An unquoted logical expression selecting the rows to
  drop, written in the style of base \code{subset} (i.e.
  \code{age>60 | is.na(md)}). Comparisons use \code{==} (not \code{=}). Rows for
  which the condition is TRUE are removed; rows that are FALSE or missing (NA) are
  kept. (Note the polarity is opposite to \code{s_keep}: here the condition marks
  rows to remove, matching Stata's \code{drop if}.) If omitted, all rows are kept
  and only the \code{varlist} is applied.}
  \item{verbose}{if TRUE (the default) messages report how many rows and which
  variables were dropped. Set to FALSE to silence them.}
  \item{overwrite}{if TRUE (the default) the dataframe is modified in place,
  Stata-style, and no assignment is needed. If FALSE the original object is left
  untouched and the reduced copy is returned only through the function's value, so
  you must assign it (i.e. \code{dta2 <- s_drop(...)}) to keep it.}
}
\value{
When \code{overwrite = TRUE} the reduced dataframe is returned invisibly and the
original object is overwritten in place. When \code{overwrite = FALSE} the original
object is left unchanged and the reduced dataframe is returned (visibly); assign it
to keep it.

If the requested variables would remove every column of the dataframe the function
stops with an error rather than returning an empty (0-column) data.frame; use
\code{\link{s_keep}} instead.
}
\note{
At least one of \code{varlist} or \code{condition} must be supplied. When both are
given, the matching rows are removed first and then the listed columns are removed.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_keep}} To keep variables and/or observations instead.

\code{\link{s_order}} To reorder variables (columns) without dropping any.

\code{\link{s_rename}} To rename variables.

\code{\link{codebook}} To explore a dataframe including its variable names and types.
}
\examples{
data(drops)

## 1 Drop some columns
s_drop(drops, c(eye, idn))

## 2 Drop some rows (Stata 'drop if')
data(drops)
s_drop(drops, condition = age > 60)

## 3 Both at once: drop these columns, and drop rows meeting the condition
data(drops)
s_drop(drops, c(eye, idn), condition = age > 60 | is.na(md))

## 4 Non-destructive: preserve the original, create a reduced copy
data(drops)
drops_small <- s_drop(drops, eye, condition = age > 60, overwrite = FALSE)
}
