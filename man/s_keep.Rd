\name{s_keep}
\alias{s_keep}
\title{
Keep Selected Variables and/or Observations in a Dataframe (Stata-style)
}
\description{
Emulates Stata's \code{keep} and \code{keep if} commands. With a \code{varlist}
it retains only the listed variables (columns), in the order listed, dropping all
others. With a \code{condition} it retains only the rows for which the condition
is TRUE (Stata's \code{keep if}). Both may be given in one call, in which case the
rows are subset and the columns reduced together. By default the change is applied
in place, overwriting the original object in your workspace.
}
\usage{
s_keep(dataframe, varlist, condition, verbose = TRUE, overwrite = TRUE)
}
\arguments{
  \item{dataframe}{an object of class data.frame, given without quotes.}
  \item{varlist}{optional. The variable(s) to keep. A single variable may be
  given unquoted (i.e. \code{age}); several as a vector, unquoted
  (i.e. \code{c(age, sex)}) or quoted (i.e. \code{c("age","sex")}). Kept variables
  are arranged in the order listed. If omitted, all columns are kept and only the
  \code{condition} is applied.}
  \item{condition}{optional. An unquoted logical expression selecting the rows to
  keep, written in the style of base \code{subset} (i.e.
  \code{sex=="female" & (age>60 | age<40) & !is.na(race)}). Comparisons use
  \code{==} (not \code{=}). Rows for which the condition is TRUE are kept; rows
  that are FALSE or missing (NA) are dropped, as in base \code{subset}. If omitted,
  all rows are kept and only the \code{varlist} is applied.}
  \item{verbose}{if TRUE (the default) messages report how many rows and which
  variables were kept. Set to FALSE to silence them.}
  \item{overwrite}{if TRUE (the default) the dataframe is modified in place,
  Stata-style, and no assignment is needed. If FALSE the original object is left
  untouched and the reduced copy is returned only through the function's value, so
  you must assign it (i.e. \code{dta2 <- s_keep(...)}) to keep it.}
}
\value{
When \code{overwrite = TRUE} the reduced dataframe is returned invisibly and the
original object is overwritten in place. When \code{overwrite = FALSE} the original
object is left unchanged and the reduced dataframe is returned (visibly); assign it
to keep it.
}
\note{
At least one of \code{varlist} or \code{condition} must be supplied. When both are
given, the rows are selected first and then the columns are reduced.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_drop}} To drop variables and/or observations instead.

\code{\link{s_order}} To reorder variables (columns) without dropping any.

\code{\link{s_rename}} To rename variables.

\code{\link{codebook}} To explore a dataframe including its variable names and types.
}
\examples{
data(drops)

## 1 Keep only some columns
s_keep(drops, c(age, sex, md))

## 2 Keep only some rows (Stata 'keep if')
data(drops)
s_keep(drops, condition = age > 60)

## 3 Both at once: keep these columns, for rows meeting the condition
data(drops)
s_keep(drops, c(age, sex, md), condition = age > 60 & !is.na(md))

## 4 Non-destructive: preserve the original, create a reduced copy
data(drops)
drops_small <- s_keep(drops, c(age, sex), condition = age > 60, overwrite = FALSE)
}
