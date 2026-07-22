\name{s_order}
\alias{s_order}
\title{
Reorder Variables (Columns) in a Dataframe (Stata-style)
}
\description{
Emulates Stata's \code{order} command. Moves one or more variables to a new
position among the columns of a dataframe. By default the reordering is applied
in place, overwriting the original object in your workspace. Variable and
dataframe names may be given without quotes when they contain no special
characters.
}
\usage{
s_order(dataframe, varlist, after, verbose = TRUE, overwrite = TRUE)
}
\arguments{
  \item{dataframe}{
an object of class data.frame, given without quotes. When \code{overwrite = TRUE}
(the default) the reordered dataframe is written back to this same object in your
workspace.
}
  \item{varlist}{
the variable(s) to move. A single variable may be given unquoted (i.e.
\code{age}); several variables are given as a vector, either unquoted
(i.e. \code{c(age, sex)}) or quoted (i.e. \code{c("age","sex")}).
}
  \item{after}{
optional. The variable after which the moved variable(s) should be placed, given
unquoted (i.e. \code{after = sex}) or quoted. If omitted, the moved variable(s)
are placed at the beginning of the dataframe and the remaining columns are shifted
to the right. The \code{after} variable cannot be one of the variables being moved.
}
  \item{verbose}{
if TRUE (the default) a message reports which variables were moved and where. Set
to FALSE to silence it.
}
  \item{overwrite}{
if TRUE (the default) the dataframe is modified in place, Stata-style, and no
assignment is needed. If FALSE the original object is left untouched and the
reordered copy is returned only through the function's value, so you must assign
it (i.e. \code{dta2 <- s_order(...)}) to keep it.
}
}
\value{
When \code{overwrite = TRUE} the reordered dataframe is returned invisibly and the
original object is overwritten in place. When \code{overwrite = FALSE} the original
object is left unchanged and the reordered dataframe is returned (visibly); assign
it to keep it.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_keep}} To keep only a chosen set of variables.

\code{\link{s_drop}} To drop a chosen set of variables.

\code{\link{s_rename}} To rename variables.

\code{\link{codebook}} To explore a dataframe including its variable names and types.
}
\examples{
data(drops)

## 1 Move a single variable to the front
s_order(drops, md)

## 2 Move several variables to the front (rest shift right)
data(drops)
s_order(drops, c(sex, age))

## 3 Move a variable to sit immediately after another
data(drops)
s_order(drops, md, after = age)

## 4 Non-destructive reorder: keep the original, create a reordered copy
data(drops)
drops_ordered <- s_order(drops, md, after = age, overwrite = FALSE)
}