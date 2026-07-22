\name{s_rename}
\alias{s_rename}
\title{
Rename Variables in a Dataframe (Stata-style)
}
\description{
Emulates Stata's \code{rename} command. Renames one or more variables in a
dataframe. By default the rename is applied in place, overwriting the original
object in your workspace so that no reassignment is needed (mirroring Stata,
where \code{rename} acts on the dataset in memory). Variable and dataframe names
may be given without quotes when they contain no special characters.
}
\usage{
s_rename(dataframe, oldname, newname, overwrite = TRUE, verbose = TRUE)
}
\arguments{
  \item{dataframe}{
an object of class data.frame, given without quotes. When \code{overwrite = TRUE}
(the default) the renamed dataframe is written back to this same object in your
workspace.
}
  \item{oldname}{
the current variable name(s). A single name may be given unquoted (i.e.
\code{age}). To rename several variables at once, supply a vector, either unquoted
(i.e. \code{c(age, sex)}) or quoted (i.e. \code{c("age","sex")}).
}
  \item{newname}{
the new variable name(s), matching \code{oldname} in length and order. Given
unquoted for a single rename, or as an unquoted or quoted vector for several. Use
quotes if a new name contains spaces or other special characters.
}
  \item{overwrite}{
if TRUE (the default) the dataframe is modified in place: the original object in
your workspace is overwritten with the renamed version, Stata-style, and no
assignment is needed. If FALSE the original object is left untouched and the
renamed copy is returned only through the function's value, so you must assign it
(i.e. \code{dta2 <- s_rename(...)}) to keep it.
}
  \item{verbose}{
if TRUE (the default) a confirmation message is printed for each variable
renamed. When \code{overwrite = FALSE} an additional note reminds you to assign
the result. Set to FALSE to silence these messages.
}
}
\value{
When \code{overwrite = TRUE} the function returns the renamed dataframe invisibly;
the main effect is the in-place overwrite of the original object. The invisible
return still allows the result to be captured by assignment
(i.e. \code{new.dta <- s_rename(...)}) if desired, though the original object is
overwritten in that case as well.

When \code{overwrite = FALSE} the original object is left unchanged and the
renamed dataframe is returned (visibly). Assign it to keep it
(i.e. \code{dta2 <- s_rename(...)}); if it is not assigned it simply prints.
}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{s_keep}} To keep only a chosen set of variables.

\code{\link{s_drop}} To drop a chosen set of variables.

\code{\link{s_order}} To reorder variables (columns) in a dataframe.

\code{\link{codebook}} To explore a dataframe including its variable names and types.
}
\examples{
data(drops)

## 1 Single rename, unquoted (in place: 'drops' itself is changed)
s_rename(drops, age, patient_age)

## 2 Single rename, quoted new name containing a space
data(drops)
s_rename(drops, va_log, "visual acuity")

## 3 Rename several variables at once (unquoted or quoted vectors both work)
data(drops)
s_rename(drops, c(age, sex), c(patient_age, gender))

## 4 Silent rename (no messages)
data(drops)
s_rename(drops, age, patient_age, verbose = FALSE)

## 5 Non-destructive rename: keep the original, create a renamed copy
data(drops)
drops_renamed <- s_rename(drops, age, patient_age, overwrite = FALSE)
## 'drops' still has 'age'; 'drops_renamed' has 'patient_age'
}