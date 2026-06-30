\name{t23}
\alias{t23}
\title{
Create Tables with Univariable or Multivariables Regression Results
}
\description{
t23 function in this version will allow you to conduct uni and multivariable regression analysis of variables. This function is meant to easily create tables 2 and 3 (hence the function name is t23) of an average paper that reports regression results in the form of a table. In the current version linear regression and logistic regression as well as their gee equivalents are implemented.
}
\usage{
t23(data = NA, y.var = NA, idvar = NA, 
    mod.type = c("univar", "multivar"), 
    mod.func = c("lm", "logistic", "gee", "cox"), 
    model.family = c("gaussian", "binomial"), cor = "exchangeable", 
    time.var = NULL, event.var = NULL, export.table = NA, 
    sep = c(",", ";"), auto.convert = TRUE, selected.vars = NULL, 
    delete.miss.rows = FALSE, coeff.names = NA, coeff.digits = 2, 
    p.digits = 3, trim.digits = FALSE, ...)
}
\arguments{
  \item{data}{
an object of class data.frame containing the variables you wish to describe.
}
  \item{auto.convert}{
if set to TRUE (the default) logical variables will be transformed to factor variables, string variables with less than 10 levels will be transformed to factors (otherwise excluded from the data) and levels of factor variables with 0 counts will be excluded.
}
  \item{selected.vars}{
a character vector specifying which variables in the dataframe you want to describe. Do not include the y.var name in this vector, and if you are running gee, do not include the idvar name in this vector either. (i.e. selected.vars = c("age","sex")) 
}
  \item{delete.miss.rows}{
"FALSE" the default value for this argument. In this case missing values will be excluded on a variable-by-variable basis. 
"TRUE" will lead any row with a missing value to be excluded from the regression, this means that only the subset of observations with no missing values for all of the predictors will be included. This argument is relevant for univariable regression, depending on the pattern of missing values coefficients might differ significantly. To allow for a meaningful comparison of coefficients for a variable in the univariable and multivariable analysis setting this argument to TRUE is preferable. 
}
  \item{mod.type}{
"univar" (the default) will generate a table with univariable regression coefficients. 
"multivar" option in contrast will generate a multivariable regression results table.
}
  \item{mod.func}{
"lm" (the default)will conduct linear regression analysis. "logistic" will run logistic regression using glm function. 
"gee" will lead to generalized estimating equations in cases of correlated data. Only if "gee" is chosen you need to specify "model.family" and "idvar". You can also change the correlation structure specification with the "cor" argument.
}
  \item{model.family}{
is specified only in the event of gee. 
"gaussian" (the default) for linear regression gee and 
"binomial" for logistic regression gee.}
  \item{y.var}{
y variable or dependent variable for the analysis of interest. This variable must be part of the dataframe. 
For "cox" regression y.var is left as NA.
}
  \item{idvar}{
id variable is used only in the event of gee to specify the cluster. (i.e. in an analysis where you include both eyes of some or all patients the cluster is the patient, and you must create a numeric variable that identifies each subject) It must be a numeric variable.
}
  \item{time.var}{
this numeric variable is used only for cox regression. It represents time from initial observation to the occurence of an event. 
In case no event is observed for a subject, then time represents the end of followup.
}
  \item{event.var}{
this numeric variable is used only for cox regression.
It should be coded as:
0 = no event
1 = event
}
  \item{cor}{
the default is "exchangeable", but you may also specify any of the other costr allowed by gee. 
Since gee function calculates robust standard errors correct specification of cor is not as critical as it is for multilevel models or other models where the random effects are of interest. 
}
  \item{coeff.names}{
character vector with the coefficient names as you want them to appear in the table and graph (if you complement this function with the ramu.plot function). Not all names in the table need to be specified. (i.e. The vector can be shorter or can have NA values for those names you don't want to change.)
We recommend running the model with the default value (NA) first, and look at the "uni.table" matrix generated to use as reference for which names to change and in which order to specify them.
}
  \item{coeff.digits}{
number of decimal places for coefficients and confidence intervals. Defaults to 2 decimal places.
}
  \item{p.digits}{
number of decimal places for p values. defaults to 3.
}
  \item{trim.digits}{
the default FALSE and will lead to a table where coefficients are justified. Setting the parameter to TRUE is suitable for centered text, providing more compact tables without trailing blanks.
}
  \item{export.table}{
if you provide a name, (i.e. export.table = "table2") instead of the default value (NA), then a .csv file is exported with the Summary matrix to the current working directory. The date is appended to the file name to avoid inadvertently deleting already created .csv files.}
  \item{sep}{
separator for the csv file, if want to export to a csv file easily read by excel and you live in latin-america and use microsoft office in spanish use sep = ";"
}
  \item{\dots}{
optional arguments}
}
\value{
For univariable models (model.type="univar")
  \item{uni.models}{A list with same number of elements as variables fitted against the dependent variable. Each element is the corresponds to one fitted model. The order of the variables is the same as the order of variables in the dataframe excluding the y.var and idvar}
  \item{uni.coeff}{ A matrix with regression coefficients, confidence intervals and p values. If coeff.names has a non-missing argument (non-NA) then an additional column is added with the original variable names so that you can double-check there are no errors in the specification of variables or their levels}
  \item{uni.table}{ A matrix with regression coefficients summarized, confidence intervals and p values...almost ready for your publication table. If you have specified alternative names for variables or their levels please look at the uni.coeff object to verify that there are no label errors.}

For multivariable models (model.type="multivar")
  \item{multi.models}{A list with the fitted model
}
  \item{multi.coeff}{A matrix with regression coefficients, confidence intervals and p values. If coeff.names has a non-missing argument (non-NA) then an additional column is added with the original variable names so that you can double-check there are no errors in the specification of variables or their levels}
  \item{multi.table}{A matrix with regression coefficients summarized, confidence intervals and p values...almost ready for your publication table. If you have specified alternative names for variables or their levels please look at the multi.coeff object to verify that there are no label errors.}

For uni and multivariable models, this data is only relevant if you want to plot the results by applying the ramu.plot function to the object generated by this function.
  \item{coeff.digits}{same as the function argument coeff.digits
}
  \item{p.digits}{same as the function argument p.digits
}
  \item{trim.digits}{same as the function argument trim.digits
}
  \item{mod.func}{same as the function argument mod.func
}
  \item{model.family}{same as the function argument model.family
}
  \item{model.type}{same as the function argument model.type
}

}
\author{
Eugenio A. Maul MD MPH and Maria Angelica Paulos MD MPH
}
\seealso{
\code{\link{codebook}} To explore a dataframe numerically including missing values.

\code{\link{inspect.plot}} To display graphs with the distribution of all variables on the dataset making it easy to detect outliers and deviations from a nomrmal distribution in cotinuous variables.

\code{\link{t1}} To explore or summarize variables in a dataframe without a grouping factor.

\code{\link{t1.by}} To explore or summarize variables in a dataframe across a grouping variable.

\code{\link{ramu.plot}} To plot the regression result using a forest plot like format using an object created by the t23 function.

\code{\link{cluster.corr}} To calculate Pearson and Spearman Correlation Coefficient in data with Clustering.

}
\examples{
data(drops)

## 1 Simple Example
table.2 <- t23(data = drops, delete.miss.rows=TRUE, mod.type="univar", 
	mod.func="gee", model.family="binomial", y.var="total_success", idvar="idn")

table.3 <- t23(data = drops, delete.miss.rows=TRUE, mod.type="multivar", 
	mod.func="gee", model.family="binomial", y.var="total_success", idvar="idn")

}
