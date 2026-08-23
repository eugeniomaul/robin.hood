## Internal helpers for s_regress and s_logistic.

## Build a formula string from an outcome, a character vector of main-effect
## predictors, and an optional character vector of interaction terms (R style,
## e.g. "age:sex" or "age*sex"). Validates that every variable named exists.
.s_build_formula <- function(outcome, predictors, interactions, dataframe, df.name){

  ## Validate outcome
  if(!(outcome %in% names(dataframe))){
    stop(paste("Outcome variable ", outcome, " not found in ", df.name, sep=""))
  }
  ## Validate main-effect predictors
  for(v in predictors){
    if(!(v %in% names(dataframe))){
      stop(paste("Variable ", v, " not found in ", df.name, sep=""))
    }
  }
  ## Validate variables named inside interaction terms
  if(!is.null(interactions)){
    for(term in interactions){
      parts <- unlist(strsplit(term, "[:*]"))
      parts <- trimws(parts)
      for(p in parts){
        if(!(p %in% names(dataframe))){
          stop(paste("Variable ", p, " (in interaction '", term,
                     "') not found in ", df.name, sep=""))
        }
      }
    }
  }

  rhs <- c(predictors, interactions)
  if(length(rhs) == 0){
    stop("Specify at least one predictor variable.")
  }
  as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
}

## Format a number to a fixed number of significant-ish decimals, Stata-like,
## trimmed of leading/trailing space.
.s_num <- function(x, d) format(round(x, d), nsmall = d, trim = TRUE)

## Assemble the coefficient table (character matrix) shared by both functions.
## est.cols is a matrix with columns already in display order; col.labels are
## the Stata-style headers; row.labels are the term names.
.s_coef_matrix <- function(body, col.labels, row.labels){
  m <- rbind(col.labels, body)
  m <- cbind(c("", row.labels), m)
  noquote(m)
}