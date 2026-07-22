s_keep <- function(dataframe, varlist, condition, verbose = TRUE, overwrite = TRUE){

  df.name <- deparse(substitute(dataframe))

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  ## varlist is optional now: a condition-only call (row selection) is allowed.
  has.varlist   <- !missing(varlist)
  has.condition <- !missing(condition)

  if(!has.varlist && !has.condition){
    stop("Specify at least a varlist (columns to keep) or a condition (rows to keep).")
  }

  ## --- Row selection (Stata 'keep if': keep rows where condition is TRUE) ---
  if(has.condition){
    keep.rows <- .s_eval_condition(substitute(condition), dataframe,
                                   parent.frame(), df.name)
    n.before  <- nrow(dataframe)
    dataframe <- dataframe[keep.rows, , drop = FALSE]
    n.after   <- nrow(dataframe)
  }

  ## --- Column selection (keep listed columns, in the order listed) ---------
  if(has.varlist){
    varlist <- .s_capture_vars(substitute(varlist))
    for(v in varlist){
      if(!(v %in% names(dataframe))){
        stop(paste("Variable ", v, " not found in ", df.name, sep=""))
      }
    }
    dropped   <- names(dataframe)[!(names(dataframe) %in% varlist)]
    dataframe <- dataframe[, varlist, drop = FALSE]
  }

  if(overwrite == TRUE){
    assign(df.name, dataframe, envir = parent.frame())
  }

  if(verbose == TRUE){
    if(has.condition){
      message(paste("Kept ", n.after, " of ", n.before,
                    " rows matching the condition in ", df.name, sep=""))
    }
    if(has.varlist){
      message(paste("Kept ", length(varlist), " variable(s): ",
                    paste(varlist, collapse=", "),
                    ". Dropped ", length(dropped), " variable(s)",
                    if(length(dropped) > 0) paste(": ", paste(dropped, collapse=", "), sep="") else "",
                    sep=""))
    }
  }

  if(overwrite == TRUE){
    return(invisible(dataframe))
  } else {
    if(verbose == TRUE){
      message(paste("overwrite = FALSE: ", df.name,
                    " was left unchanged. Assign the result (e.g. dta2 <- s_keep(...)) to keep the reduced copy.",
                    sep=""))
    }
    return(dataframe)
  }
}
