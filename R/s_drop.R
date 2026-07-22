s_drop <- function(dataframe, varlist, condition, verbose = TRUE, overwrite = TRUE){

  df.name <- deparse(substitute(dataframe))

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  has.varlist   <- !missing(varlist)
  has.condition <- !missing(condition)

  if(!has.varlist && !has.condition){
    stop("Specify at least a varlist (columns to drop) or a condition (rows to drop).")
  }

  ## --- Row selection (Stata 'drop if': remove rows where condition is TRUE) -
  if(has.condition){
    drop.rows <- .s_eval_condition(substitute(condition), dataframe,
                                   parent.frame(), df.name)
    n.before  <- nrow(dataframe)
    dataframe <- dataframe[!drop.rows, , drop = FALSE]   # keep the rest
    n.after   <- nrow(dataframe)
  }

  ## --- Column selection (drop listed columns, keep the rest in place) -------
  if(has.varlist){
    varlist <- .s_capture_vars(substitute(varlist))
    for(v in varlist){
      if(!(v %in% names(dataframe))){
        stop(paste("Variable ", v, " not found in ", df.name, sep=""))
      }
    }
    kept <- names(dataframe)[!(names(dataframe) %in% varlist)]
    if(length(kept) == 0){
      stop(paste("Dropping these variables would remove every column of ", df.name,
                 ". Use s_keep, or keep at least one variable.", sep=""))
    }
    dataframe <- dataframe[, kept, drop = FALSE]
  }

  if(overwrite == TRUE){
    assign(df.name, dataframe, envir = parent.frame())
  }

  if(verbose == TRUE){
    if(has.condition){
      message(paste("Dropped ", n.before - n.after, " of ", n.before,
                    " rows matching the condition from ", df.name,
                    " (", n.after, " rows remain)", sep=""))
    }
    if(has.varlist){
      message(paste("Dropped ", length(varlist), " variable(s): ",
                    paste(varlist, collapse=", "), " from ", df.name, sep=""))
    }
  }

  if(overwrite == TRUE){
    return(invisible(dataframe))
  } else {
    if(verbose == TRUE){
      message(paste("overwrite = FALSE: ", df.name,
                    " was left unchanged. Assign the result (e.g. dta2 <- s_drop(...)) to keep the reduced copy.",
                    sep=""))
    }
    return(dataframe)
  }
}
