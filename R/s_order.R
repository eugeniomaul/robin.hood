s_order <- function(dataframe, varlist, after, verbose = TRUE, overwrite = TRUE){

  df.name <- deparse(substitute(dataframe))
  varlist  <- .s_capture_vars(substitute(varlist))

  ## 'after' is optional and unquoted; missing() distinguishes "not supplied"
  ## (move to front) from an actual variable name.
  if(missing(after)){
    after.name <- NULL
  } else {
    after.sub  <- substitute(after)
    after.name <- if(is.character(after.sub)) after.sub else deparse(after.sub)
  }

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  ## Validate the variables to move
  for(v in varlist){
    if(!(v %in% names(dataframe))){
      stop(paste("Variable ", v, " not found in ", df.name, sep=""))
    }
  }
  ## Validate 'after' if supplied, and it must not be one of the moved vars
  if(!is.null(after.name)){
    if(!(after.name %in% names(dataframe))){
      stop(paste("Variable ", after.name, " (after) not found in ", df.name, sep=""))
    }
    if(after.name %in% varlist){
      stop(paste("The 'after' variable (", after.name,
                 ") cannot be one of the variables being moved", sep=""))
    }
  }

  all.names  <- names(dataframe)
  remaining  <- all.names[!(all.names %in% varlist)]   # keep original order, minus moved

  if(is.null(after.name)){
    ## No 'after': moved variables go to the front, rest shift right
    new.order <- c(varlist, remaining)
  } else {
    ## Insert the moved block immediately AFTER 'after.name' in the remaining set
    pos       <- which(remaining == after.name)
    new.order <- append(remaining, varlist, after = pos)
  }

  dataframe <- dataframe[, new.order, drop = FALSE]

  if(overwrite == TRUE){
    assign(df.name, dataframe, envir = parent.frame())
  }

  if(verbose == TRUE){
    where <- if(is.null(after.name)) "to the beginning of" else paste("after '", after.name, "' in", sep="")
    message(paste("Variable(s) ", paste(varlist, collapse=", "),
                  " moved ", where, " dataframe ", df.name, sep=""))
  }

  if(overwrite == TRUE){
    return(invisible(dataframe))
  } else {
    if(verbose == TRUE){
      message(paste("overwrite = FALSE: ", df.name,
                    " was left unchanged. Assign the result (e.g. dta2 <- s_order(...)) to keep the reordered copy.",
                    sep=""))
    }
    return(dataframe)
  }
}
