s_rename <- function(dataframe, oldname, newname, overwrite = TRUE, verbose = TRUE){

  ## 1. Capture the object name of the dataframe as typed (for messages
  ##    and for writing the result back into the caller's environment)
  df.name <- deparse(substitute(dataframe))

  ## 2. Capture oldname / newname via the shared family helper, so single
  ##    unquoted names, unquoted vectors c(a, b) and quoted vectors c("a","b")
  ##    are all accepted, consistently with s_keep / s_drop / s_order.
  oldname <- .s_capture_vars(substitute(oldname))
  newname <- .s_capture_vars(substitute(newname))

  ## 3. Lengths must match (relevant for the multi-rename vector case)
  if(length(oldname) != length(newname)){
    stop("oldname and newname must have the same length")
  }

  ## 4. dataframe must actually be a data.frame
  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  ## 5. Validate every oldname exists and no newname collides, BEFORE changing
  ##    anything (so a bad entry in a multi-rename doesn't leave a half-done job).
  ##    A newname is allowed to reuse an oldname being renamed away in the same
  ##    call (permits swaps like c("a","b") -> c("b","a")).
  for(i in seq_along(oldname)){
    if(!(oldname[i] %in% names(dataframe))){
      stop(paste("Variable ", oldname[i], " not found in ", df.name, sep=""))
    }
    if(newname[i] %in% names(dataframe) & !(newname[i] %in% oldname)){
      stop(paste("Variable ", newname[i], " already exists in ", df.name,
                 ". Choose a different name or remove the existing variable first.", sep=""))
    }
  }

  ## 6. Perform the rename(s) on the local copy
  for(i in seq_along(oldname)){
    names(dataframe)[names(dataframe) == oldname[i]] <- newname[i]
  }

  ## 7. In-place mutation ONLY when overwrite = TRUE. This is the Stata-style
  ##    behavior: the original object is overwritten in the caller's workspace.
  ##    When overwrite = FALSE the caller's original object is left untouched and
  ##    the renamed copy is delivered solely through the return value (step 9).
  if(overwrite == TRUE){
    assign(df.name, dataframe, envir = parent.frame())
  }

  ## 8. Status message(s), default verbose = TRUE
  if(verbose == TRUE){
    for(i in seq_along(oldname)){
      message(paste("Variable ", oldname[i], " successfully renamed to ",
                    newname[i], " for dataframe ", df.name, sep=""))
    }
  }

  ## 9. Return behavior depends on overwrite:
  ##    - overwrite = TRUE : return INVISIBLY (in-place already did the work;
  ##      assignment like new.dta <- s_rename(...) still captures it if wanted).
  ##    - overwrite = FALSE: return VISIBLY so that if the user forgot to assign
  ##      (dta2 <- ...), the result prints instead of vanishing silently, plus
  ##      a note that nothing was saved to the workspace.
  if(overwrite == TRUE){
    return(invisible(dataframe))
  } else {
    if(verbose == TRUE){
      message(paste("overwrite = FALSE: ", df.name,
                    " was left unchanged. Assign the result (e.g. dta2 <- s_rename(...)) to keep the renamed copy.",
                    sep=""))
    }
    return(dataframe)   # visible on purpose
  }
}
