s_tab1 <- function(dataframe, varlist, row = TRUE, miss = FALSE,
                   maxlevels = 20, pct.digits = 2, verbose = TRUE){

  df.name <- deparse(substitute(dataframe))
  varlist <- .s_capture_vars(substitute(varlist))

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }
  for(v in varlist){
    if(!(v %in% names(dataframe))){
      stop(paste("Variable ", v, " not found in ", df.name, sep=""))
    }
  }

  useNA.opt <- if(miss == TRUE) "always" else "no"

  to.factor <- function(x){
    if(is.factor(x)) return(as.factor(as.character(x)))
    if(inherits(x, c("Date","POSIXt","POSIXct","POSIXlt"))) return(as.factor(as.character(x)))
    return(as.factor(x))
  }
  level.count <- function(x){
    length(levels(to.factor(x))) + (if(miss == TRUE && any(is.na(x))) 1 else 0)
  }

  results <- list()
  for(v in varlist){
    if(level.count(dataframe[[v]]) > maxlevels){
      if(verbose){
        message(paste("Skipped ", v, " (more than maxlevels = ", maxlevels,
                      " levels). Increase maxlevels to tabulate it.", sep=""))
      }
      next
    }
    counts  <- table(to.factor(dataframe[[v]]), useNA = useNA.opt)
    display <- .s_tab_format_oneway(counts, v, row, miss, pct.digits)
    results[[v]] <- list(display = display, counts = counts, row.pct = NULL,
                         col.pct = NULL, test = NULL, row.var = v, col.var = NA)
  }

  if(length(results) == 0){
    stop("No variables are within maxlevels; nothing to tabulate. Increase maxlevels.")
  }

  out <- list(tables = results,
              call.info = list(row = row, col = FALSE, chi2 = FALSE, exact = FALSE,
                               miss = miss, maxlevels = maxlevels,
                               pct.digits = pct.digits, dataframe = df.name))
  class(out) <- "s_describe"

  if(verbose){
    for(tb in results){ cat("\n"); print(tb$display) }
    cat("\n")
  }

  return(invisible(out))
}