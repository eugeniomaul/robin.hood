s_tab1 <- function(dataframe, varlist, row = TRUE, miss = FALSE,
                   maxlevels = 20, max.groups = 5, condition, group.var,
                   pct.digits = 2, verbose = TRUE){

  df.name <- deparse(substitute(dataframe))

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  varlist <- .s_capture_vars(substitute(varlist))
  for(v in varlist){
    if(!(v %in% names(dataframe))){
      stop(paste("Variable ", v, " not found in ", df.name, sep=""))
    }
  }

  if(!missing(condition)){
    keep.rows <- .s_eval_condition(substitute(condition), dataframe,
                                   parent.frame(), df.name)
    dataframe <- dataframe[keep.rows, , drop = FALSE]
  }

  to.factor <- function(x){
    if(is.factor(x)) return(as.factor(as.character(x)))
    if(inherits(x, c("Date","POSIXt","POSIXct","POSIXlt"))) return(as.factor(as.character(x)))
    return(as.factor(x))
  }
  level.count <- function(x){
    length(levels(to.factor(x))) + (if(miss == TRUE && any(is.na(x))) 1 else 0)
  }

  if(!missing(group.var)){
    gv.sub  <- substitute(group.var)
    gv.name <- if(is.character(gv.sub)) gv.sub else deparse(gv.sub)
    if(!(gv.name %in% names(dataframe))){
      stop(paste("Grouping variable ", gv.name, " not found in ", df.name, sep=""))
    }
    gv <- to.factor(dataframe[[gv.name]])
    if(nlevels(gv) > max.groups){
      stop(paste("The grouping variable you specified (", gv.name, ") has more than ",
                 max.groups, " levels. Check it is the right variable, and raise max.groups ",
                 "to obtain the result.", sep=""))
    }
    group.levels <- levels(gv)
  } else {
    gv.name <- NULL; gv <- NULL; group.levels <- NULL
  }

  useNA.opt <- if(miss == TRUE) "always" else "no"

  build.block <- function(sub.df, block.label){
    res <- list()
    for(v in varlist){
      if(level.count(sub.df[[v]]) > maxlevels){
        if(verbose){
          message(paste("Skipped ", v, " (more than maxlevels = ", maxlevels,
                        " levels). Increase maxlevels to tabulate it.", sep=""))
        }
        next
      }
      counts  <- table(to.factor(sub.df[[v]]), useNA = useNA.opt)
      display <- .s_tab_format_oneway(counts, v, row, miss, pct.digits)
      key <- if(is.null(block.label)) v else paste(v, block.label, sep=" | ")
      res[[key]] <- list(display = display, counts = counts, row.pct = NULL,
                         col.pct = NULL, test = NULL, row.var = v, col.var = NA)
    }
    res
  }

  tables <- list()
  if(is.null(gv)){
    tables <- build.block(dataframe, NULL)
  } else {
    for(lev in group.levels){
      sel <- !is.na(gv) & gv == lev
      tables <- c(tables, build.block(dataframe[sel, , drop = FALSE],
                                      paste(gv.name, "=", lev)))
    }
  }

  if(length(tables) == 0){
    stop("No variables are within maxlevels; nothing to tabulate. Increase maxlevels.")
  }

  out <- list(tables = tables,
              call.info = list(row = row, col = FALSE, chi2 = FALSE, exact = FALSE,
                               miss = miss, maxlevels = maxlevels, max.groups = max.groups,
                               group.var = gv.name, pct.digits = pct.digits,
                               dataframe = df.name))
  class(out) <- "s_describe"

  if(verbose){
    for(nm in names(tables)){ cat("\n"); print(tables[[nm]]$display) }
    cat("\n")
  }

  return(invisible(out))
}