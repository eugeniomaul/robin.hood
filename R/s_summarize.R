s_summarize <- function(dataframe, varlist, detail = 1,
                        cont.digits = 2, horizontal = TRUE,
                        condition, group.var, max.groups = 5,
                        miss.levels = FALSE, separator = 5,
                        statistics = NULL, verbose = TRUE){

  df.name <- deparse(substitute(dataframe))

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  varlist <- .s_capture_vars(substitute(varlist))
  for(v in varlist){
    if(!(v %in% names(dataframe))){
      stop(paste("Variable ", v, " not found in ", df.name, sep=""))
    }
    if(!is.numeric(dataframe[[v]]) && !inherits(dataframe[[v]], "Date")){
      stop(paste("Variable ", v, " is not numeric; s_summarize describes numeric variables only.", sep=""))
    }
  }

  ## --- Resolve which statistics to show -------------------------------------
  ## Priority: explicit 'statistics' overrides 'detail'. If 'statistics' is NULL,
  ## the 'detail' preset level (1, 2 or 3) chooses the set.
  detail.forced.transpose <- FALSE
  if(is.null(statistics)){
    if(!(detail %in% c(1, 2, 3))){
      stop("detail must be 1, 2 or 3 (or specify 'statistics' directly).")
    }
    if(detail == 1){
      stats.req <- c("n","mean","sd","min","max")
    } else if(detail == 2){
      stats.req <- c("n","mean","sd","median","p25","p75","min","max","shapiro.test")
    } else {  # detail == 3: everything, forced to statistics-in-rows layout
      stats.req <- c("n","mean","sd","se","variance","median",
                     "p1","p5","p10","p25","p50","p75","p90","p95","p99",
                     "min","max","skewness","kurtosis","shapiro.test")
      detail.forced.transpose <- TRUE   # hard-force horizontal = FALSE
    }
  } else {
    stats.req <- statistics
  }

  ## detail = 3 forces variables-in-columns / statistics-in-rows (hard).
  if(detail.forced.transpose) horizontal <- FALSE

  stats <- .s_expand_stats(stats.req)

  ## --- optional row condition (base-subset semantics, shared helper) --------
  if(!missing(condition)){
    keep.rows <- .s_eval_condition(substitute(condition), dataframe,
                                   parent.frame(), df.name)
    dataframe <- dataframe[keep.rows, , drop = FALSE]
  }

  ## --- optional grouping variable ------------------------------------------
  if(!missing(group.var)){
    gv.sub  <- substitute(group.var)
    gv.name <- if(is.character(gv.sub)) gv.sub else deparse(gv.sub)
    if(!(gv.name %in% names(dataframe))){
      stop(paste("Grouping variable ", gv.name, " not found in ", df.name, sep=""))
    }
    gv <- dataframe[[gv.name]]
    if(!is.factor(gv)) gv <- as.factor(as.character(gv))
if(nlevels(gv) > max.groups){
      stop(paste("The grouping variable you specified (", gv.name, ") has more than ",
                 max.groups, " levels. Check it is the right variable, and raise max.groups ",
                 "to obtain the result.", sep=""))
    }
    group.levels <- levels(gv)
  } else {
    gv.name <- NULL
    gv <- NULL
    group.levels <- NULL
  }

  ## ---- builder for one block of data (one group level, or all data) --------
  build.block <- function(sub.df, block.label){
    mat <- matrix("", nrow = length(varlist), ncol = length(stats),
                  dimnames = list(varlist, vapply(stats, .s_stat_label, character(1))))
    raw <- matrix(NA_real_, nrow = length(varlist), ncol = length(stats),
                  dimnames = list(varlist, stats))
    for(i in seq_along(varlist)){
      x <- as.numeric(sub.df[[varlist[i]]])
      for(j in seq_along(stats)){
        val <- .s_one_stat(x, stats[j])
        raw[i, j] <- val
        mat[i, j] <- .s_fmt_stat(val, stats[j], cont.digits)
      }
    }
    disp.mat <- if(horizontal) mat else t(mat)
    display <- .s_summarize_format(disp.mat, block.label, separator, horizontal)
    list(display = display, stats.matrix = raw,
         block = block.label, detail = detail)
  }

  ## ---- assemble one or many blocks ----------------------------------------
  tables <- list()
  if(is.null(gv)){
    tables[["All observations"]] <- build.block(dataframe, NULL)
  } else {
    for(lev in group.levels){
      sel <- !is.na(gv) & gv == lev
      tables[[paste(gv.name, "=", lev)]] <-
        build.block(dataframe[sel, , drop = FALSE], paste(gv.name, "=", lev))
    }
    if(miss.levels == TRUE){
      sel <- is.na(gv)
      if(any(sel)){
        tables[[paste(gv.name, "= missing")]] <-
          build.block(dataframe[sel, , drop = FALSE], paste(gv.name, "= missing"))
      }
    }
  }

  out <- list(tables = tables,
              call.info = list(statistics = stats, detail = detail,
                               cont.digits = cont.digits, horizontal = horizontal,
                               group.var = gv.name, dataframe = df.name))
  class(out) <- "s_describe"

  if(verbose){
    for(tb in tables){ cat("\n"); print(tb$display) }
    cat("\n")
  }

  return(invisible(out))
}

## alias: s_su behaves identically
s_su <- s_summarize
