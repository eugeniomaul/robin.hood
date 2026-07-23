s_tab <- function(dataframe, varlist, row = TRUE, col = FALSE, chi2 = FALSE,
                  exact = FALSE, miss = FALSE, maxlevels = 10, max.groups = 5,
                  condition, group.var, pct.digits = 2, verbose = TRUE){

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

  build.one <- function(sub.df, row.var.name, col.var.name){
    rv <- to.factor(sub.df[[row.var.name]])

    if(is.null(col.var.name)){
      counts  <- table(rv, useNA = useNA.opt)
      display <- .s_tab_format_oneway(counts, row.var.name, row, miss, pct.digits)
      return(list(display = display, counts = counts, row.pct = NULL,
                  col.pct = NULL, test = NULL,
                  row.var = row.var.name, col.var = NA))
    }

    cv     <- to.factor(sub.df[[col.var.name]])
    counts <- table(rv, cv, useNA = useNA.opt)

    row.pct <- if(row) 100 * prop.table(counts, 1) else NULL
    col.pct <- if(col) 100 * prop.table(counts, 2) else NULL

    test <- NULL
    if((chi2 || exact) && nrow(counts) >= 2 && ncol(counts) >= 2){
      test <- list()
      if(chi2){
        cs <- tryCatch(suppressWarnings(chisq.test(counts)), error = function(e) NULL)
        if(!is.null(cs)){
          test$chi2 <- list(statistic = unname(cs$statistic), df = unname(cs$parameter),
                            p.value = cs$p.value, approx.warning = any(cs$expected < 5))
        }
      }
      if(exact){
        small.enough <- (prod(dim(counts)) <= 200 && sum(counts) <= 5000)
        ft <- NULL
        if(small.enough) ft <- tryCatch(fisher.test(counts), error = function(e) NULL)
        if(is.null(ft)){
          ft <- tryCatch(fisher.test(counts, simulate.p.value = TRUE, B = 2000),
                         error = function(e) NULL)
          if(!is.null(ft)) ft$simulated <- TRUE
        } else ft$simulated <- FALSE
        if(!is.null(ft)) test$exact <- list(p.value = ft$p.value, simulated = isTRUE(ft$simulated))
      }
    }

    display <- .s_tab_format_twoway(counts, row.pct, col.pct, test,
                                    row.var.name, col.var.name, row, col, miss, pct.digits)
    list(display = display, counts = counts, row.pct = row.pct, col.pct = col.pct,
         test = test, row.var = row.var.name, col.var = col.var.name)
  }

  build.block <- function(sub.df, block.label){
    over.max <- character(0); ok.vars <- character(0)
    for(v in varlist){
      if(level.count(sub.df[[v]]) > maxlevels) over.max <- c(over.max, v) else ok.vars <- c(ok.vars, v)
    }
    if(length(over.max) > 0 && verbose){
      message(paste("Skipped (more than maxlevels = ", maxlevels, " levels): ",
                    paste(over.max, collapse=", "),
                    ". Increase maxlevels to tabulate these.", sep=""))
    }
    res <- list()
    if(length(ok.vars) == 0){
      stop("No variables are within maxlevels; nothing to tabulate. Increase maxlevels.")
    } else if(length(ok.vars) == 1){
      key <- if(is.null(block.label)) ok.vars[1] else paste(ok.vars[1], block.label, sep=" | ")
      res[[key]] <- build.one(sub.df, ok.vars[1], NULL)
    } else {
      first <- ok.vars[1]
      for(v in ok.vars[-1]){
        key <- paste(first, "x", v)
        if(!is.null(block.label)) key <- paste(key, block.label, sep=" | ")
        res[[key]] <- build.one(sub.df, first, v)
      }
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

  out <- list(tables = tables,
              call.info = list(row = row, col = col, chi2 = chi2, exact = exact,
                               miss = miss, maxlevels = maxlevels, max.groups = max.groups,
                               group.var = gv.name, pct.digits = pct.digits,
                               dataframe = df.name))
  class(out) <- "s_describe"

  if(verbose){
    for(nm in names(tables)){
      cat("\n"); print(tables[[nm]]$display)
    }
    cat("\n")
  }

  return(invisible(out))
}