s_tab <- function(dataframe, varlist, row = TRUE, col = FALSE, chi2 = FALSE,
                  exact = FALSE, miss = FALSE, maxlevels = 10,
                  pct.digits = 2, verbose = TRUE){

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

  build.one <- function(row.var.name, col.var.name){
    rv <- to.factor(dataframe[[row.var.name]])

    if(is.null(col.var.name)){
      counts  <- table(rv, useNA = useNA.opt)
      display <- .s_tab_format_oneway(counts, row.var.name, row, miss, pct.digits)
      return(list(display = display, counts = counts, row.pct = NULL,
                  col.pct = NULL, test = NULL,
                  row.var = row.var.name, col.var = NA))
    }

    cv     <- to.factor(dataframe[[col.var.name]])
    counts <- table(rv, cv, useNA = useNA.opt)

    row.pct <- if(row) 100 * prop.table(counts, 1) else NULL
    col.pct <- if(col) 100 * prop.table(counts, 2) else NULL

    test <- NULL
    valid.for.test <- (nrow(counts) >= 2 && ncol(counts) >= 2)
    if((chi2 || exact) && valid.for.test){
      test <- list()
      if(chi2){
        cs <- tryCatch(suppressWarnings(chisq.test(counts)), error = function(e) NULL)
        if(!is.null(cs)){
          test$chi2 <- list(statistic = unname(cs$statistic),
                            df = unname(cs$parameter),
                            p.value = cs$p.value,
                            approx.warning = any(cs$expected < 5))
        }
      }
      if(exact){
        cells <- prod(dim(counts))
        small.enough <- (cells <= 200 && sum(counts) <= 5000)
        ft <- NULL
        if(small.enough){
          ft <- tryCatch(fisher.test(counts), error = function(e) NULL)
        }
        if(is.null(ft)){
          ft <- tryCatch(fisher.test(counts, simulate.p.value = TRUE, B = 2000),
                         error = function(e) NULL)
          if(!is.null(ft)) ft$simulated <- TRUE
        } else {
          ft$simulated <- FALSE
        }
        if(!is.null(ft)){
          test$exact <- list(p.value = ft$p.value, simulated = isTRUE(ft$simulated))
        }
      }
    }

    display <- .s_tab_format_twoway(counts, row.pct, col.pct, test,
                                    row.var.name, col.var.name, row, col, miss, pct.digits)
    list(display = display, counts = counts, row.pct = row.pct,
         col.pct = col.pct, test = test,
         row.var = row.var.name, col.var = col.var.name)
  }

  over.max <- character(0)
  ok.vars  <- character(0)
  for(v in varlist){
    if(level.count(dataframe[[v]]) > maxlevels){
      over.max <- c(over.max, v)
    } else {
      ok.vars <- c(ok.vars, v)
    }
  }
  if(length(over.max) > 0 && verbose){
    message(paste("Skipped (more than maxlevels = ", maxlevels, " levels): ",
                  paste(over.max, collapse=", "),
                  ". Increase maxlevels to tabulate these.", sep=""))
  }

  results <- list()

  if(length(ok.vars) == 0){
    stop("No variables are within maxlevels; nothing to tabulate. Increase maxlevels.")
  } else if(length(ok.vars) == 1){
    results[[ok.vars[1]]] <- build.one(ok.vars[1], NULL)
  } else {
    first <- ok.vars[1]
    if(first %in% over.max){
      stop(paste("The first variable (", first,
                 ") exceeds maxlevels; cannot use it as the row variable.", sep=""))
    }
    rest <- ok.vars[-1]
    for(v in rest){
      key <- paste(first, "x", v, sep=" ")
      results[[key]] <- build.one(first, v)
    }
  }

  out <- list(tables = results,
              call.info = list(row = row, col = col, chi2 = chi2, exact = exact,
                               miss = miss, maxlevels = maxlevels,
                               pct.digits = pct.digits, dataframe = df.name))
  class(out) <- "s_describe"

  if(verbose){
    for(tb in results){
      cat("\n")
      print(tb$display)
    }
    cat("\n")
  }

  return(invisible(out))
}