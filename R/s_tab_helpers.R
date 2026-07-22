## Internal formatting helpers for s_tab and s_tab1.
## All Stata-look formatting (spacing, percentages, test lines) lives here.

## ---- One-way frequency table -------------------------------------------
## counts    : a 1-D table() object
## var.name  : name of the variable (for the title)
## show.pct  : logical, whether to show percentages
## miss      : logical, whether NAs were tabulated (affects the "Missing" label)
## pct.digits: decimal places for percentages
.s_tab_format_oneway <- function(counts, var.name, show.pct, miss, pct.digits){

  lvl.names <- names(counts)
  lvl.names[is.na(lvl.names)] <- "Missing"

  n.vec     <- as.integer(counts)
  total.n   <- sum(n.vec)

  if(show.pct){
    pct.vec <- 100 * n.vec / total.n
    pct.str <- format(round(pct.vec, digits = pct.digits),
                      nsmall = pct.digits, trim = TRUE)
    cell    <- paste(n.vec, " (", pct.str, "%)", sep="")
    total.cell <- paste(total.n, " (",
                        format(round(100, digits = pct.digits),
                               nsmall = pct.digits, trim = TRUE),
                        "%)", sep="")
    col.title <- "n (%)"
  } else {
    cell       <- as.character(n.vec)
    total.cell <- as.character(total.n)
    col.title  <- "n"
  }

  body <- matrix(c(lvl.names, cell), ncol = 2)
  body <- rbind(body, c("Total", total.cell))
  colnames(body) <- c(var.name, col.title)
  rownames(body) <- rep("", nrow(body))

  noquote(format(body, justify = "centre"))
}

## ---- Two-way cross-tabulation ------------------------------------------
## counts     : a 2-D table() (rows = row.var, cols = col.var)
## row.pct    : numeric matrix of row %  (or NULL)
## col.pct    : numeric matrix of col %  (or NULL)
## test       : list possibly containing $chi2 and/or $exact (or NULL)
## row.var.name, col.var.name : variable names
## show.row, show.col : logicals
## miss       : logical (NA tabulated?) -> relabel NA margins as "Missing"
## pct.digits : decimal places for percentages
.s_tab_format_twoway <- function(counts, row.pct, col.pct, test,
                                 row.var.name, col.var.name,
                                 show.row, show.col, miss, pct.digits){

  r.names <- rownames(counts); r.names[is.na(r.names)] <- "Missing"
  c.names <- colnames(counts); c.names[is.na(c.names)] <- "Missing"

  n.r <- nrow(counts)
  n.c <- ncol(counts)

  fmt.pct <- function(x){
    paste(format(round(x, digits = pct.digits), nsmall = pct.digits, trim = TRUE),
          "%", sep="")
  }

  ## Assemble the cell string: count, optionally with row% and/or col% beneath,
  ## joined with a slash so a single console line stays readable.
  make.cell <- function(i, j){
    parts <- as.character(counts[i, j])
    extras <- character(0)
    if(show.row && !is.null(row.pct)) extras <- c(extras, paste("r:", fmt.pct(row.pct[i, j]), sep=""))
    if(show.col && !is.null(col.pct)) extras <- c(extras, paste("c:", fmt.pct(col.pct[i, j]), sep=""))
    if(length(extras) > 0) parts <- paste(parts, " (", paste(extras, collapse=", "), ")", sep="")
    parts
  }

  ## Body cells
  body <- matrix("", nrow = n.r, ncol = n.c)
  for(i in 1:n.r) for(j in 1:n.c) body[i, j] <- make.cell(i, j)

  ## Row totals and column totals (counts only, to keep the table legible)
  row.tot <- as.integer(rowSums(counts))
  col.tot <- as.integer(colSums(counts))
  grand   <- sum(counts)

  body <- cbind(body, as.character(row.tot))
  body <- rbind(body, c(as.character(col.tot), as.character(grand)))

  colnames(body) <- c(c.names, "Total")
  rownames(body) <- c(r.names, "Total")

  ## Title rows: which variable is rows, which is cols
  header.line <- paste(row.var.name, " (rows)  x  ", col.var.name, " (cols)", sep="")

  disp <- noquote(format(body, justify = "centre"))

  ## Attach test line(s) as an attribute-free footer by returning a list-like
  ## character block. We print header, table, then tests via a wrapper matrix.
  test.lines <- character(0)
  if(!is.null(test)){
    if(!is.null(test$chi2)){
      cl <- test$chi2
      line <- paste("Pearson chi2(", cl$df, ") = ",
                    format(round(cl$statistic, 3), nsmall = 3, trim = TRUE),
                    "   p = ", format(round(cl$p.value, 4), nsmall = 4, trim = TRUE),
                    sep="")
      if(isTRUE(cl$approx.warning)) line <- paste(line, " (approx.; some expected < 5)", sep="")
      test.lines <- c(test.lines, line)
    }
    if(!is.null(test$exact)){
      el <- test$exact
      lab <- if(isTRUE(el$simulated)) "Fisher's exact (simulated) p = " else "Fisher's exact p = "
      test.lines <- c(test.lines,
                      paste(lab, format(round(el$p.value, 4), nsmall = 4, trim = TRUE), sep=""))
    }
  }

  ## Return a small structure the caller prints: header + table + tests.
  ## We store them together in an attribute so $display prints cleanly and
  ## the raw table remains available separately in the parent object.
  structure(disp,
            s_header = header.line,
            s_tests  = if(length(test.lines) > 0) test.lines else NULL,
            class    = c("s_tab_display", "noquote"))
}

## Print method so the header and test lines appear around the table.
print.s_tab_display <- function(x, ...){
  hdr   <- attr(x, "s_header")
  tests <- attr(x, "s_tests")
  if(!is.null(hdr)) cat(hdr, "\n")
  ## strip our extra class so the underlying noquote matrix prints normally
  y <- x
  attr(y, "s_header") <- NULL
  attr(y, "s_tests")  <- NULL
  class(y) <- "noquote"
  print(y)
  if(!is.null(tests)){
    cat("\n")
    for(tl in tests) cat(tl, "\n")
  }
  invisible(x)
}
