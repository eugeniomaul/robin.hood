## Internal formatting helpers for s_tab and s_tab1.

## ---- One-way frequency table -------------------------------------------
.s_tab_format_oneway <- function(counts, var.name, show.pct, miss, pct.digits){

  lvl.names <- names(counts)
  lvl.names[is.na(lvl.names)] <- "Missing"

  n.vec   <- as.integer(counts)
  total.n <- sum(n.vec)

  if(show.pct){
    pct.str <- format(round(100 * n.vec / total.n, digits = pct.digits),
                      nsmall = pct.digits, trim = TRUE)
    cell       <- paste(n.vec, " (", pct.str, "%)", sep = "")
    total.cell <- paste(total.n, " (",
                        format(round(100, digits = pct.digits),
                               nsmall = pct.digits, trim = TRUE), "%)", sep = "")
    col.title <- "n (%)"
  } else {
    cell       <- as.character(n.vec)
    total.cell <- as.character(total.n)
    col.title  <- "n"
  }

  labels <- c(lvl.names, "Total")
  values <- c(cell, total.cell)

  ## Right-align the level labels (fix #1) and the values.
  w.lab <- max(nchar(labels), nchar(var.name))
  w.val <- max(nchar(values), nchar(col.title))

  structure(list(labels = labels, values = values,
                 var.name = var.name, col.title = col.title,
                 w.lab = w.lab, w.val = w.val),
            class = "s_tab_display")
}

## ---- Two-way cross-tabulation ------------------------------------------
## Stata-style layout: counts and percentages on SEPARATE rows (fix #3),
## row variable name above the level labels, column variable name on top (fix #4).
.s_tab_format_twoway <- function(counts, row.pct, col.pct, test,
                                 row.var.name, col.var.name,
                                 show.row, show.col, miss, pct.digits){

  r.names <- rownames(counts); r.names[is.na(r.names)] <- "Missing"
  c.names <- colnames(counts); c.names[is.na(c.names)] <- "Missing"

  n.r <- nrow(counts); n.c <- ncol(counts)

  fmt.pct <- function(v) format(round(v, digits = pct.digits),
                                nsmall = pct.digits, trim = TRUE)

  row.tot <- as.integer(rowSums(counts))
  col.tot <- as.integer(colSums(counts))
  grand   <- sum(counts)

  ## Build the stacked rows: for each level, a count line plus optional
  ## row-% and col-% lines beneath it.
  blocks <- list()
  for(i in 1:n.r){
    lines <- list(c(as.character(counts[i, ]), as.character(row.tot[i])))
    if(show.row && !is.null(row.pct)){
      lines[[length(lines) + 1]] <- c(fmt.pct(row.pct[i, ]), fmt.pct(100))
    }
    if(show.col && !is.null(col.pct)){
      cp <- c(fmt.pct(col.pct[i, ]),
              fmt.pct(100 * row.tot[i] / grand))
      lines[[length(lines) + 1]] <- cp
    }
    blocks[[i]] <- list(label = r.names[i], lines = lines)
  }

  ## Total block
  tot.lines <- list(c(as.character(col.tot), as.character(grand)))
  if(show.row && !is.null(row.pct)){
    tot.lines[[length(tot.lines) + 1]] <- c(fmt.pct(100 * col.tot / grand), fmt.pct(100))
  }
  if(show.col && !is.null(col.pct)){
    tot.lines[[length(tot.lines) + 1]] <- c(rep(fmt.pct(100), n.c), fmt.pct(100))
  }
  blocks[[n.r + 1]] <- list(label = "Total", lines = tot.lines)

  ## Key describing what the stacked numbers mean
  key <- "frequency"
  if(show.row && !is.null(row.pct)) key <- paste(key, "row percentage", sep = " / ")
  if(show.col && !is.null(col.pct)) key <- paste(key, "column percentage", sep = " / ")

  test.lines <- character(0)
  if(!is.null(test)){
    if(!is.null(test$chi2)){
      cl <- test$chi2
      line <- paste("Pearson chi2(", cl$df, ") = ",
                    format(round(cl$statistic, 3), nsmall = 3, trim = TRUE),
                    "   p = ", format(round(cl$p.value, 4), nsmall = 4, trim = TRUE), sep = "")
      if(isTRUE(cl$approx.warning)) line <- paste(line, " (approx.; some expected < 5)", sep = "")
      test.lines <- c(test.lines, line)
    }
    if(!is.null(test$exact)){
      el  <- test$exact
      lab <- if(isTRUE(el$simulated)) "Fisher's exact (simulated) p = " else "Fisher's exact p = "
      test.lines <- c(test.lines, paste(lab, format(round(el$p.value, 4), nsmall = 4, trim = TRUE), sep = ""))
    }
  }

  structure(list(blocks = blocks, col.names = c(c.names, "Total"),
                 row.var = row.var.name, col.var = col.var.name,
                 key = key,
                 tests = if(length(test.lines) > 0) test.lines else NULL),
            class = "s_tab_display")
}

## ---- Print method ------------------------------------------------------

print.s_tab_display <- function(x, ...){

  ## One-way table
  if(!is.null(x$labels)){
    cat("\n", formatC(x$var.name, width = x$w.lab, flag = "-"), "  ",
        formatC(x$col.title, width = x$w.val), "\n", sep = "")
    rule <- paste(rep("-", x$w.lab + x$w.val + 2), collapse = "")
    cat(rule, "\n", sep = "")
    n <- length(x$labels)
    for(i in seq_along(x$labels)){
      ## line above the Total row (the last entry)
      if(i == n) cat(rule, "\n", sep = "")
      cat(formatC(x$labels[i], width = x$w.lab), "  ",
          formatC(x$values[i], width = x$w.val), "\n", sep = "")
    }
    return(invisible(x))
  }

  ## Two-way table
  lab.w <- max(nchar(vapply(x$blocks, function(b) b$label, character(1))),
               nchar(x$row.var))
  n.c   <- length(x$col.names)
  col.w <- integer(n.c)
  for(j in 1:n.c){
    vals <- unlist(lapply(x$blocks, function(b) vapply(b$lines, function(l) l[j], character(1))))
    col.w[j] <- max(nchar(x$col.names[j]), max(nchar(vals)))
  }

  total.w <- lab.w + 2 + sum(col.w) + 2 * (n.c - 1)
  rule <- paste(rep("-", total.w), collapse = "")

  ## TRUE when blocks span more than one line (percentages were requested),
  ## in which case a separator between levels aids readability.
  multiline <- any(vapply(x$blocks, function(b) length(b$lines) > 1, logical(1)))

  cat("\n", "Key: ", x$key, "\n\n", sep = "")

  cat(formatC("", width = lab.w), "   ", x$col.var, "\n", sep = "")
  cat(formatC("", width = lab.w), "  ",
      paste(vapply(1:n.c, function(j) formatC(x$col.names[j], width = col.w[j]),
                   character(1)), collapse = "  "), "\n", sep = "")

  cat(rule, "\n", sep = "")
  cat(formatC(x$row.var, width = lab.w, flag = "-"), "\n", sep = "")

  n.b <- length(x$blocks)
  for(bi in seq_along(x$blocks)){
    b <- x$blocks[[bi]]

    ## line above the Total block (always), and between levels when
    ## the blocks are multi-line because percentages are shown
    if(bi == n.b){
      cat(rule, "\n", sep = "")
    } else if(multiline && bi > 1){
      cat(rule, "\n", sep = "")
    }

    for(k in seq_along(b$lines)){
      lab <- if(k == 1) b$label else ""
      cat(formatC(lab, width = lab.w), "  ",
          paste(vapply(1:n.c, function(j) formatC(b$lines[[k]][j], width = col.w[j]),
                       character(1)), collapse = "  "), "\n", sep = "")
    }
  }

  if(!is.null(x$tests)){
    cat("\n")
    for(tl in x$tests) cat(tl, "\n", sep = "")
  }
  invisible(x)
}
