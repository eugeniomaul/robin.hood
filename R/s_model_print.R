## Print method for s_regress / s_logistic display objects.
print.s_model_display <- function(x, ...){
  h <- x$header
  n2 <- function(v, d) format(round(v, d), nsmall = d, trim = TRUE)

  if(x$type == "regress"){
    ## Source block (left) beside summary stats (right)
    cat("      Source |       SS       df       MS",
        "            Number of obs = ", format(h$n, width = 6), "\n", sep="")
    cat("-------------+------------------------------",
        "           F(", h$df.mod, ",", format(h$df.res, width = 4), ") = ",
        format(n2(h$F, 2), width = 8), "\n", sep="")
    cat("       Model | ", format(n2(h$ss.mod, 4), width = 11), format(h$df.mod, width = 3),
        format(n2(h$ms.mod, 4), width = 12),
        "           Prob > F      = ", n2(h$Fp, 4), "\n", sep="")
    cat("    Residual | ", format(n2(h$ss.res, 4), width = 11), format(h$df.res, width = 3),
        format(n2(h$ms.res, 5), width = 12),
        "           R-squared     = ", n2(h$r2, 4), "\n", sep="")
    cat("-------------+------------------------------",
        "           Adj R-squared = ", n2(h$adj.r2, 4), "\n", sep="")
    cat("       Total | ", format(n2(h$ss.tot, 4), width = 11), format(h$df.tot, width = 3),
        format(n2(h$ms.tot, 5), width = 12),
        "           Root MSE      = ", n2(h$root.mse, 3), "\n", sep="")
    cat("\n")
  } else {
    cat("Logistic regression",
        strrep(" ", 27), "Number of obs   = ", format(h$n, width = 10), "\n", sep="")
    cat(strrep(" ", 50), "LR chi2(", h$df.lr, ")      = ",
        format(n2(h$LR, 2), width = 10), "\n", sep="")
    cat(strrep(" ", 50), "Prob > chi2     = ",
        format(n2(h$LRp, 4), width = 10), "\n", sep="")
    cat("Log likelihood = ", n2(h$loglik, 6),
        strrep(" ", 8), "Pseudo R2       = ", format(n2(h$pseudo, 4), width = 10), "\n", sep="")
    cat("\n")
  }

  ## Coefficient table, columns padded to a common width
  m <- unclass(x$coef)
  nc <- ncol(m)
  w  <- integer(nc)
  for(j in 1:nc) w[j] <- max(nchar(m[, j]))
  rule.left <- paste(rep("-", w[1] + 1), collapse = "")
  rule.right <- paste(rep("-", sum(w[-1]) + 2 * (nc - 1) + 1), collapse = "")

  for(i in 1:nrow(m)){
    cells <- vapply(1:nc, function(j){
      if(j == 1) formatC(m[i, j], width = w[j], flag = "-")
      else formatC(m[i, j], width = w[j])
    }, character(1))
    ## first data column separated from the rest by a vertical bar, Stata-style
    line <- paste0(cells[1], " | ", paste(cells[-1], collapse = "  "))
    cat(line, "\n", sep="")
    if(i == 1) cat(rule.left, "+", rule.right, "\n", sep="")
  }
  invisible(x)
}

## Convert an s_model_display into a plain matrix block for s_export.
## (s_export calls this via the generic path; see below.)
.s_model_export_block <- function(disp){
  h <- disp$header
  n2 <- function(v, d) format(round(v, d), nsmall = d, trim = TRUE)
  m  <- unclass(disp$coef)

  if(disp$type == "regress"){
    hdr <- rbind(
      c(paste("Linear regression: outcome =", h$outcome), rep("", ncol(m) - 1)),
      c(paste("Number of obs =", h$n), rep("", ncol(m) - 1)),
      c(paste("F(", h$df.mod, ",", h$df.res, ") =", n2(h$F, 2)), rep("", ncol(m) - 1)),
      c(paste("Prob > F =", n2(h$Fp, 4)), rep("", ncol(m) - 1)),
      c(paste("R-squared =", n2(h$r2, 4)), rep("", ncol(m) - 1)),
      c(paste("Adj R-squared =", n2(h$adj.r2, 4)), rep("", ncol(m) - 1)),
      c(paste("Root MSE =", n2(h$root.mse, 3)), rep("", ncol(m) - 1))
    )
  } else {
    hdr <- rbind(
      c(paste("Logistic regression: outcome =", h$outcome), rep("", ncol(m) - 1)),
      c(paste("Number of obs =", h$n), rep("", ncol(m) - 1)),
      c(paste("LR chi2(", h$df.lr, ") =", n2(h$LR, 2)), rep("", ncol(m) - 1)),
      c(paste("Prob > chi2 =", n2(h$LRp, 4)), rep("", ncol(m) - 1)),
      c(paste("Log likelihood =", n2(h$loglik, 6)), rep("", ncol(m) - 1)),
      c(paste("Pseudo R2 =", n2(h$pseudo, 4)), rep("", ncol(m) - 1))
    )
  }
  rbind(hdr, m)
}