## Format the tabstat/su-style matrix into a noquote display with an optional
## title and separator lines every 'separator' rows.
.s_summarize_format <- function(disp.mat, block.label, separator, horizontal){
  body <- cbind(rownames(disp.mat), unclass(disp.mat))
  colnames(body)[1] <- if(horizontal) "Variable" else "Statistic"
  rownames(body) <- NULL
  out <- noquote(body)
  attr(out, "s_block") <- block.label
  attr(out, "s_sep")   <- separator
  class(out) <- c("s_summarize_display", "noquote")
  out
}

print.s_summarize_display <- function(x, ...){
  blk <- attr(x, "s_block")
  sep <- attr(x, "s_sep")
  mat <- unclass(x)
  attr(mat, "s_block") <- NULL
  attr(mat, "s_sep")   <- NULL

  header <- colnames(mat)

  ## ONE set of widths derived from header AND body, so they always line up.
  ## Column 1 (variable/statistic names) is left-justified; the rest right.
  n.col  <- ncol(mat)
  widths <- integer(n.col)
  for(j in 1:n.col){
    widths[j] <- max(nchar(header[j]), max(nchar(mat[, j])), na.rm = TRUE)
  }

  pad <- function(s, w, left){
    if(left) formatC(s, width = w, flag = "-") else formatC(s, width = w)
  }

  hdr.line <- paste(vapply(1:n.col, function(j) pad(header[j], widths[j], j == 1),
                           character(1)), collapse = "  ")
  rule <- paste(rep("-", nchar(hdr.line)), collapse = "")

  if(!is.null(blk)) cat("-> ", blk, "\n", sep = "")
  cat(hdr.line, "\n", sep = "")
  cat(rule, "\n", sep = "")
  for(i in seq_len(nrow(mat))){
    cat(paste(vapply(1:n.col, function(j) pad(mat[i, j], widths[j], j == 1),
                     character(1)), collapse = "  "), "\n", sep = "")
    if(!is.null(sep) && sep > 0 && i %% sep == 0 && i < nrow(mat)) cat(rule, "\n", sep = "")
  }
  invisible(x)
}