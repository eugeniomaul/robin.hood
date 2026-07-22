## Format the tabstat/su-style matrix into a noquote display with an optional
## title and separator lines every 'separator' rows.
.s_summarize_format <- function(disp.mat, block.label, separator, horizontal){
  m <- format(disp.mat, justify = "right")
  body <- cbind(rownames(m), m)
  colnames(body)[1] <- if(horizontal) "Variable" else "Statistic"
  rownames(body) <- NULL
  out <- noquote(format(body, justify = "right"))
  attr(out, "s_block") <- block.label
  attr(out, "s_sep")   <- separator
  class(out) <- c("s_summarize_display", "noquote")
  out
}

print.s_summarize_display <- function(x, ...){
  blk <- attr(x, "s_block")
  sep <- attr(x, "s_sep")
  if(!is.null(blk)) cat("-> ", blk, "\n", sep="")
  y <- x; attr(y,"s_block") <- NULL; attr(y,"s_sep") <- NULL
  class(y) <- "noquote"
  ## print with a separator rule every 'sep' data rows
  mat <- unclass(y)
  header <- colnames(mat)
  line.width <- sum(nchar(header)) + 2*(length(header)-1)
  rule <- paste(rep("-", max(line.width, 20)), collapse="")
  cat(paste(header, collapse="  "), "\n")
  cat(rule, "\n")
  for(i in seq_len(nrow(mat))){
    cat(paste(mat[i, ], collapse="  "), "\n")
    if(!is.null(sep) && sep > 0 && i %% sep == 0 && i < nrow(mat)) cat(rule, "\n")
  }
  invisible(x)
}

