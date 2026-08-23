s_export <- function(..., file = "s_export", sep = ",", verbose = TRUE){

  objs <- list(...)
  if(length(objs) == 0){
    stop("Supply at least one object created by s_tab, s_tab1, s_summarize, s_regress or s_logistic.")
  }
  for(i in seq_along(objs)){
    if(!inherits(objs[[i]], "s_describe")){
      stop(paste("Argument ", i, " is not an object created by s_tab, s_tab1, s_summarize, s_regress or s_logistic.", sep=""))
    }
  }

  ## Convert one display object into a plain character matrix for the CSV.
  as.block <- function(disp){
    if(!is.null(disp$labels)){                       # one-way table
      m <- cbind(c(disp$var.name, disp$labels),
                 c(disp$col.title, disp$values))
      return(m)
    }
    ## two-way table
    n.c   <- length(disp$col.names)
    lines <- list()
    lines[[1]] <- c(paste("Key:", disp$key), rep("", n.c))
    lines[[2]] <- c("", disp$col.var, rep("", n.c - 1))
    lines[[3]] <- c(disp$row.var, disp$col.names)
    for(b in disp$blocks){
      for(k in seq_along(b$lines)){
        lab <- if(k == 1) b$label else ""
        lines[[length(lines) + 1]] <- c(lab, b$lines[[k]])
      }
    }
    if(!is.null(disp$tests)){
      for(tl in disp$tests) lines[[length(lines) + 1]] <- c(tl, rep("", n.c))
    }
    do.call(rbind, lines)
  }

  ## s_summarize displays are plain matrices with a header row
  as.block.summ <- function(disp){
    m   <- unclass(disp)
    attr(m, "s_block") <- NULL; attr(m, "s_sep") <- NULL
    hdr <- colnames(m)
    rbind(hdr, m)
  }

  ## s_regress / s_logistic displays: model-fit header rows above the
  ## coefficient table.
  as.block.model <- function(disp){
    h  <- disp$header
    n2 <- function(v, d) format(round(v, d), nsmall = d, trim = TRUE)
    m  <- unclass(disp$coef)

    if(disp$type == "regress"){
      hdr <- rbind(
        c(paste("Linear regression: outcome =", h$outcome), rep("", ncol(m) - 1)),
        c(paste("Number of obs =", h$n),                     rep("", ncol(m) - 1)),
        c(paste("F(", h$df.mod, ",", h$df.res, ") =", n2(h$F, 2)), rep("", ncol(m) - 1)),
        c(paste("Prob > F =", n2(h$Fp, 4)),                  rep("", ncol(m) - 1)),
        c(paste("R-squared =", n2(h$r2, 4)),                 rep("", ncol(m) - 1)),
        c(paste("Adj R-squared =", n2(h$adj.r2, 4)),         rep("", ncol(m) - 1)),
        c(paste("Root MSE =", n2(h$root.mse, 3)),            rep("", ncol(m) - 1))
      )
    } else {
      hdr <- rbind(
        c(paste("Logistic regression: outcome =", h$outcome), rep("", ncol(m) - 1)),
        c(paste("Number of obs =", h$n),                       rep("", ncol(m) - 1)),
        c(paste("LR chi2(", h$df.lr, ") =", n2(h$LR, 2)),      rep("", ncol(m) - 1)),
        c(paste("Prob > chi2 =", n2(h$LRp, 4)),               rep("", ncol(m) - 1)),
        c(paste("Log likelihood =", n2(h$loglik, 6)),         rep("", ncol(m) - 1)),
        c(paste("Pseudo R2 =", n2(h$pseudo, 4)),              rep("", ncol(m) - 1))
      )
    }
    rbind(hdr, m)
  }

  blocks <- list()
  for(o in objs){
    for(nm in names(o$tables)){
      tb   <- o$tables[[nm]]
      disp <- tb$display
      blk  <- if(inherits(disp, "s_model_display"))     as.block.model(disp)
              else if(inherits(disp, "s_summarize_display")) as.block.summ(disp)
              else as.block(disp)
      title <- matrix(c(nm, rep("", ncol(blk) - 1)), nrow = 1)
      blocks[[length(blocks) + 1]] <- title
      blocks[[length(blocks) + 1]] <- blk
      blocks[[length(blocks) + 1]] <- matrix("", nrow = 1, ncol = ncol(blk))
    }
  }

  ## Pad every block to the widest, so they stack in one sheet
  max.cols <- max(vapply(blocks, ncol, integer(1)))
  blocks <- lapply(blocks, function(b){
    if(ncol(b) < max.cols){
      cbind(b, matrix("", nrow = nrow(b), ncol = max.cols - ncol(b)))
    } else b
  })

  out <- do.call(rbind, blocks)

  fname <- paste(file, "_", format(Sys.time(), "%Y_%m_%d"), ".csv", sep="")
  write.table(out, file = fname, row.names = FALSE, col.names = FALSE,
              sep = sep, dec = ".", quote = TRUE)

  if(verbose){
    message(paste("Exported ", length(objs), " object(s) to ", fname, sep=""))
  }
  return(invisible(out))
}