lincom <- function(model, betas = NULL, mag = NULL, or = FALSE){

  ## Accept either a bare R model (lm/glm/...) OR an object created by
  ## s_regress / s_logistic. If given an s_ object, extract the fitted model.
  if(inherits(model, "s_describe")){
    if(length(model$tables) != 1){
      stop("This object contains more than one model; extract the specific model with model$tables[[i]]$model and pass that to lincom.")
    }
    model <- model$tables[[1]]$model
    if(is.null(model)){
      stop("No fitted model found in this object. lincom needs an s_regress or s_logistic result (or a plain lm/glm model).")
    }
  }

  ## Validated argument check (fixed: previously length(betas>=1))
  if(length(betas) >= 1 & length(betas) == length(mag)){

    coef.names <- rownames(summary(model)$coefficients)

    ## Accept Stata's "_cons" as an alias for R's "(Intercept)", so the label
    ## shown in the s_regress / s_logistic output can be used directly.
    betas[betas == "_cons"] <- "(Intercept)"

    ## Guard: every requested beta must match a real coefficient name, otherwise
    ## it would silently contribute zero to the linear combination.
    unmatched <- betas[!(betas %in% coef.names)]
    if(length(unmatched) >= 1){
      stop(paste("The following coefficient name(s) were not found in the model: ",
                 paste(unmatched, collapse = ", "),
                 ".\n  Use the names exactly as shown in the coefficient table",
                 " (factor and interaction terms carry R's level names, e.g. sexmale, age:sexmale;",
                 " the intercept is (Intercept) or _cons).", sep = ""))
    }

    title <- c()
    for(i in 1:length(betas)){
      if(i != length(betas) & sum(coef.names==betas[i])>=1){
        temp <- paste(betas[i], "*", mag[i], " + ", sep="")
        title <- paste(title, temp, sep="")
      }else if(sum(coef.names==betas[i])>=1){
        temp <- paste(betas[i], "*", mag[i], sep="")
        title <- paste(title, temp, sep="")
      }
    }

    K <- rep(0, times=length(coef.names))
    for(i in 1:length(coef.names)){
      temp.beta <- betas[i]
      K[coef.names==temp.beta] <- mag[i]
    }
    K <- matrix(K, 1)

    t   <- multcomp::glht(model, linfct = K)
    s.t <- summary(t)
    ci  <- confint(s.t)

    result <- list()
    result$linear.combination.for <- title

    if(or == TRUE){
      ## Exponentiate estimate and CI to the odds-ratio scale (Stata's , or).
      ## SE on the OR scale via the delta method; z and p unchanged (the test
      ## is invariant to the monotone transform).
      est.lin <- as.numeric(s.t$test$coefficients)
      se.lin  <- as.numeric(s.t$test$sigma)
      or.est  <- exp(est.lin)
      or.se   <- or.est * se.lin
      or.ci   <- exp(confint(s.t)$confint[, c("lwr","upr")])

      or.table <- cbind(
        "Odds Ratio" = or.est,
        "Std. Err."  = or.se,
        "z"          = as.numeric(s.t$test$tstat),
        "P>|z|"      = as.numeric(s.t$test$pvalues),
        "CI.lower"   = or.ci[1],
        "CI.upper"   = or.ci[2]
      )
      rownames(or.table) <- "lincom"
      result$or <- TRUE
      result$result <- or.table
      result$ci <- or.ci
    } else {
      result$or <- FALSE
      result$result <- s.t
      result$ci <- ci
    }

    return(result)

  } else {
    result <- "You must specify at least 1 coefficient (beta) and length of betas and magnitude must be the same"
    return(result)
  }
}