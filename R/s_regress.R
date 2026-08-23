s_regress <- function(dataframe, outcome, varlist, interactions = NULL,
                      coeff.digits = 6, condition, verbose = TRUE){

  df.name <- deparse(substitute(dataframe))

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  eval.env <- parent.frame()
  outcome  <- .s_capture_flex(substitute(outcome),  eval.env)
  varlist  <- .s_capture_flex(substitute(varlist),  eval.env, default = NULL)

  if(!missing(condition)){
    keep.rows <- .s_eval_condition(substitute(condition), dataframe, eval.env, df.name)
    dataframe <- dataframe[keep.rows, , drop = FALSE]
  }

  ## Convert character predictors to factors so they enter as categorical,
  ## matching Stata's i. behaviour (numeric stays numeric).
  all.vars.used <- unique(c(varlist,
                            if(!is.null(interactions))
                              unlist(strsplit(interactions, "[:*]"))))
  all.vars.used <- trimws(all.vars.used)
  for(v in all.vars.used){
    if(v %in% names(dataframe) && is.character(dataframe[[v]])){
      dataframe[[v]] <- as.factor(dataframe[[v]])
    }
  }

  form  <- .s_build_formula(outcome, varlist, interactions, dataframe, df.name)
  model <- lm(form, data = dataframe)
  s     <- summary(model)

  ## --- Coefficient table (Coef., Std. Err., t, P>|t|, 95% CI) --------------
  co    <- s$coefficients                      # Estimate, SE, t, p
  ci    <- confint.default(model)              # Wald CIs, matches Stata
  terms <- rownames(co)
  ## Stata prints the intercept last as _cons; reorder to match
  is.int <- terms == "(Intercept)"
  ord    <- c(which(!is.int), which(is.int))
  co     <- co[ord, , drop = FALSE]
  ci     <- ci[ord, , drop = FALSE]
  terms  <- terms[ord]
  terms[terms == "(Intercept)"] <- "_cons"

  body <- cbind(
    .s_num(co[, "Estimate"],   coeff.digits),
    .s_num(co[, "Std. Error"], coeff.digits),
    .s_num(co[, "t value"],    2),
    .s_num(co[, "Pr(>|t|)"],   3),
    .s_num(ci[, 1],            coeff.digits),
    .s_num(ci[, 2],            coeff.digits)
  )
  col.labels <- c("Coef.", "Std. Err.", "t", "P>|t|", "[95% Conf.", "Interval]")
  coef.display <- .s_coef_matrix(body, col.labels, terms)

  ## --- ANOVA / Source block ------------------------------------------------
  n      <- length(model$residuals)
  df.mod <- s$fstatistic["numdf"]
  df.res <- s$fstatistic["dendf"]
  df.tot <- df.mod + df.res
  ss.res <- sum(model$residuals^2)
  ss.tot <- sum((model$model[[1]] - mean(model$model[[1]]))^2)
  ss.mod <- ss.tot - ss.res
  ms.mod <- ss.mod / df.mod
  ms.res <- ss.res / df.res
  ms.tot <- ss.tot / df.tot
  Fstat  <- unname(s$fstatistic["value"])
  Fp     <- pf(Fstat, df.mod, df.res, lower.tail = FALSE)
  r2     <- s$r.squared
  adj.r2 <- s$adj.r.squared
  root.mse <- sqrt(ms.res)

  header <- list(
    n = n, F = Fstat, df.mod = unname(df.mod), df.res = unname(df.res),
    Fp = Fp, r2 = r2, adj.r2 = adj.r2, root.mse = root.mse,
    ss.mod = ss.mod, ss.res = ss.res, ss.tot = ss.tot,
    ms.mod = ms.mod, ms.res = ms.res, ms.tot = ms.tot,
    df.tot = df.tot, outcome = outcome
  )

  display <- structure(list(coef = coef.display, header = header, type = "regress"),
                       class = "s_model_display")

  out <- list(tables = list("Linear regression" =
                              list(display = display, model = model,
                                   coef.raw = co, ci.raw = ci, header = header)),
              call.info = list(type = "regress", outcome = outcome,
                               dataframe = df.name))
  class(out) <- "s_describe"

  if(verbose){ cat("\n"); print(display); cat("\n") }
  return(invisible(out))
}