s_logistic <- function(dataframe, outcome, varlist, interactions = NULL,
                       coeff.digits = 6, condition, verbose = TRUE){

  df.name <- deparse(substitute(dataframe))

  if(!is.data.frame(dataframe)){
    stop(paste(df.name, " is not a data.frame", sep=""))
  }

  eval.env <- parent.frame()
  outcome  <- .s_capture_flex(substitute(outcome), eval.env)
  varlist  <- .s_capture_flex(substitute(varlist), eval.env, default = NULL)

  if(!missing(condition)){
    keep.rows <- .s_eval_condition(substitute(condition), dataframe, eval.env, df.name)
    dataframe <- dataframe[keep.rows, , drop = FALSE]
  }

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
  model <- glm(form, data = dataframe, family = binomial)
  null  <- update(model, . ~ 1)              # same rows as model, for LR test
  s     <- summary(model)

  co    <- s$coefficients                     # log-odds, SE, z, p
  ci.lo <- confint.default(model)             # Wald CIs on log-odds scale

  ## Drop intercept (Stata's logistic does not print _cons OR by default)
  keep  <- rownames(co) != "(Intercept)"
  co    <- co[keep, , drop = FALSE]
  ci.lo <- ci.lo[keep, , drop = FALSE]
  terms <- rownames(co)

  or      <- exp(co[, "Estimate"])
  se.or   <- or * co[, "Std. Error"]          # delta-method SE on OR scale
  ci.or   <- exp(ci.lo)

  body <- cbind(
    .s_num(or,               coeff.digits),
    .s_num(se.or,            coeff.digits),
    .s_num(co[, "z value"],  2),
    .s_num(co[, "Pr(>|z|)"], 3),
    .s_num(ci.or[, 1],       coeff.digits),
    .s_num(ci.or[, 2],       coeff.digits)
  )
  col.labels <- c("Odds Ratio", "Std. Err.", "z", "P>|z|", "[95% Conf.", "Interval]")
  coef.display <- .s_coef_matrix(body, col.labels, terms)

  ## --- Header block --------------------------------------------------------
  ll     <- as.numeric(logLik(model))
  ll0    <- as.numeric(logLik(null))
  LR     <- 2 * (ll - ll0)
  df.lr  <- length(coef(model)) - 1
  LRp    <- pchisq(LR, df.lr, lower.tail = FALSE)
  pseudo <- 1 - ll / ll0
  n      <- length(model$residuals)

  header <- list(n = n, LR = LR, df.lr = df.lr, LRp = LRp,
                 loglik = ll, pseudo = pseudo, outcome = outcome)

  display <- structure(list(coef = coef.display, header = header, type = "logistic"),
                       class = "s_model_display")

  out <- list(tables = list("Logistic regression" =
                              list(display = display, model = model,
                                   coef.raw = co, or = or, ci.or = ci.or,
                                   header = header)),
              call.info = list(type = "logistic", outcome = outcome,
                               dataframe = df.name))
  class(out) <- "s_describe"

  if(verbose){ cat("\n"); print(display); cat("\n") }
  return(invisible(out))
}