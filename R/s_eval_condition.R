## Internal helper: evaluate an unquoted row-selection condition inside a
## dataframe, base-subset() style. Returns a logical vector the SAME length as
## nrow(dataframe), where NA results are treated as FALSE (row dropped), exactly
## as base::subset() does.
##
## cond.sub : the result of substitute(condition) in the calling function
## dataframe: the data.frame to evaluate against
## eval.env : the caller's environment (so variables not in the data still resolve)
## df.name  : dataframe name, for error messages
.s_eval_condition <- function(cond.sub, dataframe, eval.env, df.name){

  ## Evaluate the expression with the dataframe as the first environment, so
  ## bare column names (age, sex, race) resolve to columns.
  r <- tryCatch(
    eval(cond.sub, dataframe, eval.env),
    error = function(e){
      ## Targeted hint for the classic Stata -> R mistake: single = for ==.
      expr.txt <- paste(deparse(cond.sub), collapse=" ")
      lone.eq  <- grepl("[^=<>!]=[^=]", expr.txt)   # an = not part of == <= >= !=
      msg <- paste("Could not evaluate the condition on ", df.name, ": ",
                   conditionMessage(e), sep="")
      if(lone.eq){
        msg <- paste(msg,
                     "\n  Hint: did you use '=' instead of '=='? In R, comparisons use '==' (e.g. sex==\"female\").",
                     sep="")
      }
      stop(msg, call. = FALSE)
    }
  )

  if(!is.logical(r)){
    stop(paste("The condition on ", df.name,
               " must be a logical expression (something that is TRUE/FALSE for each row).",
               sep=""), call. = FALSE)
  }
  if(length(r) != nrow(dataframe)){
    stop(paste("The condition on ", df.name, " produced ", length(r),
               " values but the dataframe has ", nrow(dataframe),
               " rows. Check that the condition refers to variables in the dataframe.",
               sep=""), call. = FALSE)
  }

  ## base-subset() semantics: NA -> FALSE (row dropped)
  r & !is.na(r)
}
