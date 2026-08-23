## Flexible capture of a variable-name argument that may be given as:
##   - unquoted single name:        age
##   - unquoted vector of names:    c(age, sex)
##   - quoted single or vector:     "age" / c("age","sex")
##   - a pre-built variable holding a character vector:  myvars
##   - left at its default (NA or NULL)
##
## Strategy: first honour NA/NULL defaults; then TRY to evaluate the captured
## expression in the caller's environment. If that yields a character vector,
## use it (covers quoted strings AND pre-built name-holding variables). If it
## fails (bare column symbols aren't real objects), fall back to treating the
## expression as literal unquoted name(s).
##
## sub      : substitute(arg) from the calling function
## eval.env : the caller's environment (parent.frame() of the calling function)
## default  : the argument's default (NA or NULL) so we can pass it through
.s_capture_flex <- function(sub, eval.env, default = NA){

  ## 1. Left at default? (substitute returns the default value itself)
  if(is.null(sub)) return(NULL)
  if(length(sub) == 1 && is.logical(sub) && is.na(sub)) return(default)

  ## 2. Already a plain character constant in the call: "age" or c("age","sex")
  ##    (is.character catches a bare string; the c(...) case is handled by eval below)
  if(is.character(sub)) return(as.character(sub))

  ## 3. Try evaluating in the caller's environment. Succeeds for:
  ##    - c("age","sex")        -> character vector
  ##    - myvars (holds names)  -> character vector
  ##    Fails (error) for bare column symbols like age or c(age, sex).
  evaluated <- tryCatch(eval(sub, envir = eval.env),
                        error = function(e) NULL)
  if(is.character(evaluated) && length(evaluated) >= 1){
    return(as.character(evaluated))
  }

  ## 4. Fall back to literal unquoted name(s): age  OR  c(age, sex)
  if(is.call(sub) && identical(sub[[1]], as.name("c"))){
    return(vapply(as.list(sub)[-1], function(e){
      if(is.character(e)) e else deparse(e)
    }, character(1)))
  }
  if(is.name(sub)) return(deparse(sub))

  ## 5. Anything unexpected: deparse as a last resort
  deparse(sub)
}