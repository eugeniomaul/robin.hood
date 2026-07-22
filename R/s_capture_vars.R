## Internal helper: resolve a varlist argument that may be given as
##  - unquoted single name:      age
##  - unquoted vector of names:  c(age, sex, md)
##  - quoted single or vector:   "age" / c("age","sex")
## Returns a character vector of variable names.
.s_capture_vars <- function(sub){
  ## sub is the result of substitute(varlist) from the calling function
  if(is.character(sub)){
    return(as.character(sub))                    # already quoted string(s)
  } else if(is.call(sub) && identical(sub[[1]], as.name("c"))){
    ## c(age, sex, md) -> drop the 'c', deparse each remaining element
    return(vapply(as.list(sub)[-1], function(e){
      if(is.character(e)) e else deparse(e)
    }, character(1)))
  } else {
    return(deparse(sub))                          # single unquoted symbol
  }
}