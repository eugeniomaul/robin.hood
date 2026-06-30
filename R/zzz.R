.onAttach <- function(libname, pkgname) {
packageStartupMessage("
##################################################################
Robin Hood Package version 4.1
A package to help you become friends with R!
Developed by:
Maria Angelica Paulos MD MPH
Eugenio A. Maul MD MPH
#########################################################\n")	
}

# Silence R CMD Check notes for variables used internally by geepack/lmer formulas
if(getRversion() >= "2.15.1")  utils::globalVariables(c("idvar2", "n.eyes", "subj", "idn"))
	