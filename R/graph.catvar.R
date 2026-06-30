
graph.catvar <- function(data = NULL, 
	varname = NULL, 
	out.label = NULL, 
	include.missing = c("no","yes"), 
	page.rows = NULL, ...){

include.missing = match.arg(include.missing)

	if(is.data.frame(data)){
		if(length(varname) == 1 & (varname %in% names(data))){
		data2 <- as.vector(data[,varname])
		names(data2) <- rownames(data)
		data <- data2
	}
	}

par(las=2) # make label text perpendicular to axis
par(mar=c(5,6,4,2)) # increase y-axis margin.

table.var <- data

if(include.missing == "yes"){
	counts <- table(table.var, useNA = "ifany")
} else{
	counts <- table(table.var, useNA = "no")
}

names(counts)[is.na(names(counts))] <- "missing"

counts <- table(table.var)

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

if(!is.null(varname)){
	varname <- simpleCap(varname)
}else{
	varname <- "Variable"
}

	if(!is.null(page.rows)){
adj.cex.main <- ifelse(page.rows == 1, 1,
					ifelse(page.rows == 2, 0.8, 
						ifelse(page.rows == 3, 0.7,
							ifelse(page.rows == 4, 0.65,NA))))
} else {
	adj.cex.main <- 1
}

length.counts <- length(counts)
if(length.counts < 5){
y.lim <- c(0,6)
box.space <- 0.4
barplot(counts, main=paste(varname, "Distribution", sep=" "), horiz=TRUE, ylim = y.lim, space = box.space,
cex.names=0.8, cex.main=adj.cex.main)

} else {

barplot(counts, main=paste(varname, "Distribution", sep=" "), horiz=TRUE,
#names.arg=c("3 Gears", "4 Gears", "5   Gears"), 
cex.names=0.8, cex.main=adj.cex.main)
}

}
