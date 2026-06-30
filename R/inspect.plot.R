inspect.plot <- function(data = NULL, 
	include.missing =  c("yes","no"), 
	graphs = NULL, 
	graphs.filename = NULL, 
	page.width = NULL, 
	page.height = NULL,
	out.label = NULL, 
	 ...){

if(!is.data.frame(data)){
	stop("Please enter a valid dataframe to run the function")
}

include.missing = match.arg(include.missing)

excluded.data <- sapply(data,is.character)
excluded.data <- names(data)[excluded.data]
excluded.data <- subset(data, select = excluded.data)

if(ncol(excluded.data)>=1){
excluded.varnames <- names(excluded.data)
cat("\n\nThe following variables are free text and were not plotted: ")
cat(excluded.varnames)
cat("\n\n")

included.data <- sapply(data,(is.character))
included.data <- !included.data	
included.data <- names(data)[included.data]
data <- subset(data, select = included.data)
}

if(is.null(graphs)){
	graphs <- 6
} else if(graphs < 1){
	stop("graphs argument must be between 1 and 12")
} else if(graphs > 12){
	message("Maximum number of graphs per page is 12. Value truncated to 12.")
	graphs <- 12
} else {
	graphs <- round(graphs)
	graphs <- ifelse(graphs > 12, 12, graphs)
}

page.width <- ifelse(is.null(page.width), 7, page.width)
page.height <- ifelse(is.null(page.height),(page.width*9/7), page.height)


length.vars <- ncol(data)
pages <- ceiling(length.vars/graphs)
page.cols <- ifelse(graphs>=1 & graphs <=2,1,
				ifelse(graphs>=3 & graphs <=8,2,
					ifelse(graphs>=9 & graphs <=12,3,NA)))
page.rows <- ifelse(graphs==1,1,
				ifelse(graphs>=2 & graphs <=4,2,
					ifelse(graphs>=5 & graphs <=6,3,
						ifelse(graphs>=7 & graphs <=8,4,
							ifelse(graphs==9 ,3,
								ifelse(graphs>=10 & graphs <=12,4, NA))))))


modulus.vars <- length.vars %% graphs
modulus.factor <- ifelse(modulus.vars == 0, 1, 0)
is.date <- function(x) inherits(x, 'Date')

classify.vars <- function(x){
	if(is.numeric(x) | is.date(x)){
		type <- "continuous"
	} else if(is.factor(x) | is.logical(x)) {
		type <- "categorical"
	}
}

if(is.null(graphs.filename)){
	graphs.filename <- "dataframe_plots"
} else {
	graphs.filename <- graphs.filename
}

# FIXED: height and width are strictly singular for pdf()
pdf(paste(graphs.filename,"_",format(Sys.time(),"%Y_%m_%d"),".pdf",sep=""), height=page.height, width=page.width)

for(i in 1:pages){

		from.col <- ((i-1)*graphs) + 1
		if(i != pages){
			to.col <- i*graphs
		} else if(i== pages){
			to.col <-(i-1)*graphs +  modulus.vars			
		}

		if((to.col-from.col) == 0){
		temp.data <- as.data.frame(data[,from.col:to.col])
		names(temp.data)[1] <- names(data)[from.col]
		} else {
		temp.data <- data[,from.col:to.col]
		}
		var.types <- sapply(temp.data,classify.vars)
		graph.heights <- rep(c(3,1), times= ceiling(graphs/page.cols))
		
		for(j in 1:length(var.types)){
			if(j==1){
				if(var.types[j] == "continuous"){
					new.mat <- c(1,2)
				}else{
					new.mat <- c(1,1)
				}
			} else {
				max.mat <- max(new.mat)
				if(var.types[j] == "continuous"){
				new.addition.to.mat <- c(max.mat+1, max.mat+2)
				}else{
				new.addition.to.mat <- c(max.mat+1, max.mat+1)
				}
				new.mat <- c(new.mat, new.addition.to.mat)
			}
		}
		graph.blanks <- (page.rows*2*page.cols) - length(new.mat)
		if(graph.blanks > 0){
		graph.blanks <- rep(0,times=graph.blanks)
		new.mat <- c(new.mat,graph.blanks)
		}
		final.mat <- matrix(new.mat,nrow=page.rows*2,ncol=page.cols)
        
        # FIXED: heights is strictly plural for layout()
		nf <- layout(mat = final.mat, heights = graph.heights)


if(page.cols==1){
	right.left <- 5
	} else if(page.cols == 2){
	right.left <- 3
	} else if(page.cols == 3){
	right.left <- 2
	} 

if(page.rows==1){
	top.bottom <- 10
	} else if(page.rows == 2){
	top.bottom <- 6
	} else if(page.rows == 3){
	top.bottom <- 4
	} else if(page.rows == 4){
	top.bottom <- 2
	} 
	
	added.oma <- c(top.bottom, right.left,top.bottom,right.left)
	new.oma <- c(2.1, 2.1, 2.1, 2.1) + added.oma
	par(oma=new.oma)

	for(k in 1:length(var.types)){
		varname <- names(temp.data)[k]
		if(var.types[k] == "continuous"){
		graph.contvar(temp.data, varname, out.label = out.label, run.independently = "no", page.rows = page.rows)			
		} else if(var.types[k] == "categorical"){
		graph.catvar(temp.data, varname, out.label = NA, include.missing = include.missing, page.rows = page.rows)
		}
		}	
	}	

dev.off()
}
