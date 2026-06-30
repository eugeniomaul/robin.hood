graph.contvar <- function(data = NULL, 
	varname = NULL, 
	out.label = NULL, 
	graph.range = NULL, 
	run.independently = c("yes","no"), 
	page.rows = NULL,  ...){

	run.independently = match.arg(run.independently)

	if(is.data.frame(data)){
		if(length(varname) == 1 & (varname %in% names(data))){
		data2 <- as.vector(data[,varname])
		names(data2) <- rownames(data)
		data <- data2
	}
	}

	valid.data <- !is.na(data)
	length.data <- length(data)
	data <- data[valid.data]

	if(is.null(names(data))){
		names(data) <- 1:length(data)
	}

	if(length(out.label) == length.data){
		names(data) <- out.label[valid.data]
	}

	if(is.null(graph.range)){
		graph.range.min <- min(data)
		extra.margin.min <- ifelse(graph.range.min >= 0, 0.9, 1.1)
		graph.range.max <- max(data)
		extra.margin.max <- ifelse(graph.range.max >= 0, 1.1, 0.9)		
		graph.range <- c( (graph.range.min * extra.margin.min),(graph.range.max * extra.margin.max) )
	}

if(run.independently == "yes"){

if(sum(is.na(data))== length(data)){	
	plot(0,type='n',axes=FALSE,ann=FALSE)
	} else {

			quant<- t(quantile(data, c(0.25,0.75)))
			iqr <- quant[2] - quant[1]
	
			extra.box <- ( ((graph.range[2]-graph.range[1]) / iqr) > 5)

		if(extra.box){
			mat = matrix(c(1,2,1,3),2,2, byrow=TRUE)
            # FIXED: heights and widths pluralized
			nf <- layout(mat = mat, heights = c(3,1), widths = c(1,4))
			par(oma=c(2.1, 2.1, 2.1, 2.1))

			par(mar=c(1.1, 1.1, 1.1, 3.1))
			bxpdat <- boxplot(data,
				outline=FALSE)
		} else {
			mat = matrix(c(1,1,2,2),2,2, byrow=TRUE)
            # FIXED: heights pluralized
			nf <- layout(mat = mat, heights = c(2,1))
			par(oma=c(2.1, 4.1, 2.1, 3.1))
	
		}
	}
}
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

par(mar=c(1.1, 1.1, 1.1, 1.1))

if(sum(is.na(data))== length(data)){	
	plot(0,type='n',axes=FALSE,ann=FALSE,main=paste(varname, "Distribution (empty variable)", sep=" "),)
	plot(0,type='n',axes=FALSE,ann=FALSE)
	} else {

	if(!is.null(page.rows)){
		adj.cex.main <- ifelse(page.rows == 1, 1,
							ifelse(page.rows == 2, 0.8, 
								ifelse(page.rows == 3, 0.7,
									ifelse(page.rows == 4, 0.65,NA))))
		adj.cex.text <- round(adj.cex.main/1.5,digits=1)
		} else {
			adj.cex.main <- 1
			adj.cex.text <- 0.1
		}

h <- hist(data,xlim=graph.range, main=paste(varname,"Distribution",sep=" "), cex.main = adj.cex.main)

xfit<-seq(min(data),max(data),length=100) 
yfit<-dnorm(xfit,mean=mean(data),sd=sd(data)) 
yfit <- yfit*diff(h$mids[1:2])*length(data) 
lines(xfit, yfit, col="blue", lwd=2)

par(mar=c(1.1, 1.1, 1.1, 1.1))

bxpdat <- boxplot(data,horizontal = TRUE,
	ylim = graph.range, axes = FALSE)
if(length(bxpdat$out)>=1){
	new.pos <- rep(c(1,3),length.out = length(bxpdat$out) %/% 2)
	if( (length(bxpdat$out) %% 2) > 0 ){
		new.pos <- c(new.pos,1)
	}
	bxpdat$out2 <- bxpdat$out[order(bxpdat$out)]
	bxpdat$group <- bxpdat$group[order(bxpdat$out)]
	
text(bxpdat$out2, bxpdat$group, names(data)[data %in% bxpdat$out2], pos = new.pos, srt=45,cex=adj.cex.text)
}

} 
}