

ramu.plot <- function(data = NA, 
	plot.pdf = FALSE, 
	no.screen = FALSE, 
	plot.name = "figure1", 
	plot.height = 9, 
	plot.width = 8,
	xlim.coord = NULL, 
	x.ticks = NULL, 
	xaxis.title = NULL, 
	h.coeff.align = NULL, 
	v.coeff.align = -0.3,
	coeff.labels = c("name.p","name.coeff.p","name","full.1.line","full.2.line"),
	plot.order = c("increase","decrease","signif.increase","signif.decrease","none"), 
	signif.col = "red", 
	non.signif.col = "navyblue", 
	right.censor = NULL, 
	left.censor = NULL, 
	main.text = "", 
	axis.title.mag = 1.5, 
	axis.label.mag = 1.5, 
	axis.line.width = 1.5, 
	point.mag = 2, 
	point.type = 18, 
	ci.line.width = 1.5, 
	coeff.labels.mag = 1.25, 
	vertical.line.width = 2,
	plot.margins = c(8.1, 2.1, 4.1, 2.1),
	...){

plot.order = match.arg(plot.order)
coeff.labels = match.arg(coeff.labels) 

# 1 Retrieve key objects from the list
uni.coeff <- data[[2]]
uni.table <- data[[3]]
coeff.digits <- data$coeff.digits
p.digits <- data$p.digits
trim.digits <- data$trim.digits
mod.func <- data$mod.func
model.family <- data$model.family
mod.type <- data$mod.type
y.varname <- data$y.varname

if(mod.func=="cox"){
uni.coeff[,1:3] <- log(uni.coeff[,1:3]) 
}

# 2 Create a sorting index based on the plot.order argument
if(plot.order=="increase"){
	coeff.order <- order(uni.coeff[,1])
}else if(plot.order=="decrease"){
	coeff.order <- order(-uni.coeff[,1])	
}else if(plot.order=="signif.increase"){	
	coeff.order <- order(uni.coeff[,4]>0.05, uni.coeff[,1])	
}else if(plot.order=="signif.decrease"){	
	coeff.order <- order(uni.coeff[,4]>0.05, -uni.coeff[,1])	
}else if(plot.order=="none"){
	coeff.order <- nrow(uni.coeff):1
}

# 3 Create a color vector for each of the lines to be plotted
line.col <- ifelse(uni.coeff[,4]<=0.05,signif.col,non.signif.col)

# 4 If no.screen is TRUE Generate a PDF file directly without displaying it with name specified
if(no.screen==TRUE){
pdf(paste(plot.name,"_",mod.type,"_",format(Sys.time(),"%Y_%m_%d_%H%M"),".pdf",sep=""),height=plot.height,width=plot.width)
}

# 5 Censor Extension of plotted Confidence intervals if applicable
if(!is.null(right.censor)){
	right.censor2 <- uni.coeff[,3]>right.censor
	uni.coeff[right.censor2,3] <- right.censor
}

if(!is.null(left.censor)){
	left.censor2 <- uni.coeff[,2]<left.censor
	uni.coeff[left.censor2,2] <- left.censor
}

# 6 If h.coeff.align is null create a numeric argument to align text to the right of the coefficients
if(is.null(h.coeff.align)){
	h.coeff.align <- max(uni.coeff[,3])
}

# 7 If xlim.coord argument is null automatically specify the x dimension
if(is.null(xlim.coord) & h.coeff.align=="right.next"){
	x.start <- min(uni.coeff[,2])
	x.stop <- max(uni.coeff[,3])
	xlim.coord <- c(x.start,x.stop + (x.stop-x.start)*1)
}else if(is.null(xlim.coord) & h.coeff.align=="left.next"){
	x.start <- min(uni.coeff[,2])
	x.stop <- max(uni.coeff[,3])
	xlim.coord <- c(x.start-(x.stop-x.start)*1,x.stop)	
}else if(is.null(xlim.coord) & max(h.coeff.align>0)){
	x.start <- min(uni.coeff[,2])
	x.stop <- max(uni.coeff[,3])
	xlim.coord <- c(x.start,x.stop + (x.stop-x.start)*1)
}else if(is.null(xlim.coord) & min(h.coeff.align<0)){
	x.start <- min(uni.coeff[,2])
	x.stop <- max(uni.coeff[,3])
	xlim.coord <- c(x.start-(x.stop-x.start)*1,x.stop)
	
}

# 8 If no title was provided for the x axis then autogenerate it
if(is.null(xaxis.title)){
if(is.null(x.ticks) & (mod.func=="logistic" | (mod.func=="gee" & model.family=="binomial"))){
	xaxis.title <- bquote(paste(Delta," log odds (95% CI) ", .(y.varname),sep="")) 
}else if(!is.null(x.ticks) & (mod.func=="logistic" | (mod.func=="gee" & model.family=="binomial"))){
	xaxis.title <- paste("Odds Ratio (95% CI) ", y.varname,sep="") 
}else if(is.null(x.ticks) & mod.func=="cox"){
	xaxis.title <- "log Relative Hazard (95% CI) " 
}else if(!is.null(x.ticks) & mod.func=="cox"){
	xaxis.title <- "Hazard Ratio (95% CI) " 
}else if(mod.func=="lm" | (mod.func=="gee" & model.family=="gaussian")){
	xaxis.title <- bquote(paste(Delta, .(y.varname), " (95% CI) ",sep="")) 
}
}

# 9 Plot the main graph
par(mar=plot.margins)
plot(uni.coeff[,1],coeff.order,
	main=main.text,
	xlab="",
	ylab="",
	xlim=xlim.coord,
	ylim=c(0.5,nrow(uni.coeff)+0.5),
	pch=NA,cex.lab=axis.title.mag, axes=F)

# 10 Overlay the title for the X axis
mtext(xaxis.title,side=1, line=4.5, at=0, adj=0.5, cex=axis.title.mag)


# 11 Overlay a manually created X axis on the plot
if((mod.func=="logistic" | mod.func=="cox" | model.family=="binomial") & !is.null(x.ticks)){
at.x.ticks <- log(x.ticks)
axis(1, at=at.x.ticks, labels=x.ticks,cex.axis=axis.label.mag,lwd=axis.line.width)
}else{
axis(1, at=x.ticks, labels=TRUE,cex.axis=axis.label.mag,lwd=axis.line.width)	
}

# 12 Vertical Dashed line at 0
abline(v=0,lty="dashed",lwd=vertical.line.width)

# 13 Generate a vector with the h.coeff.align2 vector and modify the text.adj variable if applicable
text.adj <- 0

if(h.coeff.align=="right.next"){
	h.coeff.align2 <- uni.coeff[,3]+ 0.01*(max(uni.coeff[,3])-min(uni.coeff[,2]))
	text.adj <- 0 
}else if(h.coeff.align=="left.next"){
	h.coeff.align2 <- uni.coeff[,2]- 0.01*(max(uni.coeff[,3])-min(uni.coeff[,2])) 
	text.adj <- 1 
}else if(is.numeric(h.coeff.align) & length(h.coeff.align)==1){
	h.coeff.align2 <- rep(h.coeff.align,nrow(uni.coeff))
	text.adj <- ifelse(h.coeff.align<0,1,0)
}else if(is.numeric(h.coeff.align) & length(h.coeff.align)>1){
	h.coeff.align2 <- h.coeff.align
	text.adj <- ifelse(min(h.coeff.align)<0,1,0)
}

# 14 Create the line names variable
if(coeff.labels=="name"){
	line.names <- row.names(uni.table)
}else if(coeff.labels=="full.1.line"){
	line.names <- paste(row.names(uni.table), " ",uni.table[,1], ", p=",uni.table[,2],sep="")
}else if(coeff.labels=="full.2.line"){
	line.names <- paste(row.names(uni.table), "\n",uni.table[,1], ", p=",uni.table[,2],sep="")
}else if(coeff.labels=="name.p"){
	line.names <- paste(row.names(uni.table), " (p=",uni.table[,2],")",sep="")
}else if(coeff.labels=="name.coeff.p"){
	line.names <- paste(row.names(uni.table), " (", format(round(uni.coeff[,1],coeff.digits),nsmall=coeff.digits,trim=trim.digits), ", p=",uni.table[,2],")",sep="")
}

# 15 Loop to add symbols for estimates, lines for 95% CI and text for coeff names +/- the numeric values, p value
for(i in 1:nrow(uni.coeff)){
temp.i <- coeff.order[i]
points(uni.coeff[temp.i,1],i,pch=point.type, cex=point.mag, col=line.col[temp.i])
lines(c(uni.coeff[temp.i,2],uni.coeff[temp.i,3]),c(i,i), lwd=ci.line.width, col=line.col[temp.i]) 
text(h.coeff.align2[temp.i],(i+v.coeff.align),line.names[temp.i],cex=coeff.labels.mag, adj=text.adj)
}

# 16 If plot.pdf =TRUE then save the plot
if(plot.pdf==TRUE & no.screen== FALSE){
dev.copy(pdf,file=paste(plot.name,"_",mod.type,"_",format(Sys.time(),"%Y_%m_%d"),".pdf",sep=""),height=plot.height,width=plot.width)
dev.off()
}

# 17 If no.screen =TRUE then plot close the device
if(no.screen == TRUE){
dev.off()
}

}
 

