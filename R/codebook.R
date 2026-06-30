codebook <- function(data=NA, blanks=c("separate","missing"), selected.vars=NULL, cont.digits=1, export.table=NA, sep=c(",",";")){

  blanks = match.arg(blanks)
  sep = match.arg(sep)

  data <- as.data.frame(data)

  if(!is.null(selected.vars)){
    data <- subset(data,select=selected.vars)
  }

  out<- list()

  xx <- dim(data)
  yy <- matrix(nrow=2,ncol=2, c(xx[1],xx[2]," observations","variables"))
  out$dimensions <- yy

  is.date <- function(x) inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))
  date.vars <- sapply(data, is.date)

  yy.1 <- matrix(nrow=5,ncol=2, 
  c(
    sum(sapply(data,is.numeric)),
    sum(sapply(data,is.factor)),
    sum(sapply(data,is.logical)),
    sum(sapply(data,is.date)),	
    sum(sapply(data,is.character)),
    "Numeric Variables",
    "Categorical Variables",
    "Logical Variables (TRUE/FALSE)",
    "Date Variables",
    "String Variables (Free Text)"))

  out$vartypes <- yy.1

  # BUG FIXED HERE: Changed to a 3x2 matrix to hold all 6 elements
  yy.2 <- matrix(nrow=3,ncol=2, c(
    paste(sum(complete.cases(data)),"/",xx[1]," (",round(sum(complete.cases(data))/xx[1]*100,digits=0),"%)",sep=""),
    paste(sum(!complete.cases(data)),"/",xx[1]," (",round(sum(!complete.cases(data))/xx[1]*100,digits=0),"%)",sep=""),
    "", 
    "Complete Data",
    "Incomplete Data (Missing values for some or all variables)",
    "Empty variables (i.e. '   ') are not displayed as missing"))	
  out$complete.data <- yy.2

  var.type <- function(x){
    if(is.numeric(x)) return("numeric")
    if(is.factor(x)) return("categorical")
    if(is.date(x)) return("date")
    if(is.logical(x)) return("logical (TRUE/FALSE)")
    if(is.character(x)) return("string (Free Text)")
    return(NA)
  }

  miss.var <- function(x){
    y <- sum(is.na(x))
    length.x <- length(x)
    y.na <- y/length.x
    if(y.na==1) return("empty variable")
    
    if(is.numeric(x) | is.logical(x) | is.date(x) ){
      return(paste(y,"/",length.x," (", round(y.na*100,digits=0), "%)",sep=""))
    } else {
      yy <- trimws(x,"left")
      yy <- sum(yy=="",na.rm=TRUE)
      if(blanks=="separate" & yy>0){
        y <- paste(y,"/",length.x," (", round(y.na*100,digits=0), "%)",sep="")
        yyy <- paste(" [",yy,"/",length.x," (", round(yy/length.x*100,digits=0), "%)  blank]",sep="")
        return(paste(y,yyy,sep=""))
      }else{
        y <- y+yy
        if(y/length.x==1) return("empty variable")
        return(paste(y,"/",length.x," (", round(y/length.x*100,digits=0), "%)",sep=""))
      }
    }
  }

  median.levels <- function(x){
    if(all(is.na(x))) return("") 
    
    if(is.numeric(x) | is.date(x)){
      x.original <- x
      if(is.date(x)) x <- as.numeric(x)
      
      quant <- t(quantile(x,c(0.5,0.25,0.75),na.rm=TRUE))
      quant <- round(quant,digits=cont.digits)
      
      if(is.date(x.original)) quant <- as.Date(quant, origin="1970-01-01")
      
      if(!is.na(quant[1])){
        return(paste(quant[1]," (",quant[2],"-",quant[3],")", sep=""))
      } else {
        return("")
      }
    } else if(is.logical(x)){
      y <- table(x,useNA="ifany")
      if(sum(y[!is.na(names(y))],na.rm=TRUE) < 1) return("")
      
      y <- cbind(y,prop.table(y))
      y <- y[1:min(2, nrow(y)), , drop=FALSE]
      z <- ""
      for(i in 1:nrow(y)){
        z <- paste(z, rownames(y)[i]," (",round(y[i,2]*100,digits=0),"%)",ifelse(i==1," /",""),sep="")
      }
      return(z)
    } else if(is.factor(x)){
      y <- table(x,useNA="no")
      if(sum(y) < 1) return("")
      
      y.empty <- sum(y==0)
      return(paste(length(y)," levels",ifelse(y.empty==0,"",paste(" (",y.empty," empty)",sep="")),".", sep=""))
    } else if(is.character(x)){
      new.x <- trimws(unique(x), "both")
      new.x <- sum(!is.na(new.x) & new.x!="")
      if(new.x==0) return("")
      return(paste("Free text (",new.x," levels).",sep=""))
    }
    return("")
  }

  range.reference <- function(x){
    if(all(is.na(x))) return("") 
    
    if(is.numeric(x)){
      quant <- t(quantile(x,c(0.5),na.rm=TRUE))
      if(!is.na(quant[1])){
        return(paste("(min-max): ", "(",round(min(x,na.rm=TRUE),digits=cont.digits),": ",round(max(x,na.rm=TRUE),digits=cont.digits),")",sep=""))
      }
    } else if(is.date(x)){
      x <- as.numeric(x)
      quant <- c(t(quantile(x,c(0.5),na.rm=TRUE)),
                 round(min(x,na.rm=TRUE),digits=cont.digits),
                 round(max(x,na.rm=TRUE),digits=cont.digits))
      quant <- as.Date(quant, origin="1970-01-01")
      if(!is.na(quant[1])){
        return(paste("(min-max): ", "(",quant[2],": ",quant[3], ")",sep=""))
      }
    } else if(is.factor(x)){
      y <- table(x,useNA="no")
      if(sum(y) >= 1){
        y.prop <- cbind(y,prop.table(y))
        return(paste("Ref: ", substr(names(y)[1],start=1,stop=10),ifelse(nchar(names(y)[1])>10,"...",""),
                     "(",round(y.prop[1,2]*100,digits=0),"%)",sep=""))
      }
    }
    return("")
  }

  yy.3a <- c("Variable Name", names(data))
  yy.3b <- c("Variable Type", sapply(data,var.type))
  yy.3c <- c("N (%) Missing", sapply(data,miss.var))
  yy.3d <- c("Median /N of Levels", sapply(data,median.levels))
  yy.3e <- c("Range /Reference Categ", sapply(data,range.reference))

  yy.3 <- cbind(yy.3a,yy.3b,yy.3c,yy.3d,yy.3e)
  out$summary <- yy.3

  out$dimensions <- noquote(format(out$dimensions,justify="left"))
  out$vartypes <- noquote(format(out$vartypes,justify="left"))
  out$complete.data <- noquote(format(out$complete.data,justify="left"))
  out$summary <- noquote(format(out$summary,justify="left"))

  cat("\n\n##################################################################\nDatabase Dimensions\n")
  write.table(format(out$dimensions, justify="right"),row.names=FALSE, col.names=FALSE,quote=FALSE)
  cat("\n\nDatabase Variables /Types\n")
  write.table(format(out$vartypes, justify="right"),row.names=FALSE, col.names=FALSE,quote=FALSE)
  cat("\n\nMissing Data\n")
  write.table(format(out$complete.data, justify="right"),row.names=FALSE, col.names=FALSE,quote=FALSE)
  cat("\n\n##################################################################\nVariable General Summary\n")
  write.table(format(out$summary, justify="right"),row.names=FALSE, col.names=FALSE,quote=FALSE)
  cat("\n##################################################################\n")

  if(!is.na(export.table)){
    dimensions.temp <- cbind(out$dimensions, rep("",times=2), rep("",times=2), rep("",times=2))
    dimensions.temp <- rbind(c("Database Dimensions",rep("",times=4)), dimensions.temp, c(rep("",times=5)))
                  
    vartypes.temp <- cbind(out$vartypes,rep("",times=4),rep("",times=4),rep("",times=4))
    vartypes.temp <- rbind(c("Database Variables /Types",rep("",times=4)), vartypes.temp, c(rep("",times=5)))
    
    complete.data.temp <- cbind(out$complete.data, rep("",times=2), rep("",times=2), rep("",times=2))
    complete.data.temp <- rbind(c("Missing Data",rep("",times=4)), complete.data.temp, c(rep("",times=5)), c(rep("",times=5)), c(rep("",times=5)))

    summary.temp <- rbind(c("Variable General Summary",rep("",times=4)), out$summary, c(rep("",times=5)))
    out.write <- rbind(dimensions.temp,vartypes.temp,complete.data.temp,summary.temp)
     
    write.table(format(noquote(out.write),justify="left"), file=paste(export.table,format(Sys.time(),"%Y_%m_%d"),".csv",sep=""),row.names=FALSE,col.names=FALSE,sep=sep,dec=".",fileEncoding = "latin1") 
  }
  return(invisible(out))
}