t1 <- function(data=NA, selected.vars=NULL, var.names=NA, table.na=c("ifany","no","always"), cont.var=c("mean","median","auto","manual"), manual.mean=NA, cont.digits=1, auto.convert=TRUE, export.table=NA, sep=","){

  cont.var = match.arg(cont.var)
  table.na = match.arg(table.na)

  data <- as.data.frame(data)

  if(!is.null(selected.vars)){
    data <- subset(data,select=selected.vars)
  }

  is.date <- function(x) inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))

  if(auto.convert==TRUE){
    for(i in 1:ncol(data)){
      if(is.character(data[,i])==TRUE){
        if(length(levels(as.factor(data[,i]))) <10){
          data[,i] <- as.factor(data[,i])
        }		
      } else if(is.numeric(data[,i])==TRUE) {
        next	
      } else if(is.factor(data[,i])==TRUE){
        data[,i] <- as.factor(as.character(data[,i]))
      }else if(is.logical(data[,i])==TRUE){
        data[,i] <- as.factor( (as.numeric(data[,i])) )
      }
    }
  }

  dta.vartype <- lapply(data,class)
  dta.non.character <- as.vector(dta.vartype!="character")
  dta.select <- data[,dta.non.character, drop=FALSE]

  result.detail <- list()
  result.summary <- matrix(nrow=0,ncol=2)
  cont.var.result <- matrix(nrow=0,ncol=10)
  date.var.result <- matrix(nrow=0,ncol=10)

  for(i in 1:ncol(dta.select)){
    if(is.factor(dta.select[,i])==TRUE){
      temp.table.n <- table(dta.select[,i],useNA=table.na)
      table.total.n <- sum(temp.table.n)
      temp.table.p <- format(round(100*(prop.table(temp.table.n)),digits=1),nsmall=1,trim=TRUE)
      table.names <- replace(names(temp.table.n),is.na(names(temp.table.n)),"Missing")
      table.cells <- c("",paste(temp.table.n, " (",temp.table.p,"%)",sep=""))
      
      table.result <- matrix("",ncol=2,nrow=length(table.cells))
      table.result[1,1] <- table.total.n
      table.result[,2] <- table.cells
      colnames(table.result) <- c("Total N", "Statistic")
      
      var.name <- paste(ifelse(!is.na(var.names[i]), var.names[i], names(dta.select)[i]) ,", n (%)",sep="")
      rownames(table.result) <- c(var.name,paste("   ",table.names,sep=""))
      
      result.detail[[length(result.detail)+1]] <- table.result
      result.summary <- rbind(result.summary,table.result)

    } else if(is.numeric(dta.select[,i])==TRUE){
      x <- dta.select[,i]
      temp.x <- table(x,useNA="no")
      temp.x <- length(names(temp.x))
      
      if(temp.x > 1){
        shapiro.test2 <- if(sum(!is.na(x)) > 2 & sum(!is.na(x)) < 5000) tryCatch(round(shapiro.test(x)$p.value,4), error=function(e) NA) else NA
        lillie.test2 <- if(sum(!is.na(x)) > 4) tryCatch(round(nortest::lillie.test(x)$p.value, 4), error=function(e) NA) else NA
      } else {
        shapiro.test2 <- NA
        lillie.test2 <- NA
      }

      var.sum <- cbind(sum(!is.na(x)), mean(x,na.rm=TRUE), sd(x,na.rm=TRUE), t(quantile(x,c(0.5,0.25,0.75),na.rm=TRUE)), min(x,na.rm=TRUE), max(x,na.rm=TRUE), shapiro.test2, lillie.test2)
      colnames(var.sum) <- c("Total N","mean","sd","median","p25","p75","min","max","Shapiro.Test","Lilliefors.Test")
      var.name <- ifelse(!is.na(var.names[i]),var.names[i],names(dta.select)[i])
      rownames(var.sum) <- var.name
      cont.var.result <- rbind(cont.var.result,var.sum)	

      if(cont.var=="mean"){
        cont.var2 <- "mean"
      }else if(cont.var=="median"){
        cont.var2 <- "median"
      }else if(cont.var=="auto"){
        if((!is.na(var.sum[,9]) && var.sum[,1]<50 && var.sum[,9]>=0.05) | (!is.na(var.sum[,10]) && var.sum[,1]>=50 && var.sum[,10]>=0.05)){
          cont.var2 <- "mean"
        }else{
          cont.var2 <- "median"
        }
      }else if(cont.var=="manual"){
        if(!is.na(manual.mean[i]) & manual.mean[i]==FALSE){ cont.var2 <- "median" }
        else if(!is.na(manual.mean[i]) & manual.mean[i]==TRUE){ cont.var2 <- "mean" }
        else{ cont.var2 <- "mean" }
      }

      if(cont.var2=="mean"){
        temp.summary <- cbind(var.sum[,1], paste(format(round(var.sum[,2],digits=cont.digits),nsmall=cont.digits,trim=TRUE), " (", format(round(var.sum[,3],digits=cont.digits),nsmall=cont.digits,trim=TRUE), ")",sep=""))
        rownames(temp.summary) <- paste(ifelse(!is.na(var.names[i]), var.names[i], names(dta.select)[i]) ,", mean (sd)",sep="")
        result.summary <- rbind(result.summary,temp.summary)
      } else if(cont.var2=="median"){
        temp.summary <- cbind(var.sum[,1], paste(format(round(var.sum[,4],digits=cont.digits),nsmall=cont.digits,trim=TRUE), " (", format(round(var.sum[,5],digits=cont.digits),nsmall=cont.digits,trim=TRUE), ", ", format(round(var.sum[,6],digits=cont.digits),nsmall=cont.digits,trim=TRUE), ")",sep=""))
        rownames(temp.summary) <- paste(ifelse(!is.na(var.names[i]), var.names[i], names(dta.select)[i]) ,", median (IQR)",sep="")
        result.summary <- rbind(result.summary,temp.summary)
      } 

    } else if(is.date(dta.select[,i])==TRUE){
      x <- as.numeric(dta.select[,i])
      temp.x <- length(names(table(x,useNA="no")))
      
      if(temp.x > 1){
        shapiro.test2 <- if(sum(!is.na(x)) > 2 & sum(!is.na(x)) < 5000) tryCatch(round(shapiro.test(x)$p.value,4), error=function(e) NA) else NA
        lillie.test2 <- if(sum(!is.na(x)) > 4) tryCatch(round(nortest::lillie.test(x)$p.value, 4), error=function(e) NA) else NA
      } else {
        shapiro.test2 <- NA
        lillie.test2 <- NA
      }

      var.sum <- cbind(sum(!is.na(x)), mean(x,na.rm=TRUE), (sd(x,na.rm=TRUE)/30), t(quantile(x,c(0.5,0.25,0.75),na.rm=TRUE)), min(x,na.rm=TRUE), max(x,na.rm=TRUE), shapiro.test2, lillie.test2)
      colnames(var.sum) <- c("Total N","mean","sd (months)","median","p25","p75","min","max","Shapiro.Test","Lilliefors.Test")
      rownames(var.sum) <- ifelse(!is.na(var.names[i]),var.names[i],names(dta.select)[i])
      date.var.result <- rbind(date.var.result,var.sum)	

      if(cont.var=="mean"){
        cont.var2 <- "mean"
      }else if(cont.var=="median"){
        cont.var2 <- "median"
      }else if(cont.var=="auto"){
        if((!is.na(var.sum[,9]) && var.sum[,1]<50 && var.sum[,9]>=0.05) | (!is.na(var.sum[,10]) && var.sum[,1]>=50 && var.sum[,10]>=0.05)){
          cont.var2 <- "mean"
        }else{
          cont.var2 <- "median"
        }
      }else if(cont.var=="manual"){
        if(!is.na(manual.mean[i]) & manual.mean[i]==FALSE){ cont.var2 <- "median" }
        else if(!is.na(manual.mean[i]) & manual.mean[i]==TRUE){ cont.var2 <- "mean" }
        else{ cont.var2 <- "mean" }
      }

      if(cont.var2=="mean"){
        mean.var.sum <- as.Date(var.sum[,2],origin="1970-01-01")
        sd.var.sum <- format(round(var.sum[,3],digits=cont.digits),nsmall=cont.digits,trim=TRUE)	
        temp.summary <- cbind(var.sum[,1], paste(mean.var.sum, " (", sd.var.sum, ")",sep=""))
        rownames(temp.summary) <- paste(ifelse(!is.na(var.names[i]), var.names[i], names(dta.select)[i]) ,", mean (sd, months)",sep="")
        result.summary <- rbind(result.summary,temp.summary)
      } else if(cont.var2=="median"){
        median.var.sum <- as.Date(var.sum[,4],origin="1970-01-01")
        p25.varsum <- as.Date(var.sum[,5],origin="1970-01-01")
        p75.varsum <- as.Date(var.sum[,6],origin="1970-01-01")
        temp.summary <- cbind(var.sum[,1], paste(median.var.sum, " (", p25.varsum, ", ", p75.varsum, ")",sep=""))
        rownames(temp.summary) <- paste(ifelse(!is.na(var.names[i]), var.names[i], names(dta.select)[i]) ,", median (IQR)",sep="")
        result.summary <- rbind(result.summary,temp.summary)
      } 
    }
  }

  result.table <- list()
  if(length(result.detail)>0){
    result.table$Categorical.Variables <- noquote(format(do.call(rbind,result.detail),justify="centre"))
  }else{
    result.table$Categorical.Variables <- "No categorical variables defined"	
    colnames(result.summary) <- c("Total N","Statistic")
  }

  if(nrow(cont.var.result) >=1){
    cont.var.result <- noquote(cbind(format(round(cont.var.result[,1:8],digits=cont.digits),nsmall=cont.digits,trim=TRUE),
                       format(round(cont.var.result[,c(9,10)],digits=cont.digits+4),nsmall=cont.digits+4,trim=TRUE)))	
    result.table$Continuous.Variables <- noquote(format(cont.var.result,justify="centre"))
  }else{
    result.table$Continuous.Variables <- "No continuous variables defined"
  }

  if(nrow(date.var.result) >=1){
    date.var.result <- cbind(
      as.character(format(round(date.var.result[,1],digits=cont.digits),nsmall=cont.digits,trim=TRUE)),
      as.character(as.Date(date.var.result[,2],origin="1970-01-01")),
      as.character(format(round(date.var.result[,3],digits=cont.digits),nsmall=cont.digits,trim=TRUE)),
      as.character(as.Date(date.var.result[,4],origin="1970-01-01")),
      as.character(as.Date(date.var.result[,5],origin="1970-01-01")),
      as.character(as.Date(date.var.result[,6],origin="1970-01-01")),
      as.character(as.Date(date.var.result[,7],origin="1970-01-01")),
      as.character(as.Date(date.var.result[,8],origin="1970-01-01")),
      format(round(date.var.result[,9:10],digits=cont.digits+4),nsmall=cont.digits+4,trim=TRUE))
    
    colnames(date.var.result) <- c("Total N","mean","sd (months)","median","p25","p75","min","max","Shapiro.Test","Lilliefors.Test")
    result.table$Date.Variables <- noquote(format(date.var.result,justify="centre"))
  }else{
    result.table$Date.Variables <- "No date variables defined"
  }

  result.table$Summary <- noquote(format(result.summary,justify="centre"))

  cat("\n\n##################################################################\n\tTable 1\n")
  print(result.table$Summary)
  cat("\n##################################################################\n")
  
  if(!is.na(export.table)){
    write.table(result.table$Summary,file=paste(export.table,format(Sys.time(),"%Y_%m_%d"),".csv",sep=""),row.names=T, col.names=NA,sep=sep,dec=".") 
  }

  return(invisible(result.table))
}