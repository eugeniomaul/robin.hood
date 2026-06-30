t1.by <- function(data=NA, selected.vars=NULL, group.var=NA, miss.group=FALSE, var.names=NA, table.na=c("ifany","no","always"), cont.var=c("mean","median","auto","manual"), manual.mean=NA, cont.digits=1, categ.test=c("chi2","fisher"), auto.convert=TRUE, max.levels=4, export.table=NA, sep = ","){

  cont.var = match.arg(cont.var)
  table.na = match.arg(table.na)
  categ.test = match.arg(categ.test)
  
  data <- as.data.frame(data) 

  if(!is.null(selected.vars)){
    data <- subset(data,select=c(selected.vars, group.var))
  }

  temp.data<-data[!(names(data)==group.var)]
  group.var<-data[(names(data)==group.var)]
  group.var<-group.var[,1]
  data<-temp.data

  is.date <- function(x) inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))

  if((is.factor(group.var) | is.character(group.var)) & miss.group==FALSE){
    group.var<-as.factor(as.character(group.var))
  }else if((is.factor(group.var) | is.character(group.var)) & miss.group==TRUE){
    group.var<-as.character(group.var)
    group.var[is.na(group.var)]<-"missing"
    group.var<-as.factor(group.var)
  }else if( (is.numeric(group.var) | is.logical(group.var) | is.date(group.var)) & miss.group==FALSE){
    group.var<-as.factor(group.var)
  }else if( (is.numeric(group.var) | is.logical(group.var) | is.date(group.var)) & miss.group==TRUE){
    group.var<-as.character(group.var)
    group.var[is.na(group.var)]<-"missing"
    group.var<-as.factor(group.var)
  }

  if(length(levels(group.var))>max.levels){
    stop(paste("Maximum number of levels is ",max.levels, " . Set the max.levels argument to a higher number or choose a different grouping variable", sep=""))
  }else if(length(levels(group.var))>2){
    multiple.groups<-TRUE	
  }else if(length(levels(group.var))<=2){
    multiple.groups<-FALSE	
  }

  if(table.na=="ifany") table.na <- "always"

  group.levels<-levels(group.var)
  n.levels<-length(group.levels)

  if(auto.convert==TRUE){
    for(i in 1:ncol(data)){
      if(is.character(data[,i])==TRUE){
        if(length(levels(as.factor(data[,i]))) <10){
          data[,i]<-as.factor(data[,i])
        }		
      } else if(is.numeric(data[,i])==TRUE) {
        next	
      } else if(is.factor(data[,i])==TRUE){
        data[,i]<-as.factor(as.character(data[,i]))
      }else if(is.logical(data[,i])==TRUE){
        data[,i]<-as.factor( (as.numeric(data[,i])) )
      }
    }
  }

  dta.vartype<-lapply(data,class)
  dta.non.character<-as.vector(dta.vartype!="character")
  data <- data[,dta.non.character, drop=FALSE]

  result.detail<-list()
  result.summary<-matrix(nrow=0,ncol=2+n.levels)
  colnames(result.summary)<-c("Total N", group.levels,"p value")
  cont.var.result<-list()
  date.var.result <- list()
  warnings.hood <-matrix(nrow=0,ncol=2)
  
  for(i in 1:n.levels){
    cont.var.result[[i]]<-matrix(nrow=0,ncol=10)
    date.var.result[[i]]<-matrix(nrow=0,ncol=10)
  }

  for(i in 1:ncol(data)){
    if(is.factor(data[,i])==TRUE){
      temp.table.n<- table(data[,i],group.var,useNA=table.na)
      temp.table.dim <- dim(temp.table.n)[1] 
      if(temp.table.dim > 1){
        temp.table.n<-temp.table.n[,!is.na(colnames(temp.table.n)), drop=FALSE]
      }
      table.total.n<-sum(temp.table.n)
      temp.table.p<-format(round(100*(prop.table(temp.table.n,2)),digits=1),nsmall=1,trim=TRUE)
      table.names<-replace(rownames(temp.table.n),is.na(rownames(temp.table.n)),"Missing")
      table.cells<- paste(temp.table.n, " (",temp.table.p,"%)",sep="")
      table.dim <- length(dimnames(temp.table.n)[[2]])

      if(table.dim>=2 & temp.table.dim >1){
        if(categ.test=="chi2"){
          test_res <- tryCatch(suppressWarnings(chisq.test(temp.table.n)), error = function(e) NULL)
          
          if (is.null(test_res) || any(test_res$expected < 5)) {
            test_res <- tryCatch(
              fisher.test(temp.table.n, simulate.p.value = TRUE, B = 2000),
              error = function(e) suppressWarnings(chisq.test(temp.table.n))
            )
            temp.warnings <- c(names(data)[i], "Expected counts < 5; used Fisher's exact with simulated p-value")
            warnings.hood <- rbind(warnings.hood, temp.warnings)
          }
          
          table.p.val <- format(round(test_res$p.value, 4), nsmall = 4, trim = TRUE)
          if(grepl("Fisher", test_res$method)) table.p.val <- paste0(table.p.val, " (Fisher)")
          
        } else if(categ.test=="fisher"){
          error.test <- inherits(try(fisher.test(temp.table.n), silent=TRUE), "try-error")
          if(error.test==FALSE){
            test_res <- fisher.test(temp.table.n)
          } else {
            test_res <- suppressWarnings(chisq.test(temp.table.n))
            temp.warnings <- c(names(data)[i],"fisher.test failed, chi2 used instead")	
            warnings.hood <- rbind(warnings.hood, temp.warnings)
          }
          
          table.p.val <- format(round(test_res$p.value, 4), nsmall = 4, trim = TRUE)
          if(grepl("Fisher", test_res$method)) table.p.val <- paste0(table.p.val, " (Fisher)")
        }
      } else{
        table.p.val <- NA
      }

      temp.col<-rep("",times=nrow(temp.table.n))
      table.result<-matrix(c(temp.col,table.cells,temp.col),ncol=n.levels+2,nrow=nrow(temp.table.n))
      table.result<-rbind(c(rep("",times=n.levels+1),table.p.val),table.result)
      table.result[1,1]<-table.total.n
      colnames(table.result)<-c("Total N", group.levels,"p value")
      var.name<- paste( ifelse(!is.na(var.names[i]), var.names[i], names(data)[i]) ,", n (%)",sep="")
      rownames(table.result)<-c(var.name,paste("   ",table.names,sep=""))
      result.detail[[length(result.detail)+1]] <- table.result
      result.summary <- rbind(result.summary,table.result)
      
    } else if(is.numeric(data[,i])==TRUE){
      x<-data[,i]
      for(j in 1:n.levels){
        x2<-x[!is.na(group.var) & group.levels[j]==group.var]
        temp.x <- table(x2,useNA="no")
        temp.x <- length(names(temp.x))
        if(temp.x > 1){
          if(sum(!is.na(x2)) > 2 & sum(!is.na(x2)) < 5000){
            shapiro.test2 <- tryCatch(suppressWarnings(round(shapiro.test(x2)$p.value,4)), error=function(e) NA)
          } else{
            shapiro.test2 <- NA
          }
          if(sum(!is.na(x2)) > 4){
            lillie.test2 <- tryCatch(suppressWarnings(round(nortest::lillie.test(x2)$p.value, 4)), error=function(e) NA)
          } else {
            lillie.test2 <- NA
          }
        } else{
          shapiro.test2 <- NA
          lillie.test2 <- NA
        }

        var.sum<-cbind(sum(!is.na(x2)), mean(x2,na.rm=TRUE), sd(x2,na.rm=TRUE), t(quantile(x2,c(0.5,0.25,0.75),na.rm=T)), min(x2,na.rm=T), max(x2,na.rm=T), shapiro.test2, lillie.test2)
        colnames(var.sum)<-c("Total N","mean","sd","median","p25","p75","min","max","Shapiro.Test","Lilliefors.Test")
        var.name<- ifelse(!is.na(var.names[i]),var.names[i],names(data)[i])
        rownames(var.sum)<- var.name
        cont.var.result[[j]]<-rbind(cont.var.result[[j]],var.sum)	
      }

      if(cont.var=="mean"){
        cont.var2<-"mean"
      }else if(cont.var=="median"){
        cont.var2<-"median"
      }else if(cont.var=="auto"){
        test.type<-c()
        for(k in 1:n.levels){
          x2<-x[!is.na(group.var) & group.levels[k]==group.var]
          x2.n<-sum(!is.na(x2))
          temp.x <- length(names(table(x2,useNA="no")))
          if(temp.x > 1){
            x2.shapiro <- if(x2.n > 2 & x2.n < 5000) tryCatch(suppressWarnings(shapiro.test(x2)$p.value), error=function(e) NA) else NA
            x2.lillie <- if(x2.n > 4) tryCatch(suppressWarnings(nortest::lillie.test(x2)$p.value), error=function(e) NA) else NA
          } else {
            x2.shapiro <- NA
            x2.lillie <- NA
          }
          
          if(!is.na(x2.shapiro) && x2.n<50 && x2.shapiro>=0.05) { test.type<-c(test.type,"mean") }
          else if(!is.na(x2.lillie) && x2.n>=50 && x2.lillie>=0.05) { test.type<-c(test.type,"mean") }
          else { test.type<-c(test.type,"median") }
        }
        cont.var2<-	ifelse(sum(test.type=="median",na.rm=T)>=1,"median","mean")
      }else if(cont.var=="manual"){
        if(!is.na(manual.mean[i]) & manual.mean[i]==FALSE){ cont.var2<-"median" }
        else if(!is.na(manual.mean[i]) & manual.mean[i]==TRUE){ cont.var2<-"mean" }
        else{ cont.var2<-"mean" }
      }

      temp.result.summary<-matrix(nrow=0,ncol=2+n.levels)
      use.data<- (!is.na(x) & !is.na(group.var))
      temp.total.obs<-sum(use.data)

      temp1 <- as.factor(as.character(group.var[use.data]))
      if(length(levels(temp1)) < 2 ){
        temp.result.p <- NA
      }else{
        if(cont.var2=="mean"){
          if(multiple.groups==FALSE){
            error.test <- inherits(try(t.test(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(t.test(x[use.data]~group.var[use.data])$p.value,4)
            }else{
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"t.test failed: insufficient observations per group"))
              temp.result.p <- NA
            }
          } else if(multiple.groups==TRUE){
            error.test <- inherits(try(aov(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(summary(aov(x[use.data]~group.var[use.data]))[[1]]$`Pr(>F)`[1],4)
            } else {
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"anova failed: insufficient observations per group"))
              temp.result.p <- NA				
            }
          }
        }else if(cont.var2=="median"){
          if(multiple.groups==FALSE){
            error.test <- inherits(try(wilcox.test(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(suppressWarnings(wilcox.test(x[use.data]~group.var[use.data])$p.value),4)
            } else {
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"wilcoxon.test failed: insufficient observations per group"))
              temp.result.p <- NA	
            }
          } else if(multiple.groups==TRUE){
            error.test <- inherits(try(kruskal.test(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(suppressWarnings(kruskal.test(x[use.data]~group.var[use.data])$p.value),4)
            } else {
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"kruskal.wallis failed: insufficient observations per group"))
              temp.result.p <- NA			
            }
          }
        }
      }
      
      temp.summary<-c()
      for(k in 1:n.levels){
        x2<-x[!is.na(x) & !is.na(group.var) & group.var==group.levels[k]]
        if(cont.var2=="mean"){
          temp.summary <- c(temp.summary, paste(format(round(mean(x2,na.rm=T),digits=cont.digits),nsmall=cont.digits,trim=TRUE), " (", format(round(sd(x2,na.rm=T),digits=cont.digits),nsmall=cont.digits,trim=TRUE), ")",sep=""))
        } else if(cont.var2=="median"){
          temp.summary <- c(temp.summary, paste(format(round(quantile(x2,0.5,na.rm=T),digits=cont.digits),nsmall=cont.digits,trim=TRUE)," (",format(round(quantile(x2,0.25,na.rm=T),digits=cont.digits),nsmall=cont.digits,trim=TRUE),", ", format(round(quantile(x2,0.75,na.rm=T),digits=cont.digits),nsmall=cont.digits,trim=TRUE),")",sep="")) 
        } 
      }

      temp.summary<-matrix(c(temp.total.obs,temp.summary,temp.result.p),nrow=1)
      rownames(temp.summary)<- paste( ifelse(!is.na(var.names[i]), var.names[i], names(data)[i]), ifelse(cont.var2=="mean", ", mean (sd)", ", median (IQR)"), sep="")
      result.summary<- rbind(result.summary,temp.summary)
      
    } else if(is.date(data[,i])==TRUE){
      x<-as.numeric(data[,i])
      for(j in 1:n.levels){
        x2<-x[!is.na(group.var) & group.levels[j]==group.var]
        temp.x <- table(x2,useNA="no")
        temp.x <- length(names(temp.x))
        if(temp.x > 1){
          if(sum(!is.na(x2)) > 2 & sum(!is.na(x2)) < 5000){
            shapiro.test2 <- tryCatch(suppressWarnings(round(shapiro.test(x2)$p.value,4)), error=function(e) NA)
          } else{
            shapiro.test2 <- NA
          }
          if(sum(!is.na(x2)) > 4){
            lillie.test2 <- tryCatch(suppressWarnings(round(nortest::lillie.test(x2)$p.value, 4)), error=function(e) NA)
          } else {
            lillie.test2 <- NA
          }
        } else{
          shapiro.test2 <- NA
          lillie.test2 <- NA
        }

        var.sum<-cbind(sum(!is.na(x2)), mean(x2,na.rm=TRUE), (sd(x2,na.rm=TRUE)/30), t(quantile(x2,c(0.5,0.25,0.75),na.rm=T)), min(x2,na.rm=T), max(x2,na.rm=T), shapiro.test2, lillie.test2)
        colnames(var.sum)<-c("Total N","mean","sd (months)","median","p25","p75","min","max","Shapiro.Test","Lilliefors.Test")
        var.name<- ifelse(!is.na(var.names[i]),var.names[i],names(data)[i])
        rownames(var.sum)<- var.name
        date.var.result[[j]]<-rbind(date.var.result[[j]],var.sum)	
      }

      if(cont.var=="mean"){
        cont.var2<-"mean"
      }else if(cont.var=="median"){
        cont.var2<-"median"
      }else if(cont.var=="auto"){
        test.type<-c()
        for(k in 1:n.levels){
          x2<-x[!is.na(group.var) & group.levels[k]==group.var]
          x2.n<-sum(!is.na(x2))
          temp.x <- length(names(table(x2,useNA="no")))
          if(temp.x > 1){
            x2.shapiro <- if(x2.n > 2 & x2.n < 5000) tryCatch(suppressWarnings(shapiro.test(x2)$p.value), error=function(e) NA) else NA
            x2.lillie <- if(x2.n > 4) tryCatch(suppressWarnings(nortest::lillie.test(x2)$p.value), error=function(e) NA) else NA
          } else {
            x2.shapiro <- NA
            x2.lillie <- NA
          }
          
          if(!is.na(x2.shapiro) && x2.n<50 && x2.shapiro>=0.05) { test.type<-c(test.type,"mean") }
          else if(!is.na(x2.lillie) && x2.n>=50 && x2.lillie>=0.05) { test.type<-c(test.type,"mean") }
          else { test.type<-c(test.type,"median") }
        }
        cont.var2<-	ifelse(sum(test.type=="median",na.rm=T)>=1,"median","mean")
      }else if(cont.var=="manual"){
        if(!is.na(manual.mean[i]) & manual.mean[i]==FALSE){ cont.var2<-"median" }
        else if(!is.na(manual.mean[i]) & manual.mean[i]==TRUE){ cont.var2<-"mean" }
        else{ cont.var2<-"mean" }
      }

      temp.result.summary<-matrix(nrow=0,ncol=2+n.levels)
      use.data<- (!is.na(x) & !is.na(group.var))
      temp.total.obs<-sum(use.data)

      temp1 <- as.factor(as.character(group.var[use.data]))
      if(length(levels(temp1)) < 2 ){
        temp.result.p <- NA
      }else{
        if(cont.var2=="mean"){
          if(multiple.groups==FALSE){
            error.test <- inherits(try(t.test(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(t.test(x[use.data]~group.var[use.data])$p.value,4)
            }else{
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"t.test failed: insufficient observations per group"))
              temp.result.p <- NA
            }
          } else if(multiple.groups==TRUE){
            error.test <- inherits(try(aov(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(summary(aov(x[use.data]~group.var[use.data]))[[1]]$`Pr(>F)`[1],4)
            } else {
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"anova failed: insufficient observations per group"))
              temp.result.p <- NA				
            }
          }
        }else if(cont.var2=="median"){
          if(multiple.groups==FALSE){
            error.test <- inherits(try(wilcox.test(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(suppressWarnings(wilcox.test(x[use.data]~group.var[use.data])$p.value),4)
            } else {
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"wilcoxon.test failed: insufficient observations per group"))
              temp.result.p <- NA	
            }
          } else if(multiple.groups==TRUE){
            error.test <- inherits(try(kruskal.test(x[use.data]~group.var[use.data]), silent=TRUE), "try-error")
            if(error.test==FALSE){
              temp.result.p <- round(suppressWarnings(kruskal.test(x[use.data]~group.var[use.data])$p.value),4)
            } else {
              warnings.hood <- rbind(warnings.hood, c(names(data)[i],"kruskal.wallis failed: insufficient observations per group"))
              temp.result.p <- NA			
            }
          }
        }
      }
      
      temp.summary<-c()
      for(k in 1:n.levels){
        x2<-x[!is.na(x) & !is.na(group.var) & group.var==group.levels[k]]
        if(cont.var2=="mean"){
          origin.x <- "1970-01-01"
          mean.var.sum <- as.Date(mean(x2,na.rm=T), origin=origin.x)
          sd.var.sum <- format(round(sd((x2/30),na.rm=T),digits=cont.digits),nsmall=cont.digits,trim=TRUE) 
          temp.summary <- c(temp.summary, paste(mean.var.sum, " (", sd.var.sum , ")",sep=""))
        } else if(cont.var2=="median"){
          origin.x <- "1970-01-01"
          median.var.sum <- as.Date(quantile(x2,0.5,na.rm=T), origin=origin.x)
          p25.varsum <- as.Date(quantile(x2,0.25,na.rm=T), origin=origin.x)
          p75.varsum <- as.Date(quantile(x2,0.75,na.rm=T), origin=origin.x)
          temp.summary <- c(temp.summary, paste(median.var.sum," (",p25.varsum,", ",p75.varsum ,")",sep="")) 
        } 
      }

      temp.summary<-matrix(c(temp.total.obs,temp.summary,temp.result.p),nrow=1)
      rownames(temp.summary)<- paste( ifelse(!is.na(var.names[i]), var.names[i], names(data)[i]), ifelse(cont.var2=="mean", ", mean (sd, months)", ", median (IQR)"), sep="")
      result.summary<- rbind(result.summary,temp.summary)
    } 
  } 

  result.table<-list()
  if(length(result.detail)>0){
    result.table$Categorical.Variables<-noquote(format(do.call(rbind,result.detail),justify="centre"))
  }else{
    result.table$Categorical.Variables<-"No categorical variables defined"	
  }

  if(length(cont.var.result) >=1){
    for(i in 1:length(cont.var.result)){
      names(cont.var.result)[i]<-paste("Group = ",group.levels[i],sep="")
      cont.var.result[[i]]<-noquote(cbind(format(round(cont.var.result[[i]][,1:8],digits=cont.digits),nsmall=cont.digits,trim=TRUE), format(round(cont.var.result[[i]][,c(9,10)],digits=cont.digits+4),nsmall=cont.digits+4,trim=TRUE)))
      result.table$Continuous.Variables[[i]]<-noquote(format(cont.var.result[[i]],justify="centre"))
    }
  }else{
    result.table$Continuous.Variables <- "No continuous variables defined"
  }

  if(length(date.var.result) >=1){
    for(i in 1:length(date.var.result)){
      names(date.var.result)[i]<-paste("Group = ",group.levels[i],sep="")
      date.var.result.z <- date.var.result[[i]]
      part1 <- as.character(format(round(date.var.result.z[,1],digits=cont.digits),nsmall=cont.digits,trim=TRUE))
      part2 <- as.character(as.Date(date.var.result.z[,2],origin="1970-01-01"))
      part3 <- as.character(format(round(date.var.result.z[,3],digits=cont.digits),nsmall=cont.digits,trim=TRUE))
      part4.1 <- as.character(as.Date(date.var.result.z[,4],origin="1970-01-01"))
      part4.2 <- as.character(as.Date(date.var.result.z[,5],origin="1970-01-01"))
      part4.3 <- as.character(as.Date(date.var.result.z[,6],origin="1970-01-01"))
      part4.4 <- as.character(as.Date(date.var.result.z[,7],origin="1970-01-01"))
      part4.5 <- as.character(as.Date(date.var.result.z[,8],origin="1970-01-01"))
      part5 <-  format(round(date.var.result.z[,9:10],digits=cont.digits+4),nsmall=cont.digits+4,trim=TRUE)
      date.var.result.z <- cbind(part1,part2,part3, part4.1, part4.2, part4.3, part4.4, part4.5, part5)
      colnames(date.var.result.z)<-c("Total N","mean","sd (months)","median","p25","p75","min","max","Shapiro.Test","Lilliefors.Test")
      date.var.result[[i]] <- noquote(format(date.var.result.z,justify="centre"))
    }
    result.table$Date.Variables<-date.var.result
  }else{
    result.table$Date.Variables <- "No date variables defined"
  }

  result.table$Summary<-noquote(format(result.summary,justify="centre"))
  result.table$warnings <- if(nrow(warnings.hood)>=1) noquote(format(warnings.hood,justify="centre")) else "No relevant warnings generated"

  cat("\n\n##################################################################\n\tTable 1: Groupwise Comparisons\n")
  print(result.table$Summary)
  cat("\n##################################################################\n")
      
  if(!is.na(export.table)){
    write.table(result.table$Summary,file=paste(export.table,format(Sys.time(),"%Y_%m_%d"),".csv",sep=""),row.names=T, col.names=NA, sep=sep,dec=".") 
  }
  return(invisible(result.table))
}