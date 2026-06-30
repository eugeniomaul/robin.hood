t23 <- function(data=NA, y.var=NA, idvar=NA, mod.type = c("univar","multivar"), mod.func= c("lm","logistic","gee","cox"), model.family = c("gaussian","binomial"), cor="exchangeable", time.var=NULL, event.var=NULL, export.table=NA, sep = c(",",";"), auto.convert=TRUE, selected.vars=NULL, delete.miss.rows=FALSE, coeff.names=NA, coeff.digits=2, p.digits=3, trim.digits=FALSE, ...) {
  mod.type = match.arg(mod.type)
  mod.func = match.arg(mod.func)
  model.family = match.arg(model.family)
  sep = match.arg(sep)

  if(mod.func=="cox"){
    if(!is.null(selected.vars) & !is.na(idvar)){
      data <- subset(data,select=c(selected.vars,idvar, time.var,event.var))
    }else if(!is.null(selected.vars) & is.na(idvar)){
      data <- subset(data,select=c(selected.vars, time.var,event.var))
    }
  } else {
    if(!is.null(selected.vars) & !is.na(idvar)){
      data <- subset(data,select=c(selected.vars,y.var, idvar))
    }else if(!is.null(selected.vars) & is.na(idvar)){
      data <- subset(data,select=c(selected.vars,y.var))
    }
  }

  if(delete.miss.rows==TRUE){
    full.rows <- (rowSums(is.na(data))==0)
    data <- data[full.rows,]
  }

  if(mod.func!="cox"){
    temp.data <- data[!(names(data)==y.var)]
    y.varname <- y.var
    y.var <- data[(names(data)==y.var)]
    y.var <- y.var[,1]
    data <- temp.data
  }

  idvarname <- idvar
  if(!is.na(idvar)){
    temp.data <- data[!(names(data)==idvar)]
    idvar <- data[(names(data)==idvar)]
    idvar <- idvar[,1]
    data <- temp.data
  }

  if(mod.func=="cox"){
    temp.data <- data[!(names(data)==time.var)]
    time.var <- data[(names(data)==time.var)]
    time.var <- time.var[,1]
    data <- temp.data

    temp.data <- data[!(names(data)==event.var)]
    event.var <- data[(names(data)==event.var)]
    event.var <- event.var[,1]
    data <- temp.data
  }

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
        data[,i] <- as.factor((as.numeric(data[,i])))
      }
    }
  }

  dta.vartype <- lapply(data,class)
  dta.non.character <- as.vector(dta.vartype!="character")
  data <- data[,dta.non.character, drop=FALSE]
  varnames <- names(data)

  if(mod.type=="univar"){
    if(mod.func=="lm"){
      uni.models <- lapply(varnames,function(x){
        uni.formula <- as.formula(paste("y.var ~ ",x,sep=""))
        lm(uni.formula,data=data)
      })
    } else if(mod.func=="logistic"){
      uni.models <- lapply(varnames,function(x){
        uni.formula <- as.formula(paste("y.var ~ ",x,sep=""))
        glm(uni.formula,data=data,family="binomial")
      })
    } else if(mod.func=="gee"){
      uni.models <- lapply(varnames,function(x){
        valid.data <- !is.na(y.var) & !is.na(data[[x]]) & !is.na(idvarname)
        data2 <- data[valid.data, , drop=FALSE]
        data2$y.var2 <- y.var[valid.data]
        data2$idvar2 <- idvar[valid.data]
        data2 <- data2[order(data2$idvar2), ]
        uni.formula <- as.formula(paste("y.var2 ~ ",x,sep=""))
        fam <- if(model.family=="binomial") binomial else gaussian
        suppressMessages(geepack::geeglm(uni.formula, data=data2, family=fam, id=idvar2, corstr=cor))
      })
    } else if(mod.func=="cox"){
      if(is.na(idvarname)){
        uni.models <- lapply(varnames,function(x){
          valid.data <- !is.na(data[[x]]) & !is.na(time.var) & !is.na(event.var)
          data2 <- data[valid.data, , drop=FALSE]
          data2$survobj <- with(data2, Surv(time.var[valid.data], event.var[valid.data]))
          uni.formula <- as.formula(paste("survobj ~ ",x,sep=""))
          survival::coxph(uni.formula,data=data2)
        })
      } else {
        uni.models <- lapply(varnames,function(x){
          valid.data <- !is.na(data[[x]]) & !is.na(idvar) & !is.na(time.var) & !is.na(event.var)
          data2 <- data[valid.data, , drop=FALSE]
          data2$idvar2 <- idvar[valid.data]
          data2$survobj <- with(data2, Surv(time.var[valid.data], event.var[valid.data]))
          uni.formula <- as.formula(paste("survobj ~ ",x," + cluster(idvar2)",sep=""))
          survival::coxph(uni.formula,data=data2)
        })
      }
    }

    if(mod.func=="lm" | mod.func=="glm" | mod.func=="logistic"){
      uni.model.summary <- lapply(uni.models,function(x){
        cbind(summary(x)$coefficients,confint(x))
      })
      uni.coeff <- do.call(rbind,uni.model.summary)
      uni.coeff <- as.matrix(uni.coeff) # FIXED: Matrix enforcement
      uni.coeff <- uni.coeff[rownames(uni.coeff)!="(Intercept)", c(1,5,6,4), drop=FALSE]
      colnames(uni.coeff) <- c("Estimate","CI.95.Inf","CI.95.Sup","p.value")
    } else if(mod.func=="gee"){
      uni.model.summary <- lapply(uni.models,function(x){
        model.coeff <- as.matrix(summary(x)$coefficients)
        estimate <- model.coeff[,1]
        std.err <- model.coeff[,2]
        ci.95 <- std.err * qnorm(0.975)
        p.val <- model.coeff[,4]
        res <- matrix(c(estimate, estimate - ci.95, estimate + ci.95, p.val), ncol=4)
        rownames(res) <- rownames(model.coeff)
        return(res)
      })
      uni.coeff <- do.call(rbind,uni.model.summary)
      uni.coeff <- as.matrix(uni.coeff) # FIXED: Matrix enforcement
      uni.coeff <- uni.coeff[rownames(uni.coeff)!="(Intercept)", , drop=FALSE]
      colnames(uni.coeff) <- c("Estimate","CI.95.Inf","CI.95.Sup","p.value")	
    } else if(mod.func=="cox"){
      uni.model.summary <- lapply(uni.models,function(x){
        model.coeff <- cbind(summary(x)$conf.int[,1], summary(x)$conf.int[,3],summary(x)$conf.int[,4],summary(x)$coefficients[,ncol(summary(x)$coefficients)])
        rownames(model.coeff) <- rownames(summary(x)$conf.int)
        return(model.coeff)
      })
      uni.coeff <- do.call(rbind,uni.model.summary)
      uni.coeff <- as.matrix(uni.coeff) # FIXED: Matrix enforcement
      p.title <- ifelse(!is.na(idvarname), "robust.p.value", "p.value")
      colnames(uni.coeff) <- c("HR","CI.95.Inf","CI.95.Sup",p.title)
    }

    if(mod.func=="logistic" | model.family=="binomial"){
      uni.table.a <- format(round(exp(uni.coeff[,1:3, drop=FALSE]),digits=coeff.digits),nsmall=coeff.digits,trim=trim.digits)
      uni.table.b <- format(round(uni.coeff[,4],digits=p.digits),nsmall=p.digits,trim=trim.digits)
      uni.table <- cbind(paste(uni.table.a[,1]," (", uni.table.a[,2],", ", uni.table.a[,3],")",sep=""), uni.table.b)
      rownames(uni.table) <- rownames(uni.coeff)
      colnames(uni.table) <- c("OR (95% CI)","p value")	
    } else if((mod.func=="lm" | (mod.func=="gee" & model.family=="gaussian")) ){
      uni.table.a <- format(round(uni.coeff[,1:3, drop=FALSE],digits=coeff.digits),nsmall=coeff.digits,trim=trim.digits)
      uni.table.b <- format(round(uni.coeff[,4],digits=p.digits),nsmall=p.digits,trim=trim.digits)
      uni.table <- cbind(paste(uni.table.a[,1]," (", uni.table.a[,2],", ", uni.table.a[,3],")",sep=""), uni.table.b)
      rownames(uni.table) <- rownames(uni.coeff)
      colnames(uni.table) <- c("Beta (95% CI)","p value")	
    } else if(mod.func=="cox" ){
      uni.table.a <- format(round(uni.coeff[,1:3, drop=FALSE],digits=coeff.digits),nsmall=coeff.digits,trim=trim.digits)
      uni.table.b <- format(round(uni.coeff[,4],digits=p.digits),nsmall=p.digits,trim=trim.digits)
      uni.table <- cbind(paste(uni.table.a[,1]," (", uni.table.a[,2],", ", uni.table.a[,3],")",sep=""), uni.table.b)
      rownames(uni.table) <- rownames(uni.coeff)
      p.title <- ifelse(!is.na(idvarname), "robust.p.value", "p.value")
      colnames(uni.table) <- c("HR (95% CI)",p.title)	
    }

    uni.table <- noquote(uni.table)
    
    if(sum(!is.na(coeff.names))>=1){
      uni.table <- cbind(uni.table,row.names(uni.table))
      for(i in 1:nrow(uni.table)){
        if(!is.na(coeff.names[i])) row.names(uni.table)[i] <- coeff.names[i]
      }
    }

    print.title <- "Univariable Regression Results"
    cat("\n\n############################################################################################################")
    cat(paste("##  ", print.title,"\n\n",sep=""))
    print(uni.table)
    cat("\n############################################################################################################\n")
    
    univar <- list(uni.models = uni.models, uni.coeff = uni.coeff, uni.table = uni.table, 
                   coeff.digits = coeff.digits, p.digits = p.digits, trim.digits = trim.digits, 
                   mod.func = mod.func, model.family = model.family, mod.type = mod.type,
                   y.varname = ifelse(mod.func=="cox", "Events in time", y.varname))

    if(!is.na(export.table)){
      write.table(uni.table,file=paste(export.table,format(Sys.time(),"%Y_%m_%d"),".csv",sep=""),col.names=NA, row.names=TRUE,sep=sep,dec=".") 
    }
    return(invisible(univar))

  } else if(mod.type=="multivar"){
    
    if(mod.func=="lm"){
      multi.formula <- as.formula(paste("y.var ~ ",paste(varnames,collapse=" + "),sep=""))
      model <-  lm(multi.formula,data=data)
    } else if(mod.func=="logistic"){
      multi.formula <- as.formula(paste("y.var ~ ",paste(varnames,collapse=" + "),sep=""))
      model <-  glm(multi.formula,data=data,family="binomial")
    } else if(mod.func=="gee"){
      valid.data <- !is.na(y.var) & !is.na(idvar) & (rowSums(is.na(data))==0)
      data2 <- data[valid.data, , drop=FALSE]
      data2$y.var2 <- y.var[valid.data]
      data2$idvar2 <- idvar[valid.data]
      data2 <- data2[order(data2$idvar2), ]
      
      multi.formula <- as.formula(paste("y.var2 ~ ",paste(varnames,collapse=" + "),sep=""))
      fam <- if(model.family=="binomial") binomial else gaussian
      suppressMessages(model <- geepack::geeglm(multi.formula, data=data2, family=fam, id=idvar2, corstr=cor))
    } else if(mod.func=="cox"){
      if(is.na(idvarname)){
        valid.data <- !is.na(time.var) & !is.na(event.var) & (rowSums(is.na(data))==0)
        data2 <- data[valid.data, , drop=FALSE]
        data2$survobj <- with(data2, Surv(time.var[valid.data], event.var[valid.data]))
        multi.formula <- as.formula(paste("survobj ~ ",paste(varnames,collapse=" + "),sep=""))
        model <- survival::coxph(multi.formula,data=data2)
      } else {
        valid.data <- !is.na(time.var) & !is.na(event.var) & (rowSums(is.na(data))==0) & !is.na(idvar)
        data2 <- data[valid.data, , drop=FALSE]
        data2$idvar2 <- idvar[valid.data]
        data2$survobj <- with(data2, Surv(time.var[valid.data], event.var[valid.data]))
        multi.formula <- as.formula(paste("survobj ~ ",paste(varnames,collapse=" + ")," + cluster(idvar2)",sep=""))
        model <- survival::coxph(multi.formula,data=data2)
      }
    }

    if(mod.func=="lm" | mod.func=="glm" | mod.func=="logistic"){
      multi.coeff <- cbind(summary(model)$coefficients,confint(model))
      multi.coeff <- as.matrix(multi.coeff) # FIXED: Matrix enforcement
      multi.coeff <- multi.coeff[rownames(multi.coeff)!="(Intercept)", c(1,5,6,4), drop=FALSE]
      colnames(multi.coeff) <- c("Estimate","CI.95.Inf","CI.95.Sup","p.value")
    } else if(mod.func=="gee"){
      model.coeff <- as.matrix(summary(model)$coefficients)
      estimate <- model.coeff[,1]
      std.err <- model.coeff[,2]
      ci.95 <- std.err * qnorm(0.975)
      p.val <- model.coeff[,4]
      multi.coeff <- matrix(c(estimate, estimate - ci.95, estimate + ci.95, p.val), ncol=4)
      rownames(multi.coeff) <- rownames(model.coeff)
      multi.coeff <- multi.coeff[rownames(multi.coeff)!="(Intercept)", , drop=FALSE]
      colnames(multi.coeff) <- c("Estimate","CI.95.Inf","CI.95.Sup","p.value")	
    } else if(mod.func=="cox"){
      cox.s <- summary(model)
      multi.coeff <- cbind(cox.s$conf.int[,1], cox.s$conf.int[,3],cox.s$conf.int[,4],cox.s$coefficients[,ncol(cox.s$coefficients)])
      multi.coeff <- as.matrix(multi.coeff) # FIXED: Matrix enforcement
      rownames(multi.coeff) <- rownames(cox.s$conf.int)
      p.title <- ifelse(!is.na(idvarname), "robust.p.value", "p.value")
      colnames(multi.coeff) <- c("HR","CI.95.Inf","CI.95.Sup",p.title)
    }

    if(mod.func=="logistic" | model.family=="binomial"){
      multi.table.a <- format(round(exp(multi.coeff[,1:3, drop=FALSE]),digits=coeff.digits),nsmall=coeff.digits,trim=trim.digits)
      multi.table.b <- format(round(multi.coeff[,4],digits=p.digits),nsmall=p.digits,trim=trim.digits)
      multi.table <- cbind(paste(multi.table.a[,1]," (", multi.table.a[,2],", ", multi.table.a[,3],")",sep=""), multi.table.b)
      rownames(multi.table) <- rownames(multi.coeff)
      colnames(multi.table) <- c("OR (95% CI)","p value")	
    } else if((mod.func=="lm" | (mod.func=="gee" & model.family=="gaussian")) ){
      multi.table.a <- format(round(multi.coeff[,1:3, drop=FALSE],digits=coeff.digits),nsmall=coeff.digits,trim=trim.digits)
      multi.table.b <- format(round(multi.coeff[,4],digits=p.digits),nsmall=p.digits,trim=trim.digits)
      multi.table <- cbind(paste(multi.table.a[,1]," (", multi.table.a[,2],", ", multi.table.a[,3],")",sep=""), multi.table.b)
      rownames(multi.table) <- rownames(multi.coeff)
      colnames(multi.table) <- c("Beta (95% CI)","p value")	
    } else if(mod.func=="cox" ){
      multi.table.a <- format(round(multi.coeff[,1:3, drop=FALSE],digits=coeff.digits),nsmall=coeff.digits,trim=trim.digits)
      multi.table.b <- format(round(multi.coeff[,4],digits=p.digits),nsmall=p.digits,trim=trim.digits)
      multi.table <- cbind(paste(multi.table.a[,1]," (", multi.table.a[,2],", ", multi.table.a[,3],")",sep=""), multi.table.b)
      rownames(multi.table) <- rownames(multi.coeff)
      p.title <- ifelse(!is.na(idvarname), "robust.p.value", "p.value")
      colnames(multi.table) <- c("HR (95% CI)",p.title)	
    }

    multi.table <- noquote(multi.table)
    
    if(sum(!is.na(coeff.names))>=1){
      multi.table <- cbind(multi.table,row.names(multi.table))
      for(i in 1:nrow(multi.table)){
        if(!is.na(coeff.names[i])) row.names(multi.table)[i] <- coeff.names[i]
      }
    }

    print.title <- "Multivariable Regression Results"
    cat("\n\n############################################################################################################")
    cat(paste("##  ", print.title,"\n\n",sep=""))
    print(multi.table)
    cat("\n############################################################################################################\n")
    
    multivar <- list(multi.models = model, multi.coeff = multi.coeff, multi.table = multi.table, 
                     coeff.digits = coeff.digits, p.digits = p.digits, trim.digits = trim.digits, 
                     mod.func = mod.func, model.family = model.family, mod.type = mod.type,
                     y.varname = ifelse(mod.func=="cox", "Events in time", y.varname))

    if(!is.na(export.table)){
      write.table(multi.table,file=paste(export.table,format(Sys.time(),"%Y_%m_%d"),".csv",sep=""),col.names=NA,row.names=TRUE,sep=sep,dec=".") 
    }
    return(invisible(multivar))
  }
}