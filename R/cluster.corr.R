##################################################################
# Function cluster.corr Begins Here (This is Rosner paper's function)

cluster.corr <- function(data, id.var=NA, rep.var=NA, x.var=NA, y.var=NA, covar.list=NA,n.reps=2, n.digits=2){
  if(is.na(x.var) | is.na(y.var) | is.na(id.var) | is.na(rep.var)){
    print("You must specify at least data (no quotes), x.var, y.var, id.var, rep.var (latter 3 with quotes)")
  }else{

    dta <- data

    #################################################################
    # change the name for x.var, y.var, id.var
    x.var.name <- x.var
    y.var.name <- y.var
    id.var.name <- id.var
    rep.var.name <- rep.var
    names(dta)[names(dta)==x.var.name] <- "xx"
    names(dta)[names(dta)==y.var.name] <- "yy"
    names(dta)[names(dta)==id.var.name] <- "id"
    names(dta)[names(dta)==rep.var.name] <- "rep"

    #################################################################
    # keep only relevant variables
    if(sum(!is.na(covar.list))>=1){
      selected.vars <- c("xx","yy","id","rep", covar.list)
    }else{
      selected.vars <- c("xx","yy","id","rep")
    }
    dta <- subset(dta, select=selected.vars)

    #################################################################
    # keep only complete cases
    dta <- dta[complete.cases(dta),]

    #################################################################
    # transform the rep variable to factor then numeric
    dta$rep.text <- dta$rep
    dta$rep <- as.numeric(as.factor(dta$rep))

    #################################################################
    # keep only subjects with data for both subunits
    dta2 <- dta
    dta2$n.eyes <- 1
    dta2 <- aggregate(n.eyes ~ id, data=dta2, sum, na.rm=T)
    dta2 <- subset(dta2, n.eyes==n.reps)
    dta2 <- merge(dta,dta2)
    dta3 <- aggregate(xx ~ id, data=dta2, sum, na.rm=T)
    names(dta3) <- c("id","sum.x")
    dta3 <- dta3[order(dta3$id),]
    dta3$subj <- 1:nrow(dta3)
    dta2 <- merge(dta2,dta3)
    dta2 <- dta2[order(dta2$subj,dta2$rep),]

    #################################################################
    # save a copy of dta for fresh use in spearman calculations
    dta <- dta2

    #################################################################
    # generate the database with final variables
    dta2$xvar <- dta2$xx
    dta2$yvar <- dta2$yy
    dta2$x11 <- dta2$xx
    dta2$x12 <- dta2$sum.x - dta2$xx

    #################################################################
    # save the mean values for xvar and yvar
    mean.xvar <- mean(dta2$xvar)
    mean.yvar <- mean(dta2$yvar)

    ###########################################################################
    # replace xvar, yvar, x11, x12 only in case of covariate specification
    if(sum(!is.na(covar.list))>=1){
      # calculate the residuals using geepack
      multi.formula <- as.formula(paste("xvar ~ ",paste(covar.list,collapse=" + "),sep=""))
      invisible(capture.output(m2 <- suppressMessages(geepack::geeglm(multi.formula, id=subj , corstr="exchangeable", data=dta2))))
      dta2$x.resid <- as.numeric(m2$residuals)

      multi.formula <- as.formula(paste("yvar ~ ",paste(covar.list,collapse=" + "),sep=""))
      invisible(capture.output(m2 <- suppressMessages(geepack::geeglm(multi.formula, id=subj , corstr="exchangeable", data=dta2))))
      dta2$y.resid <- as.numeric(m2$residuals)

      # calculate the adjusted values for x and y variables
      dta2$xvar <- dta2$x.resid + mean.xvar
      dta2$yvar <- dta2$y.resid + mean.yvar

      dta3 <- dta2
      dta3 <- aggregate(xvar ~ id, data=dta2, sum, na.rm=T)
      names(dta3) <- c("id","sum.xx")

      dta2 <- merge(dta2,dta3)
      dta2$sum.x <- dta2$sum.xx
      dta2$x11 <- dta2$xvar
      dta2$x12 <- dta2$sum.x - dta2$xvar

      mean.xvar <- mean(dta2$xvar)
      mean.yvar <- mean(dta2$yvar)
    }

    ###########################################
    # x var
    m <- lme4::lmer(xvar ~ 1 + (1 | subj), data=dta2)
    xx <- as.data.frame(lme4::VarCorr(m))
    r11.x <- sum(xx$vcov)
    r12.x <- xx$vcov[1]
    sigmasq.x <- r11.x
    rho.x <- r12.x/r11.x

    ###########################################
    # y var
    m <- lme4::lmer(yvar ~ 1 + (1 | subj), data=dta2)
    xx <- as.data.frame(lme4::VarCorr(m))
    r11.y <- sum(xx$vcov)
    r12.y <- xx$vcov[1]
    sigmasq.y <- r11.y
    rho.y <- r12.y/r11.y

    ############################################
    # create uij and vij variables
    k=n.reps

    dta2$uij <- (sqrt(sigmasq.y)/sqrt(sigmasq.x))*(dta2$x11*(1+(k-2)*rho.x)-dta2$x12*rho.x)/
            ((1+(k-1)*rho.x)*(1-rho.x))
    dta2$vij=(sqrt(sigmasq.y)/sqrt(sigmasq.x))*(dta2$x12-(k-1)*dta2$x11*rho.x)/
           ((1+(k-1)*rho.x)*(1-rho.x))

    #############################################
    # run mixed model
    m <- lmerTest::lmer(yvar ~ uij + vij + (1 | subj), data=dta2)
    m2 <- summary(m)$coefficients

    ###########################################################################
    # use the mixed model results to calculate intra and interclass
    # correlation coefficients
    res <- list()
    res$beta1 <- m2[rownames(m2)=="uij", colnames(m2)=="Estimate" ]
    res$beta2 <- m2[rownames(m2)=="vij", colnames(m2)=="Estimate" ]
    res$se1 <- m2[rownames(m2)=="uij", colnames(m2)=="Std. Error" ]
    res$se2 <- m2[rownames(m2)=="vij", colnames(m2)=="Std. Error" ]
    res$p1 <- m2[rownames(m2)=="uij", colnames(m2)=="Pr(>|t|)" ]
    res$p2 <- m2[rownames(m2)=="vij", colnames(m2)=="Pr(>|t|)" ]

    res$z1 <- 0.5*log((1+res$beta1)/(1-res$beta1))
    res$z2 <-  0.5*log((1+res$beta2)/(1-res$beta2))
    res$var.z1=(1/(1-res$beta1**2)**2)*res$se1**2
    res$se.z1=sqrt(res$var.z1)
    res$ci1.z1=res$z1-1.96*res$se.z1
    res$ci2.z1=res$z1+1.96*res$se.z1
    res$ci1.beta1=(exp(2*res$ci1.z1)-1)/(exp(2*res$ci1.z1)+1)
    res$ci2.beta1=(exp(2*res$ci2.z1)-1)/(exp(2*res$ci2.z1)+1)
    res$var.z2=(1/(1-res$beta2**2)**2)*res$se2**2
    res$se.z2=sqrt(res$var.z2)
    res$ci1.z2=res$z2-1.96*res$se.z2
    res$ci2.z2=res$z2+1.96*res$se.z2
    res$ci1.beta2=(exp(2*res$ci1.z2)-1)/(exp(2*res$ci1.z2)+1)
    res$ci2.beta2=(exp(2*res$ci2.z2)-1)/(exp(2*res$ci2.z2)+1)

    ###########################################################################
    # create the pearson correlation results matrix
    sample.size <- length(unique(dta2$id))
    number.of.replicates <- max(dta2$rep)
    icc.xx <- round(rho.x,n.digits)
    sigma.sq.x <- round(sigmasq.x,n.digits)
    icc.yy <- round(rho.y,n.digits)
    sigma.sq.y <- round(sigmasq.y,n.digits)
    cross.correlation <- paste(round(res$beta2,n.digits), " (", round(res$ci1.beta2,n.digits), ", ", round(res$ci2.beta2,n.digits), "), p.val=", round(res$p2,n.digits+2), sep="")
    pearson.correlation <- paste(round(res$beta1,n.digits), " (", round(res$ci1.beta1,n.digits), ", ", round(res$ci2.beta1,n.digits), "), p.val=", round(res$p1,n.digits+2), sep="")
    covar.list2 <- ifelse(sum(!is.na(covar.list))<1, "No-Adjustment", paste(covar.list,collapse=" + "))
    
    pearson.result <- matrix(nrow=0, ncol=1)
    pearson.result <- rbind(pearson.result, sample.size, number.of.replicates, id.var.name, rep.var.name, x.var.name, icc.xx, sigma.sq.x, y.var.name, icc.yy, sigma.sq.y, cross.correlation, pearson.correlation, covar.list2)
    rownames(pearson.result) <- c("Sample Size", "Number of Replicates", "ID Variable Name", "Repeated Variable Name", "X Variable Name", paste("Intraclass Correlation Coeff X (",x.var.name,")",sep=""), paste("Sigma Squared X (",x.var.name,")",sep=""), "Y Variable Name", paste("Intraclass Correlation Coeff Y (",y.var.name,")",sep=""), paste("Sigma Squared Y (",y.var.name,")",sep=""), "Cross Correlation", "Pearson Correlation", "Covariates Adjusted For")
    colnames(pearson.result) <- "Result"
    pearson.result <- noquote(pearson.result)

    #################################################################
    # spearman calculations begin here
    dta2 <- dta
    dta2$xvar <- dta2$xx
    dta2$yvar <- dta2$yy
    dta2$x11 <- dta2$xx
    dta2$x12 <- dta2$sum.x - dta2$xx

    mean.xvar <- mean(dta2$xvar)
    mean.yvar <- mean(dta2$yvar)

    #################################################################
    # create the ranks for x and y within each subunit (rep)
    dta2 <- dta2[order(dta2$rep,dta2$xx),] 
    bbbb <- unlist(by(dta2$xx,dta2$rep,rank),use.names=FALSE)
    dta2$rx <- bbbb

    dta2 <- dta2[order(dta2$rep,dta2$yy),] 
    bbbb <- unlist(by(dta2$yy,dta2$rep,rank),use.names=FALSE)
    dta2$ry <- bbbb

    dta2 <- dta2[order(dta2$rx),]

    #################################################################
    # probit transformation (Replacing VGAM::probit with qnorm)
    yy.n <- length(dta2$yy[dta2$rep==1])
    xx.n <- length(dta2$xx[dta2$rep==1])
    dta2$yyprobt = qnorm(dta2$ry/(yy.n+1))
    dta2$xxprobt = qnorm(dta2$rx/(xx.n+1))
    dta2 <- dta2[order(dta2$id),]

    #################################################################
    dta3 <- aggregate( xxprobt ~ id, data=dta2, sum)
    names(dta3) <- c("id","sumx.probt")
    dta2 <- merge(dta2,dta3)

    dta2$xvar <- dta2$xxprobt
    dta2$yvar <- dta2$yyprobt
    dta2$x11 <- dta2$xxprobt
    dta2$x12 <- dta2$sumx.probt - dta2$xxprobt

    if(sum(!is.na(covar.list))>=1){
      multi.formula<-as.formula(paste("xvar ~ ",paste(covar.list,collapse=" + "), " + (1 | subj) ",sep=""))
      m <- lme4::lmer(multi.formula, data=dta2)
      xx <- as.data.frame(lme4::VarCorr(m))
      r11.x <- sum(xx$vcov)
      r12.x <- xx$vcov[1]
      sigmasq.x <- r11.x
      rho.x <- r12.x/r11.x

      multi.formula<-as.formula(paste("yvar ~ ",paste(covar.list,collapse=" + "), " + (1 | subj) ",sep=""))
      m <- lme4::lmer(multi.formula, data=dta2)
      xx <- as.data.frame(lme4::VarCorr(m))
      r11.y <- sum(xx$vcov)
      r12.y <- xx$vcov[1]
      sigmasq.y <- r11.y
      rho.y <- r12.y/r11.y
    }else{
      m <- lme4::lmer(xvar ~ 1 + (1 | subj), data=dta2)
      xx <- as.data.frame(lme4::VarCorr(m))
      r11.x <- sum(xx$vcov)
      r12.x <- xx$vcov[1]
      sigmasq.x <- r11.x
      rho.x <- r12.x/r11.x

      m <- lme4::lmer(yvar ~ 1 + (1 | subj), data=dta2)
      xx <- as.data.frame(lme4::VarCorr(m))
      r11.y <- sum(xx$vcov)
      r12.y <- xx$vcov[1]
      sigmasq.y <- r11.y
      rho.y <- r12.y/r11.y
    }

    k=n.reps
    dta2$uij <- (sqrt(sigmasq.y)/sqrt(sigmasq.x))*(dta2$x11*(1+(k-2)*rho.x)-dta2$x12*rho.x)/ ((1+(k-1)*rho.x)*(1-rho.x))
    dta2$vij=(sqrt(sigmasq.y)/sqrt(sigmasq.x))*(dta2$x12-(k-1)*dta2$x11*rho.x)/ ((1+(k-1)*rho.x)*(1-rho.x))

    m <- lmerTest::lmer(yvar ~ uij + vij + (1 | subj), data=dta2)
    m2 <- summary(m)$coefficients

    res <- list()
    res$beta1 <- m2[rownames(m2)=="uij", colnames(m2)=="Estimate" ]
    res$beta2 <- m2[rownames(m2)=="vij", colnames(m2)=="Estimate" ]
    res$se1 <- m2[rownames(m2)=="uij", colnames(m2)=="Std. Error" ]
    res$se2 <- m2[rownames(m2)=="vij", colnames(m2)=="Std. Error" ]
    res$p1 <- m2[rownames(m2)=="uij", colnames(m2)=="Pr(>|t|)" ]
    res$p2 <- m2[rownames(m2)=="vij", colnames(m2)=="Pr(>|t|)" ]

    res$z1=0.5*log((1+res$beta1)/(1-res$beta1))
    res$z2=0.5*log((1+res$beta2)/(1-res$beta2))
    res$var.z1=(1/(1-res$beta1**2)**2)*res$se1**2
    res$se.z1=sqrt(res$var.z1)
    res$ci1.z1=res$z1-1.96*res$se.z1
    res$ci2.z1=res$z1+1.96*res$se.z1
    res$ci1.beta1=(exp(2*res$ci1.z1)-1)/(exp(2*res$ci1.z1)+1)
    res$ci2.beta1=(exp(2*res$ci2.z1)-1)/(exp(2*res$ci2.z1)+1)
    res$var.z2=(1/(1-res$beta2**2)**2)*res$se2**2
    res$se.z2=sqrt(res$var.z2)
    res$ci1.z2=res$z2-1.96*res$se.z2
    res$ci2.z2=res$z2+1.96*res$se.z2
    res$ci1.beta2=(exp(2*res$ci1.z2)-1)/(exp(2*res$ci1.z2)+1)
    res$ci2.beta2=(exp(2*res$ci2.z2)-1)/(exp(2*res$ci2.z2)+1)

    rho.xij.s=6/pi*asin(res$beta1/2)
    rho.xij.str.s=6/pi*asin(res$beta2/2)
    ci1.rho.xij.s=6/pi*asin((res$ci1.beta1/2))
    ci2.rho.xij.s=6/pi*asin((res$ci2.beta1/2))
    ci1.rho.xij.str.s=6/pi*asin((res$ci1.beta2/2))
    ci2.rho.xij.str.s=6/pi*asin((res$ci2.beta2/2))

    sample.size <- length(unique(dta2$id))
    number.of.replicates <- max(dta2$rep)
    icc.xx <- round(rho.x,n.digits)
    sigma.sq.x <- round(sigmasq.x,n.digits)
    icc.yy <- round(rho.y,n.digits)
    sigma.sq.y <- round(sigmasq.y,n.digits)
    cross.correlation <- paste(round(rho.xij.str.s,n.digits), " (", round(ci1.rho.xij.str.s,n.digits), ", ", round(ci2.rho.xij.str.s,n.digits), "), p.val=", round(res$p2,n.digits+2), sep="")
    spearman.correlation <- paste(round(rho.xij.s,n.digits), " (", round(ci1.rho.xij.s,n.digits), ", ", round(ci2.rho.xij.s,n.digits), "), p.val=", round(res$p1,n.digits+2), sep="")

    spearman.result <- matrix(nrow=0, ncol=1)
    spearman.result <- rbind(spearman.result, sample.size,number.of.replicates,id.var.name,rep.var.name, x.var.name, icc.xx,sigma.sq.x, y.var.name, icc.yy, sigma.sq.y, cross.correlation, spearman.correlation,covar.list2)
    rownames(spearman.result) <- c("Sample Size", "Number of Replicates", "ID Variable Name", "Repeated Variable Name", "X Variable Name", paste("Intraclass Correlation Coeff X (",x.var.name,")",sep=""), paste("Sigma Squared X (",x.var.name,")",sep=""), "Y Variable Name", paste("Intraclass Correlation Coeff Y (",y.var.name,")",sep=""), paste("Sigma Squared Y (",y.var.name,")",sep=""), "Cross Correlation", "Spearman Correlation", "Covariates Adjusted For")
    colnames(spearman.result) <- "Result"
    spearman.result <- noquote(spearman.result)

    cluster.corr.result <- list()
    cluster.corr.result$acknowledgements <- "Many Thanks to Bernard Rosner PhD and Yvonne Mu from the Harvard School of Public Health for providing the SAS code used to create this Function and Angelica Paulos MD MPH for insight on adapting the code to R"
    cluster.corr.result$pearson.result <- pearson.result
    cluster.corr.result$spearman.result <- spearman.result
    print(cluster.corr.result)
    return(cluster.corr.result)
  }
}