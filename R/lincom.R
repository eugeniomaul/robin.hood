lincom <- function(model, betas=NULL, mag=NULL){
  if(length(betas>=1) & length(betas)==length(mag)){
    coef.names <- rownames(summary(model)$coefficients)
    title <- c()
    for(i in 1:length(betas)){
      if(i != length(betas) & sum(coef.names==betas[i])>=1){
        temp <- paste(betas[i], "*", mag[i], " + ", sep="")
        title <- paste(title, temp,sep="" )
      }else if(sum(coef.names==betas[i])>=1){
        temp <- paste(betas[i], "*", mag[i], sep="")
        title <- paste(title, temp,sep="" )			
      }
    }

    K <- rep(0, times=length(coef.names))
    for(i in 1:length(coef.names)){
      temp.beta = betas[i]
      K[coef.names==temp.beta] <- mag[i]
    }
    K <- matrix(K, 1)

    t <- multcomp::glht(model, linfct = K)

    result = list()
    result$linear.combination.for <- title
    result$result <- summary(t)
    result$ci <- confint(summary(t))

    return(result)
  }else{
    result <- "You must specify at least 1 coefficient (beta) and length of betas and magnitude must be the same"
    return(result)
  }
}