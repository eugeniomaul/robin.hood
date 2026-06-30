
glm.coef <- function(model, type= c("lm","logistic"), digits=3){
	mod.func = match.arg(type)
 if(mod.func=="logistic"){
 result <- cbind(exp(coef(model)), exp(confint(model)),summary(model)$coefficients[,4])
 colnames(result) <- c("Odds.Ratio", "CI.95.lower", "CI.95.higher", "p.value")
 result <- round(result, digits=3)
}else if(mod.func=="lm"){
 result <- cbind(coef(model), confint(model),summary(model)$coefficients[,4])
 colnames(result) <- c("Beta", "CI.95.lower", "CI.95.higher", "p.value")
 result <- round(result, digits=3)
	
}
return(result)
} 

