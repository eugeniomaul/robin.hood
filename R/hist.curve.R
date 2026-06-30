hist.curve <- function(data){
  data = data[!is.na(data)]
  h <- hist(data)
  # Add a Normal Curve (Thanks to Peter Dalgaard)
  xfit<-seq(min(data),max(data),length=100) 
  yfit<-dnorm(xfit,mean=mean(data),sd=sd(data)) 
  yfit <- yfit*diff(h$mids[1:2])*length(data) 
  lines(xfit, yfit, col="blue", lwd=2)
  }
