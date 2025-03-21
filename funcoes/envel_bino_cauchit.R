envel_bino_cauchit<-function(fit.model,alfa=0.05)
{
  par(mfrow=c(1,1))
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  td <- resid(fit.model,type="deviance")/sqrt(1-h)
  e <- matrix(0,n,100)
  #
  for(ii in 1:100){
    dif <- runif(n) - fitted(fit.model)
    dif[dif >= 0 ] <- 0
    dif[dif<0] <- 1
    nresp <- dif
    fit <- glm(nresp ~ X, family=binomial(link=cauchit))
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    e[,ii] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
  #
  e1 <- numeric(n)
  e2 <- numeric(n)
  #
  for(ii in 1:n){
    eo <- sort(e[ii,])
    e1[ii] <- (eo[2]+eo[3])/2
    e2[ii] <- (eo[97]+eo[98])/2}
  #
  med <- apply(e,1,mean)
  faixa <- range(td,e1,e2)
  par(pty="s")
  qqnorm(td,xlab="Percentis da N(0,1)",
         ylab="Componente do Desvio", ylim=faixa, pch=16)
  par(new=T)
  #
  qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2)                  
  
  
  plot_capturado<-recordPlot()
  ind <- numeric(n)
  ind <- sort(td)<sort(e1) | sort(td)>sort(e2)
  prop.fora<-sum(ind)/n
  qualidade<-NULL
  if(prop.fora==0)
  {
    qualidade<-"Ótimo"
  }else if(prop.fora>0 & prop.fora<alfa/2)
  {
    qualidade<-"Bom"
  } else if((prop.fora>=alfa/2 & prop.fora<=alfa))
  {
    qualidade<-"Razoável"
    
  }else if((prop.fora>alfa))
  {
    qualidade<-"Ruim"
  }
  return(list(qualidade, plot_capturado))
}