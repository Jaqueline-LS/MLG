envel_ninv_inverse2<-function(fit.model,alfa=0.05)
{
  rig <- function(n, mu = stop("no shape arg"), lambda = 1)
  {
    #  Random variates from inverse Gaussian distribution
    #  Reference:
    #      Chhikara and Folks, The Inverse Gaussian Distribution,
    #      Marcel Dekker, 1989, page 53.
    #  GKS  15 Jan 98
    #
    if(any(mu<=0)) stop("mu must be positive")
    if(any(lambda<=0)) stop("lambda must be positive")
    if(length(n)>1) n <- length(n)
    if(length(mu)>1 && length(mu)!=n) mu <- rep(mu,length=n)
    if(length(lambda)>1 && length(lambda)!=n) lambda <- rep(lambda,length=n)
    y2 <- rchisq(n,1)
    u <- runif(n)
    r1 <- mu/(2*lambda) * (2*lambda + mu*y2 - sqrt(4*lambda*mu*y2 + mu^2*y2^2))
    r2 <- mu^2/r1
    ifelse(u < mu/(mu+r1), r1, r2)
  }
  #----------------------------------------------------------------#                     
  #
  #par(mfrow=c(1,1))
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  mu <-predict(fit.model,type="response")
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  ro <- resid(fit.model,type="response")
  fi <- (n-p)/sum((ro^2)/(fitted(fit.model)^3))
  td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
  #
  e <- matrix(0,n,100)
  #
  for(ii in 1:100){
    resp <- rig(n,mu,fi)
    fit <- glm(resp ~ X, family=inverse.gaussian)
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    ro <- resid(fit,type="response")
    phi <- (n-p)/sum((ro^2)/(fitted(fit)^3))
    e[,ii] <- sort(resid(fit,type="deviance")*sqrt(phi/(1-h)))}
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
  qqnorm(td,xlab="Percentil da N(0,1)",
         ylab="Componente do Desvio", ylim=faixa, pch=16)
  par(new=T)
  #
  qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2)
  #----------------------------------------------------------------#  
  
 
  
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
