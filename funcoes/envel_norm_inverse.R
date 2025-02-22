envel_norm_inverse<-function(fit.model,alfa=0.05)
{
  par(mfrow=c(1,1))
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  si <- lm.influence(fit.model)$sigma
  r <- resid(fit.model)
  tsi <- r/(si*sqrt(1-h))
  mu=1/(as.vector(X%*%matrix(fit.model$coefficients,nrow=ncol(X),1))) # Link inverse
  sigma=sqrt(summary(fit.model)$dispersion)
  #
  inter=sum(abs(X[,1])-rep(1,n))
  e <- matrix(0,n,100)
  for(jj in 1:100){
    resp <- rnorm(n,mu,sigma)
    if (inter!=0) fit <- glm(resp ~ X, family=gaussian(link=inverse))
    else fit <- glm(resp ~ -1+ X, family=gaussian(link=inverse))
    X=model.matrix(fit)
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    si <- lm.influence(fit)$sigma
    r <- resid(fit)
    tsij <- r/(si*sqrt(1-h))
    e[,jj] <- sort(tsij)}
  #
  e1 <- numeric(n)
  e2 <- numeric(n)
  #
  for(ii in 1:n){
    eo <- sort(e[ii,])
    e1[ii] <- (eo[2]+eo[3])/2
    e2[ii] <- (eo[97]+eo[98])/2 }
  #
  med <- apply(e,1,mean)
  faixa <- range(tsi,e1,e2)
  #
  par(pty="s")
  pp=qqnorm(tsi,xlab="Percentis da N(0,1)",
            ylab="Residuo Studentizado", ylim=faixa, pch=16)
  par(new=T)
  qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1)
  par(new=T)
  qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2)
  plot_capturado<-recordPlot()
  ind <- numeric(n)
  ind <- sort(tsi)<sort(e1) | sort(tsi)>sort(e2)
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