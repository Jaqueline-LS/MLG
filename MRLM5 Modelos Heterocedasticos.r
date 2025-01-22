# Como detectar Heterocedasticidade:
# M?todo gr?fico: gr?fico de res x ajuste
# Teste: Bartlett (bartlett.test) e Anscombe (n?o tem no R)
rm(list=ls(all=TRUE))
# Gerando modelos heteroced?sticos (Estudo de Simula??o)
library(mvtnorm)
library(MASS)
p=3
betaS=c(5,-2,1)
n1=30
n2=40
n3=30
n=n1+n2+n3
erro1=rnorm(n1,0,3)
erro2=rnorm(n2,0,7)
erro3=rnorm(n3,0,5)
betaX=c(5,-2)
Xx=mvrnorm(n,betaX,diag(c(2,1)))
X=cbind(1,Xx)
X=as.matrix(X)
y1=X[1:n1,]%*%betaS + erro1
y2=X[(n1+1):(n1+n2),]%*%betaS + erro2
y3=X[(n1+n2+1):n,]%*%betaS + erro3
y=rbind(y1,y2,y3)
plot(y,type="l")

reg1=lm(y~-1+X)
summary(reg1)
# Verificar heterocesticidade
# Gr?fico
res=rstandard(reg1)
ajuste=reg1$fitted.values
plot(ajuste,res)

par(mfrow=c(1,2))
plot(X[,2], res)
plot(X[,3], res)


# Como detectar Heterocedasticidade:
# M?todo gr?fico: gr?fico de res x ajuste
# Teste: Bartlett (bartlett.test) e Anscombe (n?o tem no R)
rm(list=ls(all=TRUE))
# Gerando modelos heteroced?sticos (Estudo de Simula??o)

n=100
betaS=c(5,-2,1)
xi<-rnorm(n,0,0.01)
erro1=rnorm(n,0,exp(xi))
betaX=c(5,-2)
Xx=mvrnorm(n,betaX,diag(c(2,1)))
X=cbind(1,Xx)
X=as.matrix(X)
y=X[1:n,]%*%betaS + erro1

plot(y,type="l")

reg1=lm(y~-1+X)
summary(reg1)
# Verificar heterocesticidade
# Gr?fico
res=rstandard(reg1)
ajuste=reg1$fitted.values
plot(ajuste,res)

par(mfrow=c(1,2))
plot(X[,2], res)
plot(X[,3], res)




rm(list=ls())
# Modelo Charnet: variancia aumenta com X
p=3
betaS=c(5,1)
n=100
betaX=2
Xx=rnorm(n,betaX,2)
Xx=sort(Xx)
X=cbind(1,Xx)
X=as.matrix(X)

erro=rnorm(n,0,abs(Xx))
y=X%*%betaS + erro

plot(y,type='l')

reg1=lm(y~-1+X)
summary(reg1)
# Verificar heterocedasticidade
# Gr?fico
res=rstandard(reg1)
ajuste=as.numeric(reg1$fitted.values)
plot(ajuste,res)

# Teste  de Anscombe
Anscombe <-function(reg1){
X=model.matrix(reg1)
H=X%*%solve(t(X)%*%X)%*%t(X)
ym=reg1$model[,1]
p=length(reg1$coefficients)
n=length(ym)
ajuste=as.numeric(reg1$fitted.values)
ytil=sum(as.numeric((1-diag(H)))*ajuste)/(n-p)
An=anova(reg1)
S2=An[2,3]
Ad=S2*t(ym-mean(ym))%*%((diag(n)-H)^2)%*%(ym-mean(ym))
rnp=reg1$residuals
An=sum(rnp^2*(ajuste-ytil))
A=An/Ad
return(A)
}

Anscombe(reg1)
# Estima??o de M?nimos Quadrados Generalizados (EMQG)

?lm.gls

# Calculando um estimador para Phi


Phi_EMQG <-function(reg){
X=model.matrix(reg)
H=X%*%solve(t(X)%*%X)%*%t(X)
ym=reg$model[,1]
n=length(ym)
M=diag(n)-H
r=M%*%ym
sigmav=(solve(M^2))%*%(r^2)
Phi=diag(as.vector(sigmav))
return(Phi)
}

Phi=Phi_EMQG(reg1)
X=model.matrix(reg1)
Phi1=solve(Phi)
ym= reg1$model[,1]
beta_EMQG=solve(t(X)%*%Phi1%*%X)%*%t(X)%*%Phi1%*%ym
beta_EMQG

# Beta MQO
Phi1=diag(length(ym))
beta_EMQG=solve(t(X)%*%Phi1%*%X)%*%t(X)%*%Phi1%*%ym
beta_EMQG

betaS


# Regress?o Ponderada (Charnet)
rm(list=ls())
# Gera??o
n=100
sigma2=3
V=as.vector(runif(n,1,5))   # Conhecida
betaS=c(5,-2,1)
betaX=c(5,-2)
Xa=mvrnorm(n,betaX,diag(c(2,1)))
X=cbind(1,Xa)
X=as.matrix(X)
erro=mvrnorm(1,rep(0,n),sigma2*diag(V))
y=X%*%betaS + erro
# Beta MQPonderado
W=diag(1/sqrt(V))
Xw=W%*%X
yw=W%*%y
BetaW=solve(t(Xw)%*%Xw)%*%t(Xw)%*%yw
Beta_MQO=solve(t(X)%*%X)%*%t(X)%*%y

BetaW
Beta_MQO
betaS

