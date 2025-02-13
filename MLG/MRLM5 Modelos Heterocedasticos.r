# Como detectar Heterocedasticidade:
# Método gráfico: gráfico de res x ajuste
# Teste: Bartlett (bartlett.test) e Anscombe (não tem no R)
rm(list=ls(all=TRUE))
# Gerando modelos heterocedásticos (Estudo de Simulação)
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
# Gráfico
res=rstandard(reg1)
ajuste=reg1$fitted.values
plot(ajuste,res)

rm(list=ls())
# Modelo Charnet: variância aumenta com X
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
# Gráfico
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
# Estimação de Mínimos Quadrados Generalizados (EMQG)

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


# Regressão Ponderada (Charnet)
rm(list=ls())
# Geração
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
