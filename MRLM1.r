## Regressão Linear Múltipla

dado=read.table("Combustivel2.txt",header=TRUE)    # Gauss, pag. 17
attach(dado)
names(dado)

## Ajuste do MRLM
reg=lm(Con~Tax+Ren+Rod+Lic) # Consumo medio per capta, Livro do Gauss Exemplo 1.2
names(reg)
         
# ANOVA - An?lise de Vari?ncia (Teste de Hip?teses) - T e F
# H0: beta=0 x H1: Algum beta_k #0 
anova(reg)
summary(reg)
# tira Rod ======> o impacto dela vai para outra/outras que pessuem relação com ela
reg=lm(Con~Tax+Ren+Lic)
summary(reg)

# Calculo da ANOVA na mão
y=Con
X=model.matrix(reg)
n=dim(X)[1]
p=dim(X)[2]
X=as.matrix(X)  
beta=reg$coefficients
beta=as.matrix(beta,nrow=p)
H=X%*%solve(t(X)%*%X)%*%t(X)    
SQT=t(y)%*%y-n*mean(y)^2
SQReg=t(y)%*%H%*%y-n*mean(y)^2
SQR=t(y)%*%y-t(y)%*%H%*%y
QMReg=SQReg/(p-1)
QMR=SQR/(n-p)
Fc=QMReg/QMR
p_valor=1-pf(Fc,p-1,n-p)
SQT
SQReg
SQR
Fc
p_valor

R2=SQReg/SQT
R2

#Eu que inclui
# curve(df(x,3,44),-0,10)
# curve(df(x,4,43),-0,10, add=T)

# Direto
ff=summary(reg)
names(ff)

# Residual standard error: Erro padr?o residual
S=sqrt(QMR)
S

############ Sele??o de Vari?veis Explicativas
# Teste T:
#H0: beta_k=0 x H1: beta_k# 0, k=2,...,p

summary(reg)

## Selecionando vari?veis: backward, forward, stepwise e AIC
reg=lm(Con~Tax+Ren+Rod+Lic)
## AIC
# ?AIC
# Generic function calculating the Akaike information criterion for one or several fitted model objects for which a
# log-likelihood value can be obtained, according to the formula -2*log-likelihood + k*npar, where npar represents the number of parameters in the fitted model, and k = 2 for the usual AIC, or k = log(n) (n the number of observations) for  the so-called BIC or SBC (Schwarz's Bayesian criterion). 

?step    # library 'stats' k=2 é o AIC 
?stepAIC # library 'MASS'

require(MASS)

## Selecionando vari?veis: comando step(library STATS) ou stepAIC(library MASS)
reg=lm(Con~Tax+Ren+Rod+Lic) # Consumo medio per capta, Livro do Gauss Exemplo 1.2
fit0=reg  # modelo com todas as variaveis

k=2 #(default: AIC)
AIC(reg)
fit=step(fit0) #default: stepwise
fit # solta o modelo final, pode ser tbm stepAIC
summary(fit)
fit=step(fit0,direction="backward")  # Se se selecionar as mesmas variaveis, obviamente o modelo serah o mesmo (mesmas estimativas)
summary(fit)
fit=step(fit0,direction="forward") # Vejam o que aconteceu!!!! # PARACE NÃO FUNCIONAR
summary(fit)

fit=stepAIC(fit0) # d? no mesmo
summary(fit)
fit=stepAIC(fit0,direction="backward")
fit=stepAIC(fit0,direction="forward") # Clécio não recomenda o forward


## BIC
n=length(Con)
k1=log(n)# (BIC)
fit=step(fit0,k=k1) # solta o modelo final, pode ser tbm stepAIC
summary(fit)
fit=step(fit0,direction="backward",k=k1)
summary(fit)
fit=step(fit0,direction="forward",k=k1)     # Novamente, nao elimina a variavel Rod, insignificante
summary(fit)

fit=stepAIC(fit0,k=k1) # d? no mesmo
fit=stepAIC(fit0,direction="backward",k=k1)
fit=stepAIC(fit0,direction="forward",k=k1)

plot(fit)        


# n<p 
# Função glmnet
# alpha 1 LASSO
# alpha 0 rigde

##########################################################################################################
######## Exemplo de como simular um MRLM
rm(list=ls(all=TRUE))
n=100
p=4
erro=as.matrix(rnorm(n,0,2),nrow=n,ncol=1)
x=matrix(rnorm(n*p,10,1),n,p)    # variaveis independentes
X=as.matrix(cbind(rep(1,n),x),nrow=n,ncol=p+1)
beta=as.matrix(rbind(5,1,2,0.5,0.1),p+1,1)
beta=as.matrix(rnorm(p+1),p+1,1) # ou
y=X%*%beta+erro
cor(y,X[,2:5])   
reg=lm(y~x)
summary(reg) 
reg=lm(y~1+X[,2]+X[,3]+X[,4]+X[,5])
summary(reg)
reg=stepAIC(reg)         
plot(reg)


      
#############################################################################################################
######### Res?duos e Diagn?sticos

# Alavanca: avaliar a influ?ncia de yi sobre o pr?prio valor ajustado.

# fun??o LDi
LDi <- function(y,fit.model){
X=model.matrix(fit.model)
sigma2=(summary(fit.model)$sigma)^2
res=reg$residuals
n=length(y)
p=length(fit.model$coefficients)
fit.inf=lm.influence(fit.model)
hii=fit.inf$hat
#sigma2i= ((n-p)*sigma2-res^2/(1-hii) )/(n-p-1)
sigma2i=fit.inf$sigma
betai=fit.inf$coefficients # matriz n x p
aux=solve(t(X)%*%X)
ld=rep(0,n)
for (i in 1:n){
ld[i]= sum(log(dnorm(y,X%*%as.matrix(betai[i,]),sqrt(sigma2i[i]))))
}
return(ld)
}
 

############ Simula??o
rm(list=ls(all=TRUE))
require(MASS)
n=100 # Regra de ouro 10 a 20 observações por parâmetro
p=4
p1=p+1
erro=as.matrix(rnorm(n,0,2),nrow=n,ncol=1)
x=matrix(rnorm(n*p,10,1),n,p)
X=as.matrix(cbind(rep(1,n),x),nrow=n,ncol=p1)
beta=as.matrix(rbind(5,1,2,-3,-2),p1,1)
y=X%*%beta+erro
reg=lm(y~1+X[,2]+X[,3]+X[,4]+X[,5])# Trocar 1 por 0 => modelo sem intercepto
summary(reg)
reg=stepAIC(reg)   

############### Adicionar termos nao-lineares de Xj
X1= X[,2]^2
beta=as.matrix(rbind(5,1,2,-3,-2,2),p1,1)
Xn=cbind(X,X1)
y=Xn%*%beta+erro
reg=lm(y~1+X[,2]+X[,3]+X[,4]+X[,5])
summary(reg)

# com X1^2
reg=lm(y~1+X[,2]+X[,3]+X[,4]+X[,5]+I(X[,2]^2)) 
#reg=lm(y~1+X[,2]+X[,3]+X[,4]+X[,5]+X[,2]^2) Função I para n ter que criar uma variável

summary(reg)
#######################################
?influence.measures
?lm.influence
# Agora tambem tem 'influence', contendo lm.influence, valendo tbm para MLG
reg.res=influence.measures(reg)
reg.res
reg.res1=lm.influence(reg) # daqui tira direto hii, beta(i), sigma(i)  ; 
                           # dfbeta-> beta_(-i)-beta, solta para cada componente de beta
                           # cov.r eh uma medida de influencia da i-esima obs na var(beta_est): det(var(beta_(i)))/det(var(beta_est)), Besley et al. (1980)
reg.res1  
dffit=reg.res$infmat[,p1+1]
corte=2*sqrt(p/(n-p))
plot(dffit)
lines(1:n,corte*rep(1,n))
identify(dffit,n=6)

# observa??es potencialmente influentes
summary(reg.res)

Di_Cook=reg.res$infmat[,p1+3]
corte=pf(0.5,p1,n-p1)
plot(Di_Cook)
lines(1:n,corte*rep(1,n)) 
identify(Di_Cook,n=2)

Hii=reg.res$infmat[,p1+4]
Hii=reg.res1$hat # mais direto
corte=2*p1/n
plot(Hii)
lines(1:n,corte*rep(1,n))
identify(Hii,n=6)


res_pad=rstandard(reg)
res_stud=rstudent(reg) # tem distribuicao 
ajuste=reg$fitted.values

ldi=LDi(y,reg)
# 
# plot(nome do modelo ajustado)
# Figura esquerda inferior: ver se a vari?ncia est? mudando com a m?dia.

# Gr?ficos Res?duos:
plot(res_pad, ylim=c(-3,3))
plot(res_stud) # Verificar qualidade do ajuste: pontos aberrantes (mal ajustados)
plot(ajuste,res_pad) # Verificar Homogeneidade de vari?ncias ou Linearidade dos efeitos das var. explicativas

qqnorm(res_pad, ylim = c(-3,3), xlim=c(-3,3)) # Normalidade
qqline(res_pad)
plot(X[,2],res_pad) # Verificar se ainda existe rela??o entre X_k e y (n?o explicada por beta_k*X_k)
plot(X[,3],res_pad)
plot(X[,4],res_pad)
plot(X[,5],res_pad)

# Envelope simulado
fit.model=reg
source("envel_norm.txt")

#######################################################################################################

# Dados reais: Exemplo Combust?vel
rm(list=ls(all=TRUE))

dado=read.table("Combustivel2.txt",header=TRUE)
attach(dado)
names(dado)
reg=lm(Con~Ren+Lic)
reg=lm(Con~Ren+Lic+Tax)
X=model.matrix(reg)
n=dim(X)[1]
p=dim(X)[2]

summary(reg)

res_pad=rstandard(reg)
res_stud=rstudent(reg)
ajuste=reg$fitted.values

par(mfrow=c(1,2))
plot(res_pad)
plot(res_stud)
par(mfrow=c(1,1))


# Gr?ficos Res?duos:
plot(res_pad)
plot(res_stud) # Verificar qualidade do ajuste: pontos aberrantes (mal ajustados)
plot(ajuste,res_pad) # Verificar Homogeneidade de vari?ncias ou Linearidade dos efeitos das var. explicativas
qqnorm(res_pad) # Normalidade
qqnorm(res_stud)

plot(Ren,res_pad)
plot(Lic,res_pad)
plot(Tax,res_pad)

#Não é outlier em nenhuma variável explicativa que utilizamos POr que o consumo dele é maior

## Qualidade de ajuste: envelope simulado (EXPLICAR!!!) - Gilberto (2004), pag 46
fit.model <- reg
source("envel_norm.txt")

# Modelo validado só que tem um outliers=====> Cauda pesada!!!!

# Verificar Independ?ncia
plot(res_pad)
identify(res_pad,n=1)

# Para interpretar olhar os valores das variáveis para as observações analisadas e comparar com o resumos numericos dessas variáveis
summary(Lic)

?influence.measures

reg.res=influence.measures(reg)
reg.res

# DFFITS
dffits=reg.res$infmat[,p+1]
corte=2*sqrt(p/(n-p))
plot(dffits)
lines(1:n,corte*rep(1,n))
identify(dffits,n=3)
# DFFITS 19 40 45
# Cook 
Di_Cook=reg.res$infmat[,p+3]
corte=pf(0.5,p,n-p)
plot(Di_Cook)
lines(1:n,corte*rep(1,n))
identify(Di_Cook) # 40

# Hii
Hii=reg.res$infmat[,p+4]
corte=2*p/n
plot(Hii)
lines(1:n,corte*rep(1,n))
identify(Hii) #Hii 6,7,19,37,39,45

# outlier 40

## sobrepor um gr?fico: par(new=T)


# Fun??o com todos os gr?ficos do modelo ajustado
plot(reg)


# Influ?ncia global: Di
Di=Hii*(res_pad^2)/(p*(1-Hii))
plot(Di)

rm(list = ls())


