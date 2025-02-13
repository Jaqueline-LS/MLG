

# Gilberto (2013), Exemplo 1.12.6: Salario de executivos
dado=read.table(file="salary.dat",col.names=c('Salario','Sexo','Experiencia','Posicao'))
attach(dado)
Sexo <- factor(Sexo)
Sexo <- C(Sexo,treatment)

fit1 = lm(Salario ~ Sexo + Experiencia + Posicao)
summary(fit1)
n=length(Salario)
p=length(fit1$coefficients)

reg.res=influence.measures(fit1)
Di_Cook=reg.res$infmat[,p+3]
corte=pf(0.5,p,n-p)
corte
plot(Di_Cook)
abline(h=corte)


Hii=reg.res$infmat[,p+4]
corte=2*p/n
plot(Hii)
abline(h=corte)
identify(Hii,n=5)

res_pad=rstandard(fit1)
res_stud=rstudent(fit1)
ajuste=fit1$fitted.values
plot(res_stud)
abline(h=-2)
abline(h=2)

plot(ajuste,res_stud)
abline(h=-2)
abline(h=2)

# Envelope
fit.model=fit1
source("envel_norm.txt")

AIC(fit1)
# Ver R2

# Obvio que existe uma interacao entre Posicao e Experiencia

fit1 = lm(Salario ~ Sexo + Experiencia + Posicao + Experiencia*Posicao)
summary(fit1)
n=length(Salario)
p=length(fit1$coefficients)

reg.res=influence.measures(fit1)
Di_Cook=reg.res$infmat[,p+3]
corte=pf(0.5,p,n-p)
corte
plot(Di_Cook)
abline(h=corte)


Hii=reg.res$infmat[,p+4]
corte=2*p/n
plot(Hii)
abline(h=corte)

res_pad=rstandard(fit1)
res_stud=rstudent(fit1)
ajuste=fit1$fitted.values
plot(res_stud)
abline(h=-2)
abline(h=2)

plot(ajuste,res_stud)
abline(h=-2)
abline(h=2)

# Envelope
fit.model=fit1
source("envel_norm.txt")

AIC(fit1)
# Ver R2

#### outras ligacoes da Normal
tab=matrix(0,1,3)
colnames(tab)=c('Normal_ident','Normal_log','Normal_inv')
rownames(tab)='BIC'
fit0=glm(Salario ~ Sexo + Experiencia + Posicao + Experiencia*Posicao, gaussian(link='identity'))
tab[,1]=AIC(fit0,k=log(n))
fit0=glm(Salario ~ Sexo + Experiencia + Posicao + Experiencia*Posicao, gaussian(link='log'))
tab[,2]=AIC(fit0,k=log(n))
fit0=glm(Salario ~ Sexo + Experiencia + Posicao + Experiencia*Posicao, gaussian(link='inverse'))
tab[,3]=AIC(fit0,k=log(n))

tab



###################################################################################

# Exemplo Gilberto (2013), 1.12.1: Estudo entre renda e escolaridade, por Estados

dado=read.table(file="censo.dat",col.names=c('Estado','escolar','renda'))
attach(dado)
fit1.censo = lm(renda ~ escolar)

plot(escolar,renda,pch=19)
betaest=fit1.censo$coefficients
abline(a=betaest[1],b=betaest[2])

reg.res=influence.measures(fit1.censo)
p=2
n=length(renda)
Di_Cook=reg.res$infmat[,p+3]
corte=pf(0.5,p,n-p)
plot(Di_Cook)
abline(h=corte)
identify(Di_Cook,n=1)

Hii=reg.res$infmat[,p+4]
corte=2*p/n
plot(Hii)
abline(h=corte)
identify(Hii,n=1)

res_pad=rstandard(fit1.censo)
res_stud=rstudent(fit1.censo)
ajuste=fit1.censo$fitted.values
plot(res_stud)
abline(h=-2)
abline(h=2)
identify(res_stud,n=1)

# Envelope
fit.model=fit1.censo
source("envel_norm.txt")

AIC(fit1.censo)

# MLG

# Comparacao dos BICs com todas as candidatas
tab=matrix(0,1,10)
colnames(tab)=c('Normal_ident','Normal_log','Normal_inv','Gamma_inv','Gamma_ident','Gamma_log','NI_1/mu2','NI_inv','NI_ident','NI_log')
rownames(tab)='BIC'
fit0=glm(renda ~ escolar, gaussian(link='identity'))
tab[,1]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, gaussian(link='log'))
tab[,2]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, gaussian(link='inverse'))
tab[,3]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, Gamma(link='inverse'))
tab[,4]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, Gamma(link='identity'))
tab[,5]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, Gamma(link='log'))
tab[,6]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, inverse.gaussian(link='1/mu^2'))
tab[,7]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, inverse.gaussian(link='inverse'))
tab[,8]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, inverse.gaussian(link='identity'))
tab[,9]=AIC(fit0,k=log(n))
fit0=glm(renda ~ escolar, inverse.gaussian(link='log'))
tab[,10]=AIC(fit0,k=log(n))

tab


# Gamma
fit1=glm(renda ~ escolar, family=Gamma(link=log))
library(MASS)
summary(fit1)


### Analise de residuos e diagnostico
fi=as.numeric(gamma.shape(fit1))[1]
X <- model.matrix(fit1) 
V <- fitted(fit1)  
Vm <- diag(V) 
w <- fit1$weights 
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)  # vetor diagonal de H

# Resíduos
rD <- resid(fit1, type= "deviance")
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit1, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado
# LDi
LD = hii*(tS^2)/(1-hii) 

# Graficos
plot(tD)
abline(h=-2)
abline(h=2)


plot(V,tD)
identify(V,tD,n=3)

plot(V[-27],tD[-27])

plot(escolar,tD)


# Envelope
fit.model=fit1
source("envel_gama_log.txt")


# Adequação da função de ligação
pl2 <- predict(fit1)^2
modelo2=glm(renda ~ escolar+pl2, family=Gamma(link=log))
summary(modelo2)
source("testef.txt")
testef(fit.model,modelo2)
# Conclusão: teste F não rejeitou H0. Portanto, eta^2  não é significativo. Logo, a função de ligação é apropriada.
