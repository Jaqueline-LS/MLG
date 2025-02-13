 ## Dados com Resposta Positiva

# Exemplo Modelo Gamma: Dados da frota, peixe-batata
pesca <- read.table(file="MLG/Dados/pesca.dat",header=F,col.names=c("frota","ano","trimestre","latitude","longitude","dias","captura","cpue"))
attach(pesca)
latitude=as.numeric(latitude)
longitude=as.numeric(longitude)
cpue=as.numeric(cpue)
frota <- factor(frota)
frota <- C(frota,treatment)
ano <- factor(ano)
ano <- C(ano,treatment)
trim <- factor(trimestre)
trim <- C(trim,treatment)
source("MLG/Funcoes/rv.gama.txt")
source("MLG/Funcoes/testef.txt")
n=length(cpue)

# Ir anotando os BICs finais, significance e envelopes

# Family Gamma
# Gamma family: links inverse (default), identity and log
# Link log
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=log))
library(MASS)
gamma.shape(fit1)
summary(fit1)

# Testando signific?ncia do modelo: H0: betas=0 x H1: pelo menos um beta diferente de 0.
fit0=glm(cpue ~ 1, family=Gamma(link=log))
# Teste RV para gamma
rv.gama(cpue,fit0,fit1) #

# Teste F (para confirmar)
testef(fit0,fit1)

# sele??o de vari?veis
fit0=stepAIC(fit1,k=log(n))
summary(fit0)  
# mesma coisa que rodar novamente
fit0=glm(cpue ~ frota, family=Gamma(link=log))
gamma.shape(fit0)

# Envelope
fit.model=fit0
source("MLG/Funcoes/envel_gama_log.txt")


# Link inverse
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=inverse))
summary(fit1)
fit0=stepAIC(fit1,k=log(n))
summary(fit0) 
# mesma coisa que rodar novamente
fit0=glm(cpue ~ frota + latitude, family=Gamma(link=inverse))

# Envelope
fit.model=fit0
source("MLG/Funcoes/envel_gama_inverse.txt")  # Alguns pontos fora, mas nada aberrante


# Link identity
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=identity))
summary(fit1)
fit0=stepAIC(fit1,k=log(n))
summary(fit0)    
# mesma coisa que rodar novamente
fit0=glm(cpue ~ frota, family=Gamma(link=identity))
gamma.shape(fit0)

# Envelope
fit.model=fit0
source("MLG/Funcoes/envel_gama_ident.txt")  



##################### Sele??o do melhor modelo ################################
# The gaussian family accepts the links (as names) identity, log and inverse; 
# The Gamma family the links inverse, identity and log; 
# The inverse.gaussian family the links 1/mu^2, inverse, identity and log. 


tab=matrix(0,2,10)
colnames(tab)=c('Normal_Ident','Normal_log','Normal_inv','Gamma_inv','Gamma_ident','Gamma_log','NI_inv2','NI_inv','NI_ident','NI_log')
rownames(tab)=c('BIC','Envelope')
n=length(cpue)


######## Modelo Normal
j=1
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=gaussian)  # Identity
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_norm.txt')
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Ruim'


# Link log
j=2
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=gaussian(link=log)) 
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_norm_log.txt')    # problema, pois mesmo fazendo mu=exp(X*beta)), tenhode gerar de Yn ~ Normal(mu,sigma^2). Dependendo do valor de sigma^2, posso gerar valores negativos de Yn. A? o modelo n?o roda
source('MLG/Funcoes/envel_norm.txt')
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Ruim'

# Link inverse
j=3
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=gaussian(link=inverse))
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_norm_inverse.txt')   # Aqui n?o deu problema
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Ruim'

#### Modelo Gamma: inverse, identity and log;
# Link inverse
j=4
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=inverse))
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_gama_inverse.txt')  
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Razoavel'

# Link identity
j=5
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=identity))
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_gama_ident.txt')  
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Razoavel'

# Link log
j=6
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=log))
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_gama_log.txt')  
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Razoavel'


######## Normal inversa
# inverse.gaussian family the links 1/mu^2 (default), inverse, identity and log.

# Link 1/mu^2
j=7
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian)
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_ninv_inverse2.txt')  
tab[1,j]=NA
tab[2,j]='NA'

# Link inverse
j=8
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian(link=inverse))
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_ninv_inverse.txt')  
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='ruim'

# Link identity
j=9
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian(link=identity))
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_ninv_ident.txt')  
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Ruim'

# Link log
j=10
fit1=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian(link=log))
fit.model=stepAIC(fit1,k=log(n))
summary(fit.model) 
source('MLG/Funcoes/envel_ninv_log.txt')  
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Ruim'  

tab
  

# Teste de adequa??o da fun??o de liga??o, Gamma, link log
fit.model=glm(cpue ~ frota , family=Gamma(link=log))
pl2 <- predict(fit.model)^2 #  se so sobrar uma variável categorica no modelo, pl2 tem correspondencia 1 a 1 com a variavel
table(frota,pl2)
# Nao roda, pois pl2 ? 1 a 1 com frota
fit.model=glm(cpue ~ frota + pl2, family=Gamma(link=log))
summary(fit.model)
     

# Escolher o melhor modelo com base nos envelopes e BIC+significance+adequa??o da fun??o de liga??o. Fazer an?lise de res?duos e diagn?stico para o melhor modelo



# An?lise de res?duos e diagn?stico
# Escolhemos inicialmente a ligacao inverse
fit.model=glm(cpue ~ frota + latitude, family=Gamma(link=log))
# Res?duos
fi=gamma.shape(fit.model)$alpha
X <- model.matrix(fit.model)
V <- fitted(fit.model)  # vetor de valores ajustados (model$fitted.values)
Vm <- diag(V)
w <- fit.model$weights  # vetor de pesos
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)
rD <- resid(fit.model, type= "deviance")
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit.model, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado

# Gr?ficos
# Res?duos

plot(tD) # comparar com normal  (qualidade do ajuste)
plot(V,tD) # V: Valor ajustado x res?duos
# res?duos x variaveis explicativas continuas
plot(latitude,tD)


# Diagn?sticos
# Pontos de Alavanca
plot(hii)
ind=identify(hii)

# Influ?ncia - Dele??o de Casos
# Afastamento pela log-verossimilhan?a
LD = hii*(tS^2)/(1-hii)
plot(LD)
identify(LD)

X[c(3,8),]
cpue[c(3,8)]
summary(cpue)
summary(latitude)
summary(longitude)

######################################################################################
# Dados de Snack (Gilberto)
snack <- read.table(file="MLG/Dados/snack.dat",header=F,col.names=c("cisalhamento","grupo","semana"))
attach(snack)
head(snack)
grupo = factor(grupo) 
hist(cisalhamento)
boxplot(cisalhamento)
boxplot(cisalhamento ~ grupo)
boxplot(cisalhamento ~ semana)

s2 = semana*semana

# Family  Inverse Gaussian
# Link Identity
fit1.snack = glm(cisalhamento ~ grupo + semana + s2, family=inverse.gaussian(link=identity))
summary(fit1.snack)

fit2.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo+s2*grupo, family=inverse.gaussian(link=identity))
summary(fit2.snack)

fit3.snack=glm(cisalhamento ~ grupo + semana + s2 +s2*grupo, family=inverse.gaussian(link=identity))  
summary(fit3.snack)

fit.model=fit3.snack
source("MLG/Funcoes/envel_ninv_ident.txt")  # parece OK

# Link 1/mu2
fit4.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo+s2*grupo, family=inverse.gaussian)
summary(fit4.snack)

fit5.snack=glm(cisalhamento ~ grupo + semana + s2 + semana*grupo, family=inverse.gaussian)  
summary(fit5.snack)

fit.model=fit5.snack
source("MLG/Funcoes/envel_ninv_inverse2.txt")  # parece OK

# Link inverse
fit6.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo+s2*grupo, family=inverse.gaussian(link=inverse))
summary(fit6.snack)

fit7.snack=glm(cisalhamento ~ grupo + semana + s2 + semana*grupo, family=inverse.gaussian(link=inverse))  
summary(fit7.snack)

fit.model=fit7.snack
source("MLG/Funcoes/envel_ninv_inverse.txt")  # parece OK

# Link log
fit8.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo+s2*grupo, family=inverse.gaussian(link=log))
summary(fit8.snack)

fit9.snack=glm(cisalhamento ~ grupo + semana + s2 + semana*grupo, family=inverse.gaussian(link=log))  
summary(fit9.snack)

fit.model=fit9.snack
source("MLG/Funcoes/envel_ninv_inverse.txt")  # parece OK


# Ajustar modelo Gama
# Link inverse
fit10.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo+s2*grupo, family=Gamma)
summary(fit10.snack)

fit11.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo, family=Gamma)
summary(fit11.snack)

fit.model=fit11.snack
source("MLG/Funcoes/envel_gama_inverse.txt")

# Link log
fit12.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo+s2*grupo, family=Gamma(link=log))
summary(fit12.snack)

fit13.snack = glm(cisalhamento ~ grupo + semana + s2 + semana*grupo, family=Gamma(link=log))
summary(fit13.snack)

fit.model=fit13.snack
source("MLG/Funcoes/envel_gama_log.txt")

# Melhor modelo: Normal inversa, link 1/mu^2


rm(list = ls())

# An?lise de res?duos e diagn?stico
# Escolhemos inicialmente a ligacao inverse
fit.model=fit5.snack
# Res?duos
fi=gamma.shape(fit.model)$alpha
X <- model.matrix(fit.model)
V <- fitted(fit.model)  # vetor de valores ajustados (model$fitted.values)
Vm <- diag(V)
w <- fit.model$weights  # vetor de pesos
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)
rD <- resid(fit.model, type= "deviance")
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit.model, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado

# Gr?ficos
# Res?duos

plot(tD) # comparar com normal  (qualidade do ajuste)
plot(V,tD) # V: Valor ajustado x res?duos
# res?duos x variaveis explicativas continuas
plot(semana,tD)


# Diagn?sticos
# Pontos de Alavanca
plot(hii)
ind=identify(hii)

# Influ?ncia - Dele??o de Casos
# Afastamento pela log-verossimilhan?a
LD = hii*(tS^2)/(1-hii)
plot(LD)
identify(LD)

ind<-c(2,10,311,465,744)
cisalhamento[ind]
summary(cisalhamento)
X[ind,]
summary(semana)
summary(s2)
hist(cisalhamento)
hist(cisalhamento[snack$semana==2])
# Para a semana 2 os pontos que influenciaram na verossimilhança são 
#os que possuem a maior resposta nessa semana

