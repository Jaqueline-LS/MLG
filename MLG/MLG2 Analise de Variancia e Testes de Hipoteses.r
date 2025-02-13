######## An?lise de Vari?ncia e Testes Assint?ticos
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

#### 1. An?lise de Vari?ncia

# H0: beta=0 x H1:beta # 0   (betas das vari?veis, excluindo beta0)

# Modelo 1
fit1=glm(cpue ~ frota, family=Gamma(link=log))
summary(fit1)

# Modelo 2
fit2=glm(cpue ~ trim, family=Gamma(link=log))
summary(fit2)

# Sob H0
fit0=glm(cpue ~ 1, family=Gamma(link=log))
summary(fit0)

# Testes de Hip?teses
source("MLG/Funcoes/rv.gama.txt",encoding="latin1")
source("MLG/Funcoes/testef.txt",encoding="latin1")
source("MLG/Funcoes/Wald_Encaixado.txt",encoding="latin1")
source("MLG/Funcoes/Escore_Encaixado.txt",encoding="latin1")
require(MASS)
# Modelo 1
rv.gama(cpue,fit0,fit1) # gl=1
testef(fit0,fit1)       # gl=1,n-p-1, p=1,
betas=fit1$coefficients
ind=rep(1,length(betas))# beta=c(beta0,beta1), H0: beta1=0
ind[1]=0
wald_encaixado(fit1,ind)
X=model.matrix(fit1)
escore_encaixado(fit0,ind,X)

#Modelo 2
rv.gama(cpue,fit0,fit2)
testef(fit0,fit2)
betas=fit2$coefficients
ind=rep(1,length(betas))
ind[1]=0
wald_encaixado(fit2,ind)
X=model.matrix(fit2)
escore_encaixado(fit0,ind,X)

## Modelo proposto, v?rias vari?veis

#-----------------------ANOVA------------------------------
fit3=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=log))
summary(fit3)
rv.gama(cpue,fit0,fit3)
testef(fit0,fit3)
betas=fit3$coefficients
ind=rep(1,length(betas))
ind[1]=0
ind
wald_encaixado(fit3,ind)
X=model.matrix(fit3)
escore_encaixado(fit0,ind,X) # estranho, nao rejeitou

#-----------------------------------------------------------------------

####### 4. Sele??o de vari?veis   
fit4=stepAIC(fit3)
summary(fit4)  # notem que AIC n?o retirou longitude, mesmo sendo esta var. NS.
# vamos testar beta(longitude) = 0

# Selecao via ANOVA backward
# Passo 1
fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=log))
fit1=glm(cpue ~ ano + trim + latitude + longitude, family=Gamma(link=log))
fit2=glm(cpue ~ frota + trim + latitude + longitude, family=Gamma(link=log))
fit3=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=log))
fit4=glm(cpue ~ frota + ano + trim + longitude, family=Gamma(link=log))
fit5=glm(cpue ~ frota + ano + trim + latitude, family=Gamma(link=log))
testef(fit1,fit) # EM RELAÇÃO AO MODELO COM TODAS, NAO REJEITOU O MODELO COM FROTA É MELHOR FROTA IGUAL A 0
testef(fit2,fit)
testef(fit3,fit)
testef(fit4,fit)
testef(fit5,fit)
# Passo 2
fit=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=log))
fit1=glm(cpue ~ ano + latitude + longitude, family=Gamma(link=log))
fit2=glm(cpue ~ frota + latitude + longitude, family=Gamma(link=log))
fit3=glm(cpue ~ frota + ano + longitude, family=Gamma(link=log))
fit4=glm(cpue ~ frota + ano + latitude, family=Gamma(link=log))
testef(fit1,fit)
testef(fit2,fit)
testef(fit3,fit)
testef(fit4,fit)
# Passo 4
fit=glm(cpue ~ frota + ano + latitude , family=Gamma(link=log))
fit1=glm(cpue ~ ano + latitude, family=Gamma(link=log))
fit2=glm(cpue ~ frota + latitude, family=Gamma(link=log))
fit3=glm(cpue ~ frota + ano, family=Gamma(link=log))
testef(fit1,fit)
testef(fit2,fit)
testef(fit3,fit)



# 3. Modelos encaixados
# testar se beta(longitude) = 0
fit4=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=log))  # reescrever s? para facilitar constru??o do vetor ind abaixo
fit0=glm(cpue ~ frota + ano + latitude, family=Gamma(link=log))
rv.gama(cpue,fit0,fit4)
testef(fit0,fit4)
betas=fit4$coefficients
ind=rep(0,length(betas))
ind[length(ind)]=1
ind
wald_encaixado(fit4,ind)
X=model.matrix(fit4)
escore_encaixado(fit0,ind,X)

# testar se beta_1 = beta1_0
fit4=glm(cpue ~ frota + ano + latitude, family=Gamma(link=log)) 
summary(fit4)
# testar se beta(latitude)=0.05   DEPOIS  MUDAR PARA 0.04, 0.03...
bl0=0.07716#0.05
fit0=glm(cpue ~ frota + ano + offset(bl0*latitude), family=Gamma(link=log)) # fixou em 0.07716
summary(fit0)
rv.gama(cpue,fit0,fit4)
testef(fit0,fit4)
betas=fit4$coefficients
ind=rep(0,length(betas))
ind[length(ind)]=1
ind
wald_encaixado(fit4,ind,bl0)
X=model.matrix(fit4)
escore_encaixado(fit0,ind,X)


# testar se beta_1 = beta1_0
fit4=glm(cpue ~ frota + ano + latitude, family=Gamma(link=log)) 
summary(fit4)
# testar se beta(latitude)=0.05   DEPOIS  MUDAR PARA 0.04, 0.03...
bl0=0.007#0.05
fit0=glm(cpue ~ frota + ano + offset(bl0*latitude), family=Gamma(link=log)) 
summary(fit0)
rv.gama(cpue,fit0,fit4)
testef(fit0,fit4)
betas=fit4$coefficients
ind=rep(0,length(betas))
ind[length(ind)]=1
ind
wald_encaixado(fit4,ind,bl0)
X=model.matrix(fit4)
escore_encaixado(fit0,ind,X)


# 4. Hip?teses Restritas
#Testar se efeitos de frotas s?o iguais
source("MLG/Funcoes/wald.txt")
fit5=glm(cpue ~ frota + ano + latitude, family=Gamma(link=log))
fit.model=fit5 
summary(fit5)
C1=rep(0,length(fit5$coefficients))  # efeito de anos 1997, 1998 e 1999 sao iguais?
C1[4]=1
C1[5]=-1/2
C1[6]=-1/2
C1
wald(modelo=fit.model,C=C1)  


# Teste t: beta_k=0
C1=cbind(0,0,0,0,0,0,1)
wald(modelo=fit.model,C=C1)

## default b0=0. mas se for algum elemento # 0, basta criar o vetor.
