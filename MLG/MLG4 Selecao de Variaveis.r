
# outro metodo, ver como fizemos na aula MLG3 OU 2 de tirar uma a uma com os testes
source("StepBackwardF.r")

################ Sele??o de vari?veis
rm(list = ls())
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

fit1=glm(cpue ~ frota + ano +trim + latitude + longitude, family=Gamma(link=log))
library(MASS)
fitf=stepAIC(fit1, direction = "backward") # AIC
summary(fitf)

fitf=stepAIC(fit1,k=log(length(cpue))) # BIC
summary(fitf)

fitf=stepJAQ(fit1, alpha=0.05)

# Um tira muito o outro tira nada, 

# Fazer a seleção backward manual com o teste F

# Backward 'manual', via significance de 5%
fit1=glm(cpue ~ frota + ano +trim + latitude, family=Gamma(link=log))
summary(fit1)

# Criar dummies
ano1996=as.numeric(ano==1996)
ano1997=as.numeric(ano==1997)
ano1998=as.numeric(ano==1998)
ano1999=as.numeric(ano==1999)

trim2=as.numeric(trim==2)
trim3=as.numeric(trim==3)
trim4=as.numeric(trim==4)

fit1=glm(cpue ~ frota + ano1996 +ano1997 +ano1998 +ano1999 +trim2 +trim3+trim4 + latitude + longitude, family=Gamma(link=log))
summary(fit1)

fitf=stepAIC(fit1)
summary(fitf)

fitf=stepAIC(fit1,k=log(length(cpue)))
summary(fitf)

# Backward manual, 5%
fit1=glm(cpue ~ frota + ano1996 +ano1997 +ano1998 +ano1999 +trim2 +trim3+trim4 + latitude + longitude, family=Gamma(link=log))
summary(fit1)

fit1=glm(cpue ~ frota + ano1996 +ano1997 +ano1998 +trim2 +trim3+trim4 + latitude + longitude, family=Gamma(link=log))
summary(fit1)


fit1=glm(cpue ~ frota + ano1996 +ano1997 +trim2 +trim3+trim4 + latitude + longitude, family=Gamma(link=log))
summary(fit1)


fit1=glm(cpue ~ frota + ano1996 +ano1997 +trim3+trim4 + latitude + longitude, family=Gamma(link=log))
summary(fit1)

fit1=glm(cpue ~ frota + ano1996 +ano1997 +trim3+ latitude + longitude, family=Gamma(link=log))
summary(fit1)

fit1=glm(cpue ~ frota + ano1996 +ano1997 +trim3+ latitude, family=Gamma(link=log))
summary(fit1)

fit1=glm(cpue ~ frota + ano1996 +ano1997 +latitude, family=Gamma(link=log))
summary(fit1)

fit1=glm(cpue ~ frota + ano1997 +latitude, family=Gamma(link=log))
summary(fit1)

fit1=glm(cpue ~ frota + ano1997, family=Gamma(link=log))
summary(fit1)

###################################################
rm(list = ls())

dado <- read.table(file="MLG/Dados/grahanib.dat", header=F,col.names=c("num_grahani","num_total","periodo","comprim","largura","local"))
attach(dado)
num_grahani=as.numeric(num_grahani)
num_total=as.numeric(num_total)
n_controls=num_total-num_grahani

per <- factor(periodo)
per <- C(per,treatment)
comp <- factor(comprim)
comp <- C(comp,treatment)
larg <- factor(largura)
larg <- C(larg,treatment)
local1 <- factor(local)
local1 <- C(local1,treatment)

fit <- glm(cbind(num_grahani,n_controls) ~ per + comp + larg + local, family=binomial) # the binomial family the links logit(default), probit, cauchit(corresponding Cauchy CDFs), log and cloglog(complementary log-log)
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_bino.txt")

fit1 <- glm(cbind(num_grahani,n_controls) ~ per + comp + larg + local + larg*comp, family=binomial)
summary(fit1) # A interacao n foi significativa
fit.model=fit1
source("MLG/Funcoes/envel_bino.txt")

fitf=stepAIC(fit1) # AIC
summary(fitf)

fitf=stepAIC(fit1,k=log(length(n_controls))) # BIC
summary(fitf)


