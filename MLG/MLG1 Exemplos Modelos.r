## Dados com Resposta Positiva
?glm

# Exemplo: Dados da frota, peixe-batata
pesca <- read.table(file="./MLG//Dados/pesca.dat",header=F,col.names=c("frota","ano","trimestre","latitude","longitude","dias","captura","cpue"))
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

# é uma variavel positiva!!!

# Modelo Gamma: 
fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma) # the Gamma family the links inverse(default), identity and log
fit
summary(fit)
names(summary(fit))
summary(fit)$family

# Envelope
fit.model=fit
source("MLG/Funcoes/envel_gama_inverse.txt") 


fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=log)) 
summary(fit)   
# Envelope
fit.model=fit
source("MLG/Funcoes/envel_gama_log.txt") 

fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=identity)) 
summary(fit)   
# Envelope
fit.model=fit
source("MLG/Funcoes/envel_gama_ident.txt") 

plot(fitted(fit))
# problema nao foi no mu.hat 
# Pode ter sido no calculo de deviance

# Gilberto
fit=glm(cpue ~ frota + ano + ano*frota + latitude + longitude, family=Gamma(link=log)) 
summary(fit)   
# Envelope
fit.model=fit
source("MLG/Funcoes/envel_gama_log.txt") 



######## Modelo Normal
#gaussian family accepts the links (as names) identity(Default), log and inverse;

fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=gaussian)
fit.model=fit
source("MLG/Funcoes/envel_norm.txt")

fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=gaussian(link=log))
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_norm_log.txt")

fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=gaussian(link=inverse))
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_norm_inverse.txt")



# Modelo Normal inversa
# inverse.gaussian family the links 1/mu^2 (default), inverse, identity and log.
fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian) 
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_ninv_inverse2.txt")

plot(fitted(fit))

fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian(link=inverse))
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_ninv_inverse.txt")


fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian(link=log))
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_ninv_log.txt")

fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=inverse.gaussian(link=identity))
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_ninv_ident.txt")



# Interpretacao
fit=glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=log)) # the Gamma family the links inverse(default), identity and log
fit=summary(fit)
betas=fit$coefficients[,1]
exp(betas)

# ser de ubatuba diminui 20 por cento em relacao as outras frotas
# latitude é melhor aumentar a latitude

fit=glm(cpue ~ frota + ano + trim + latitude + longitude + frota*latitude, family=Gamma(link=log)) # the Gamma family the links inverse(default), identity and log
fit=summary(fit)
betas=fit$coefficients[,1]
exp(betas)

# Sair de ubatuba reduz o efeito de latitude, não aproveita o ganho, ja esta quase no topo

#############################################################################################

########################### Modelo Binomial
# Exerc. 21, Cap. 3  Gilberto Paula, 2013

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

fit <- glm(cbind(num_grahani,n_controls) ~ per + comp + larg + local1, family=binomial) # the binomial family the links logit(default), probit, cauchit(corresponding Cauchy CDFs), log and cloglog(complementary log-log)
summary(fit)
exp(fit$coefficients)
# Interpretação em termos de chances no caso da ligação logit

# Invertendo o objetivo, falar de opalinus
fit <- glm(cbind(n_controls,num_grahani) ~ per + comp + larg + local1, family=binomial) # the binomial family the links logit(default), probit, cauchit(corresponding Cauchy CDFs), log and cloglog(complementary log-log)
summary(fit)
exp(fit$coefficients)

fit.model=fit
source("MLG/Funcoes/envel_bino_logit.txt")
# O evelope é esquisito mesmo

fit <- glm(cbind(num_grahani,n_controls) ~ per + comp + larg + local1, family=binomial(link=probit))   # testar outras
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_bino_probit.txt")

fit <- glm(cbind(num_grahani,n_controls) ~ per + comp + larg + local1, family=binomial(link=cloglog))   # testar outras
summary(fit)
fit.model=fit
source("MLG/Funcoes/envel_bino_cloglog.txt")

# Warning problemas na deviance

# coment?rio help do R
## Binomial with identity link: often not a good idea.

# melhor não usar as ligações identidade

###########################################################################################

# Dados de Contagem: Poisson e Binomial Negativa(esta quando houver Superdispers?o)
# Exemplo das bacterias, Gilberto Paula (2013), se??o 1.12, pag. 88

Tempo=1:12
Num_Bacterias=c(175,108,95,82,71,50,49,31,28,17,16,11)

plot(Tempo,Num_Bacterias,xlab='Tempo',ylab='Sobreviventes',pch=19)

# 1?s tentativas de ajuste: Modelo Normal e Transforma??es

fit1=lm(Num_Bacterias ~ Tempo)
summary(fit1)
fit.model=fit1
source("envel_norm.txt")
AIC(fit1)

T2=Tempo^2    
fit2=lm(Num_Bacterias ~ Tempo + T2)
summary(fit2)
fit.model=fit2
source("envel_norm.txt")
AIC(fit2)

fit3=lm(sqrt(Num_Bacterias) ~ Tempo)
summary(fit3)
fit.model=fit3
source("envel_norm.txt")
AIC(fit3)  # melhorou...

fit4=lm(sqrt(Num_Bacterias) ~ Tempo + T2)
summary(fit4)
fit.model=fit4
source("envel_norm.txt")
AIC(fit4)  # melhorou mais... # Em termos de modelo normal esse é o melhor que consegue fazer

# Modelos Poisson
fit1=glm(Num_Bacterias ~ Tempo, family=poisson)   # poisson family the links log (default), identity, and sqrt
summary(fit1)
fit.model=fit1
source("MLG/Funcoes/envel_pois_log.txt")   # OK, mas AIC maior que Normal
AIC(fit1)  
fit2=glm(Num_Bacterias ~ Tempo + T2, family=poisson)
summary(fit2)   # T2 n?o significativo, embora diminua AIC
fit.model=fit2
source("MLG/Funcoes/envel_pois_log.txt") 
AIC(fit2)  

fit3=glm(Num_Bacterias ~ Tempo, family=poisson(link="sqrt"))
summary(fit3)  
fit.model=fit3
source("MLG/Funcoes/envel_pois_raiz.txt") # tem uma tendencia aqui
AIC(fit3)  

fit4=glm(Num_Bacterias ~ Tempo+T2, family=poisson(link="sqrt"))
summary(fit4)   # nesta liga??o, T2 ? significativa e ainda ajudou a diminuir AIC
fit.model=fit4
source("MLG/Funcoes/envel_pois_raiz.txt") 
AIC(fit4)  


fit5=glm(Num_Bacterias ~ Tempo, family=poisson(link="identity"))
summary(fit5)  
fit.model=fit5
source("MLG/Funcoes/envel_pois_ident.txt") 
AIC(fit5)

fit6=glm(Num_Bacterias ~ Tempo+T2, family=poisson(link="identity"))
summary(fit6)   # nesta liga??o, T2 ? significativa e ainda ajudou a diminuir AIC
fit.model=fit6
source("MLG/Funcoes/envel_pois_ident.txt") 
AIC(fit6)

# vamos ficar com fit1
# g(mu_i)=x_i^T*beta

b=fit1$coefficients
mu_est=exp(b[1]+b[2]*Tempo)
plot(Tempo,Num_Bacterias,pch=19)
lines(Tempo,mu_est)
# Interpreta??o (link=log)
# log(mu_i)=alpha + beta*x_i
# mu_i=exp(alpha + beta*x_i)

###### Modelo alternativo => binomial negativa
library(MASS)
?glm.nb()
mean(Num_Bacterias)
var(Num_Bacterias)
sqrt(var(Num_Bacterias))

fit1=glm.nb(Num_Bacterias ~ Tempo)   #  Liga??es: log, sqrt e identity
summary(fit1)
fit.model=fit1
source("MLG/Funcoes/envel_nbin_log.txt")

fit2=glm.nb(Num_Bacterias ~ Tempo+T2) 
summary(fit2)   # T2 n?o signifitcativo e aumentou AIC
fit.model=fit2
source("MLG/Funcoes/envel_nbin_log.txt")

fit3=glm.nb(Num_Bacterias ~ Tempo,link=sqrt)
summary(fit3)
fit.model=fit3
source("MLG/Funcoes/envel_nbin_raiz.txt")

fit4=glm.nb(Num_Bacterias ~ Tempo+T2,link=sqrt) 
summary(fit4)   # T2 significativo e diminuiu AIC
fit.model=fit4
source("MLG/Funcoes/envel_nbin_raiz.txt")

fit5=glm.nb(Num_Bacterias ~ Tempo,link=identity)
summary(fit5)
fit.model=fit5
source("MLG/Funcoes/envel_nbin_ident.txt")

fit6=glm.nb(Num_Bacterias ~ Tempo+T2,link=identity) 
summary(fit6)
fit.model=fit6
source("MLG/Funcoes/envel_nbin_ident.txt")

# Poisson com link log foi melhor

rm(list = ls())


# Exemplo: Estudantes Australianos, Gilberto Paula, se??o 4.3.6, pag. 312
# Ler o exemplo                                                  
library(MASS)
quine # ver os dados no R: Eth Sex Age Lrn Days
dado=scan("MLG/Dados/quine.dat", what=list(Etnia="character",Sexo="character",Esc="character",Desempenho="character",Dias_Ausente="numeric"))
attach(dado)
Etnia <- factor(Etnia)
Etnia <- C(Etnia,treatment)
sexo <- factor(Sexo)
sexo <- C(sexo,treatment)
Esc <-factor(Esc)
Esc <- C(Esc,treatment)
Desempenho <- factor(Desempenho)
Desempemho <- C(Desempenho,treatment)
Dias_Ausente=as.numeric(Dias_Ausente)

# Ajuste Poisson
fit.pois=glm(Dias_Ausente~Etnia + sexo + Esc + Desempenho,family=poisson)
summary(fit.pois)  
fit.model=fit.pois
source("MLG/Funcoes/envel_pois_log.txt")

# Ajuste Binomial Negativo
fit.bn=glm.nb(Dias_Ausente~Etnia + sexo + Esc + Desempenho)
summary(fit.bn)
fit.model=fit.bn
source("MLG/Funcoes/envel_nbin_log.txt")

fit.bn=glm.nb(Dias_Ausente~Etnia + Esc)
summary(fit.bn)
fit.model=fit.bn
source("MLG/Funcoes/envel_nbin_log.txt")



