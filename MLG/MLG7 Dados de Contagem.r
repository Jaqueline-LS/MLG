# Dados de Contagem: Poisson e Binomial Negativa(esta quando houver Superdispers?o)
## Modelo de Poisson: Exemplo das bacterias

Tempo=1:12
Num_Bacterias=c(175,108,95,82,71,50,49,31,28,17,16,11)

plot(Tempo,Num_Bacterias,xlab='Tempo',ylab='Sobreviventes',pch=19)

# 1?s tentativas de ajuste: Modelo Normal e Transforma??es
n=length(Tempo)
fit1=lm(Num_Bacterias ~ Tempo)
summary(fit1)
fit.model=fit1

plot(Tempo,Num_Bacterias,xlab='Tempo',ylab='Sobreviventes',pch=19)
lines(Tempo,fit1$fitted.values,lwd=2,col='grey')

source("envel_norm.txt")
AIC(fit1)
AIC(fit1,k=log(n))

T2=Tempo^2    
fit2=lm(Num_Bacterias ~ Tempo + T2)
summary(fit2)
fit.model=fit2

plot(Tempo,Num_Bacterias,xlab='Tempo',ylab='Sobreviventes',pch=19)
lines(Tempo,fit2$fitted.values,lwd=2,col='grey')

source("envel_norm.txt")
AIC(fit2) # melhorou, hein?
AIC(fit2,k=log(n))

fit3=lm(sqrt(Num_Bacterias) ~ Tempo)
summary(fit3)
fit.model=fit3
source("envel_norm.txt")
AIC(fit3)  # Uauuuuu
AIC(fit3,k=log(n))

fit4=lm(sqrt(Num_Bacterias) ~ Tempo + T2)
summary(fit4)
fit.model=fit4

plot(Tempo,sqrt(Num_Bacterias),xlab='Tempo',ylab='Sobreviventes',pch=19)
lines(Tempo,fit4$fitted.values,lwd=2,col='grey')

source("envel_norm.txt")
AIC(fit4)  # t? quase
AIC(fit4,k=log(n))

# com log
fit5=lm(log(Num_Bacterias) ~ Tempo)
summary(fit5)
fit.model=fit5

plot(Tempo,log(Num_Bacterias),xlab='Tempo',ylab='Sobreviventes',pch=19)
lines(Tempo,fit5$fitted.values,lwd=2,col='grey')

source("envel_norm.txt")
AIC(fit5)  
AIC(fit5,k=log(n))


# Modelos Poisson
fit0=glm(Num_Bacterias ~ 1, family=poisson)

fit5=glm(Num_Bacterias ~ Tempo, family=poisson)
summary(fit5)
fit.model=fit5
source("envel_pois_log.txt")   # ah!!!!!!!!
AIC(fit5) # Isso pode acontecer!
AIC(fit5,k=log(n))

fit6=glm(Num_Bacterias ~ Tempo + T2, family=poisson)
summary(fit6)
# T2 ? n?o significante. Nem precisa do termo quadratico
# AIC/BIC do modelo Poisson ? maior, mas o envelope revela que ele ajusta bem todos 
# os pontos, ao contr?rio do modelo normal!

# Ainda, modelo Normal com raiz vc transformou os dados; perde interpreta??o!!!

source("testef.txt")
testef(fit0,fit5)
testef(fit5,fit6)


# Modelo final de link log
fit5=glm(Num_Bacterias ~ Tempo, family=poisson)
b=fit5$coefficients
mu_est=exp(b[1]+b[2]*Tempo)
plot(Tempo,Num_Bacterias,pch=19)
lines(Tempo,mu_est)
# Interpreta??o (link=log)
# log(mu_i)=alpha + beta*x_i
# mu_i=exp(beta + beta1*x_i) Aqui jah temos uma curva; por isso o termo quadratico
# beta2*Tempo^2 eh desnecessario

# Teste de adequa??o da fun??o de liga??o
pl2 <- predict(fit5)^2
modelo2=glm(Num_Bacterias ~ Tempo + pl2, family=poisson) 
summary(modelo2)

source("testef.txt")
testef(fit5,modelo2)
# Conclus?o: teste F n?o rejeitou H0. Portanto, eta^2  n?o ? significativo. Logo, a fun??o de liga??o ? apropriada.




# Raiz
fit7=glm(Num_Bacterias ~ Tempo, family=poisson(link="sqrt"))
summary(fit7) # AIC pior que link log
b=fit7$coefficients
mu_est=(b[1]+b[2]*Tempo)^2
plot(Tempo,Num_Bacterias,pch=19)
lines(Tempo,mu_est)
# Interpreta??o (link=sqrt)
# sqrt(mu_i)=alpha + beta*x_i
# mu_i=(alpha + beta*x_i)^2 = alpha^2+ 2*alhpa*beta*x_i + beta^2*x_i^2
# ou seja, garanto que concavidade (beta^2) seja "para cima"
# Tamb?m, o termo quadratico beta2*Tempo^2 eh desnecessario



### Analise de residuos e diagnostico

fi=1
X <- model.matrix(fit5) 
V <- fitted(fit5)  # vetor de valores ajustados (model$fitted.values)
Vm <- diag(V) 
w <- fit5$weights  # vetor de pesos
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)  # vetor diagonal de H

# Res?duos
rD <- resid(fit5, type= "deviance")
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit5, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado
# LDi
LD = hii*(tS^2)/(1-hii) 

# Graficos
plot(tS,ylim=c(min(tS,-2),max(tS,2))) 
abline(h=2)
abline(h=-2)

plot(tS,V)

plot(LD)
identify(LD,n=2)

plot(Tempo,Num_Bacterias,pch=19)
lines(Tempo,V,lwd=2,col='grey')

plot(hii,pch=19)
plot(Tempo,hii,pch=19)


rm(list = ls())

#########################################################################################
#####################################################################################

## Modelo log-linear de Poisson para Tabelas de Conting?ncia (Apenas comentar!)
# Equivalente ao modelo log?stico para tabelas de Conting?ncia - Se??o 3.1 do Gilberto (2004)

############# Superdispers?o
# O fen?meno de superdispers?o, ocorre quando ? esperada uma distribui??o de 
# Poisson para a resposta, por?m a vari?ncia ? maior do que a resposta m?dia.

# Alternativa => modelo binomial negativo
library(MASS)
fit =glm.nb()
Liga??es: log, sqrt e identity

# Exemplo: Estudantes Australianos, Gilberto, pag. 170
# Ler o exemplo

head(quine) # na library MASS: Eth Sex Age Lrn Days
# ou
dado=scan("quine.dat", what=list(Etnia="character",Sexo="character",Esc="character",Desempenho="character",Dias_Ausente="numeric"))
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
n=length(Dias_Ausente)


# Ajuste Poisson
fit.pois=glm(Dias_Ausente~Etnia + sexo + Esc + Desempenho,family=poisson)
summary(fit.pois)  
fit.model=fit.pois
AIC(fit.pois)
AIC(fit.pois,k=log(n))
source("envel_pois_log.txt")

# Ajuste Binomial Negativo
fit.bn=glm.nb(Dias_Ausente~Etnia + sexo + Esc + Desempenho)
summary(fit.bn)
fit.model=fit.bn
AIC(fit.bn)
AIC(fit.bn,k=log(n))
source("envel_nbin_log.txt")

fit.AIC=stepAIC(fit.bn)   # com AIC
summary(fit.AIC)

fit.BIC=stepAIC(fit.bn,k=log(n))   # com BIC
summary(fit.BIC)

# Backward na mao
fit.bn=glm.nb(Dias_Ausente~Etnia + sexo + Esc + Desempenho)
summary(fit.bn) # Tira sexo

fit.bn=glm.nb(Dias_Ausente~Etnia + Esc + Desempenho)
summary(fit.bn) # Tira desempenho

fit.bn=glm.nb(Dias_Ausente~Etnia + Esc)
summary(fit.bn)

fit.bn=glm.nb(Dias_Ausente~Etnia)    ####### Mesmo resultado de sele??o via BIC
summary(fit.bn)
fit.model=fit.bn
source("envel_nbin.txt")

#####################################################
# Sele??o do melhor modelo:
tab=matrix(0,2,6)
colnames(tab)=c('Poisson_log','Poisson_ident','Poisson_sqrt','BN_log','BN_sqrt','BN_ident')
rownames(tab)=c('BIC','Envelope')
n=length(Dias_Ausente)

# Familia poisson: log, identity, and sqrt
# Link log
j=1
fit1=glm(Dias_Ausente~Etnia + sexo + Esc + Desempenho,family=poisson)
fit=stepAIC(fit1,k=log(n))
fit.model=fit
summary(fit)   
source('envel_pois_log.txt')
tab[1,j]=AIC(fit,k=log(n))
tab[2,j]='Ruim'

# Link identity
j=2
fit1=glm(Dias_Ausente~Etnia + sexo + Esc + Desempenho,family=poisson(link=identity))
fit=stepAIC(fit1,k=log(n))
fit.model=fit
summary(fit)  
source('envel_pois_ident.txt')
tab[1,j]=AIC(fit,k=log(n))
tab[2,j]='Ruim'

# Link sqrt
j=3
fit1=glm(Dias_Ausente~Etnia + sexo + Esc + Desempenho,family=poisson(link=sqrt))
fit=stepAIC(fit1,k=log(n))
summary(fit)  
fit.model=fit
source('envel_pois_raiz.txt')
tab[1,j]=AIC(fit,k=log(n))
tab[2,j]='Ruim'

# Familia Binomial Negativa: log, sqrt or identity.

# Link log
j=4
fit1=glm.nb(Dias_Ausente~Etnia + sexo + Esc + Desempenho)
fit=stepAIC(fit1,k=log(n))
summary(fit)  
fit.model=fit
source('envel_nbin.txt')
tab[1,j]=AIC(fit,k=log(n))
tab[2,j]='Otimo'

# Link sqrt
j=5
fit1=glm.nb(Dias_Ausente~Etnia + sexo + Esc + Desempenho,link=sqrt)
fit=stepAIC(fit1,k=log(n))
summary(fit)  
fit.model=fit
source('envel_nbin_raiz.txt')
tab[1,j]=AIC(fit,k=log(n))
tab[2,j]='Otimo'

# Link identity
j=6
fit1=glm.nb(Dias_Ausente~Etnia + sexo + Esc + Desempenho,link=identity)
tab[1,j]='NA'
tab[2,j]='N?o rodou'

tab # Embora modelos Poisson apresentem menor BIC, os envelopes sao ruins


############## Teste de adequa??o da fun??o de liga??o 
fit=glm.nb(Dias_Ausente~Etnia,link=log)
pl2 <- predict(fit)^2
modelo2=glm.nb(Dias_Ausente~Etnia + pl2,link=log) 
summary(modelo2)

table(Etnia,pl2)   # nao dah para fazer para uma unica variavel explicativa categorica


### Analise de residuos e diagnostico

fi=fit$theta
X <- model.matrix(fit) 
V <- fitted(fit)  # vetor de valores ajustados (model$fitted.values)
Vm <- diag(V) 
w <- fit$weights  # vetor de pesos
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)  # vetor diagonal de H

# Res?duos
rD <- resid(fit, type= "deviance")
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado
# LDi
LD = hii*(tS^2)/(1-hii) 

# Graficos
plot(tS,ylim=c(min(tS,-2),max(tS,2))) 
abline(h=2)
abline(h=-2)
hist(tS) # altamente assimetrico, devido a cauda longa dos dados
hist(Dias_Ausente)

plot(tS,V) # nao aplicavel, pois v.explic tem somente duas categorias

plot(LD)
identify(LD,n=2)
summary(Dias_Ausente)
Dias_Ausente[c(72,104)]
Etnia[c(72,104)]
tS[c(72,104)]
summary(tS)
sort(tS)

sort(Dias_Ausente[Etnia=='N'])

plot(hii)  # nao aplicavel, pois v.explic tem somente duas categorias

################################################################################
## fi para outros modelos
# Normal
fitn=glm(y~x,family=gaussian)
n=length(y)
fi=summary(fitn)$dispersion

# para conferir
X=model.matrix(fitn)
H=X%*%solve(t(X)%*%X)%*%t(X)
s2=1/(n-length(fitn$coefficients))*t(y)%*%(diag(n)-H)%*%y

# Poisson
fi=1

# Binomial
fi=n # length(y)

# gamma
fi=gamma.shape(fit)$alpha

# Binomial negativa
fi=fit.bn$theta



rm(list = ls())
