

###########################################################################

# An?lise de Res?duos e de Diagn?stico em MLG

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

fit.model=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=log))
library(MASS)
fi=gamma.shape(fit.model)$alpha
X <- model.matrix(fit.model)

V <- fitted(fit.model)  # vetor de valores ajustados (model$fitted.values)
Vm <- diag(V)

w <- fit.model$weights  # vetor de pesos
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)  # vetor diagonal de H

# Res?duos
rD <- resid(fit.model, type= "deviance")
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit.model, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado
# teste de normalidade dos res?duos
library(nortest)
lillie.test(rD) # DEVIANCE
lillie.test(tS)  # nao atingiu normalidade
lillie.test(rP) # não atingiu normalidade
lillie.test(tD)  # Geralmente, ? o mais pr?ximo da normalidade (tdi)

hist(rD)
hist(tS)
hist(tD)
# T?cnicas gr?ficas
#As t?cnicas gr?ficas mais recomendadas para os MLGs s?o as seguintes:
#(i) gr?ficos de 'tD_i' contra a ordem das observa??es, contra os valores ajustados e contra as vari?veis
#explicativas, ou contra o tempo ou alguma ordem em suspeita-se haver correla??o entre as observa??es;
#(ii) gr?fico normal de probabilidades para 'tD_i' com envelopes,
#(iii) gr?fico de 'z_i' contra 'eta_i' ou 'eta_i^2' para verificar a adequa??o da fun??o de liga??o
#(iv) gr?ficos de LDi e Di contra a ordem das observa??es.
# Os envelopes, no caso de MLGs com distribui??es diferentes da normal, s?o constru?dos com os res?duos sendo gerados a partir do modelo ajustado.

# Descri??o e detalhes:
# McCullagh e Nelder (1989), p?g.401, citam que uma forma de checar formalmente a m? especifica??o da fun??o de
# liga??o ou falta de covariadas no modelo ? adicionar o quadrado do preditor linear ajustado como uma covariada
# extra e analisar a queda no desvio.

# Gr?ficos
# Res?duos
plot(tD) # comparar com normal  (qualidade do ajuste)
plot(V,tD) # V: Valor ajustado
plot(longitude,tD)
plot(latitude,tD)

qqnorm(tD)

fit.model=lm(tD~1) # pegar o residuo como se ele fosse normal
source("envel_norm.txt")  # com modelo ajustado = fit.model

hist(tD,freq=F)
lines(sort(tD),dnorm(sort(tD)))

# Adequa??o da fun??o de liga??o
source("MLG/Funcoes/rv.gama.txt")
source("MLG/Funcoes/eta2.gama.txt")  # ABRIR FUN??O
fit.model=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=log))  # usei nome fit.model acima
eta2.gama(fit.model)  # tend?ncia linear indica adequa??o da liga??o

# OBS: VERIFICAR O PROBLEMA -> FUN??O RV.GAMA

# Caso d? problemas
# Ideia de Hinkley (1985): adicionar eta^2, eta=y estimado, ao modelo proposto.
pl2 <- predict(fit.model)^2
modelo2=glm(cpue ~ frota + ano + latitude + longitude +pl2, family=Gamma(link=log))  # mesma coisa!
summary(modelo2)

rv.gama(fit.model,modelo2) # MESMO WARNING!!! Erro
summary(fit.model)
summary(modelo2)
source("MLG/Funcoes/testef.txt")
testef(fit.model,modelo2)
# Conclus?o: teste F n?o rejeitou H0. Portanto, eta^2  n?o ? significativo. Logo, a fun??o de liga??o ? apropriada.



# Alunos: fazer testes de outras fun??es de liga??o.
fit.model=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=identity))  # usei nome fit.model acima
eta2.gama(fit.model)
# A identidade não seria adequada


fit.model=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=inverse))  # usei nome fit.model acima
eta2.gama(fit.model)
# ligação inversa também seria adequada, não rejeitou a hipótese nula



# Vari?vel adicionada 
source("MLG/Funcoes/adic.gama.txt")
fit.model=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=log))
summary(fit.model)

adic.gama(fit.model)  # desta forma, ele far? todos os gr?ficos testando todas as vari?veis  do modelo, uma a uma
# ser? feito o gr?fico dos res?duos do modelo sem elas contra o res?duo da regress?o delas pelas outras do modelo.
#latitude é importante para o modelo

# Exemplo: supor modelo ajustado: frota, ano e latitude. Queremos verificar a adi??o da vari?vel longitude
fit.model=glm(cpue ~ frota + ano + latitude, family=Gamma(link=log))
adic.gama(modelo=fit.model, var=as.matrix(longitude))

fit.model=glm(cpue ~ frota + ano + longitude, family=Gamma(link=log))
adic.gama(modelo=fit.model, var=as.matrix(latitude))

fit.model=glm(cpue ~ frota + ano + latitude + longitude, family=Gamma(link=log))

# Diagn?sticos
# Pontos de Alavanca
plot(V,hii) # os pontos de alavanca nao são valores extremos no predito

plot(hii)
ind=identify(hii)

X<-model.matrix(fit.model)

X[1:4,]
summary(latitude) # As 4 observações são o menores valores de long e lat
summary(longitude)

cpue[1:4] # Valores de y para as 4 obs
summary(cpue)
# Como imaginado elas não são outliers na resposta

# Influ?ncia - Dele??o de Casos
# Afastamento pela log-verossimilhan?a
LD = hii*(tS^2)/(1-hii)
plot(LD)
identify(LD) 

X[c(8,17,40,52),]
summary(latitude)
summary(longitude)
# não sao discrepantes no X


cpue[c(c(8,17,40,52))]
summary(cpue)
which(cpue==600)
X[83,]
tD[83]
max(tD)
which(tD==max(tD)) # o maior residuo é a observação 17


# Dist?ncia de Cook: usar a fun??o diag.'modelo'.txt


# fun??o mais completa, porém é melhor fazer o passo a passo acima com os gráficos e medidas calculadas anteriormente
source('MLG/Funcoes/diag.gama.txt')   # ABRIR FUN??O
diag.gama(fit.model)

