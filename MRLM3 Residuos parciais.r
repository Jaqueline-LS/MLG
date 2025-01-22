rm(list=ls(all=TRUE))
# Fazer ajustes de modelos de regress?o verificar que vari?veis deveriam entrar no modelo

# Exemplo: Charnet, p?g. 285/286
dado=read.table(file="Tomografia.txt",header=T)
attach(dado)
y=tomo

par(mfrow=c(2,2))
plot(cintura,y)
plot(ultra1,y)
plot(ultra2,y)
plot(prega,y)

cor(dado) # Multicolinearidade modelo adequado ===> Regressão Ridge se quiser manter todas

# Gr?ficos res?duos parciais
#cintura ultra1 ultra2 prega
res_prega=lm(y~cintura+ultra1+ultra2)$residuals
res_ultra2=lm(y~cintura+ultra1+prega)$residuals
res_ultra1=lm(y~cintura+prega+ultra2)$residuals
res_cintura=lm(y~prega+ultra1+ultra2)$residuals

par(mfrow=c(2,2))
plot(res_prega,prega) # pelo grafico essa pode ser retirada do modelo
plot(res_ultra2,ultra2)
plot(res_ultra1,ultra1)
plot(res_cintura,cintura)
#
cor(res_prega,prega)
cor(res_ultra2,ultra2)
cor(res_ultra1,ultra1)
cor(res_cintura,cintura)


ajuste1=lm(res_prega~prega)
summary(ajuste1)

ajuste1=lm(res_ultra1~ultra1)
summary(ajuste1)

ajuste1=lm(res_ultra2~ultra2)
summary(ajuste1)

ajuste1=lm(res_cintura~cintura)
summary(ajuste1)


# Fazer um, step wise foward parcial pelos residuos
# A cada passo plotar o residuo contra a propria variavel que sera incluida para verificar se tem alguma relacao adicional




ajuste=lm(y~cintura+ultra1+ultra2+prega)
summary(ajuste)    # ver beta de prega
anova(ajuste)

fit=step(ajuste)  # grata surpresa

fit=step(ajuste,k=log(length(prega))) # BIC

# Gr?ficos Res?duos:
res_pad=rstandard(ajuste)
res_stud=rstudent(ajuste)
y_est=ajuste$fitted.values

par(mfrow=c(2,2))
plot(res_pad) # verificar independ?ncia dos erros
plot(res_stud) # Verificar qualidade do ajuste: pontos aberrantes (mal ajustados)
plot(res_pad,y_est) # Verificar Homogeneidade de vari?ncias ou Linearidade dos efeitos das var. explicativas
qqnorm(res_pad) # Normalidade
lines(c(-2,2),c(-2,2))


par(mfrow=c(2,2))
plot(cintura,res_pad) # Verificar se ainda existe rela??o entre X_k e y (n?o explicada por beta_k*X_k)
plot(ultra1,res_pad)
plot(ultra2,res_pad)
plot(prega,res_pad)

# Envelope simulado
fit.model=ajuste
source("envel_norm.txt")

# Refazer sem a var. prega
ajuste1=lm(y~cintura+ultra1+ultra2)
plot(ajuste1)

# Gr?ficos Res?duos:
res_pad=rstandard(ajuste1)
res_stud=rstudent(ajuste1)
y_est=ajuste1$fitted.values

par(mfrow=c(2,2))
plot(res_pad) # verificar independ?ncia dos erros
plot(res_stud) # Verificar qualidade do ajuste: pontos aberrantes (mal ajustados)
plot(res_pad,y_est) # Verificar Homogeneidade de vari?ncias ou Linearidade dos efeitos das var. explicativas
qqnorm(res_pad) # Normalidade
lines(c(-2,2),c(-2,2))

par(mfrow=c(2,2))
plot(cintura,res_pad) # Verificar se ainda existe rela??o entre X_k e y (n?o explicada por beta_k*X_k)
plot(ultra1,res_pad)
plot(ultra2,res_pad)

# Envelope simulado
fit.model=ajuste
source("envel_norm.txt")


# Fazer an?lise de diagn?stico

# Ind?cios de heterocedasticidade => Regress?o ponderada (Charnet, pag. 293)