
# Exemplo 1. Estudo entre escolaridade e renda (Exemplo 1 Gilberto)

censo <- scan("censo.dat", what=list(uf= " ", escolar=0, renda=0))
attach(censo)
names(censo)

plot(escolar,renda)
identify(escolar,renda,n=1)
uf[27]

fit <- lm(renda ~ escolar)
summary(fit)
yest= fit$fitted.values
res_stud=rstudent(fit)
res_pad=rstandard(fit)

# Qualidade do ajuste
plot(res_stud,ylab="Res?duo Studentizado")
identify(res_stud,n=1)

plot(yest,res_pad,xlab="Valores Ajustados", ylab="Res?duos Padronizados")
qqnorm(res_pad,xlab="Score Normal",ylab="Res?duo Padronizado")

fit.model <- fit
source("envel_norm.txt")
# identify(tsi): n?o consigo buscar quem ? o argumento, pois s? se tem qqnorm()


## AJUSTANDO UM MODELO COM LOG(RENDA)

plot(escolar,log(renda),xlab="Escolaridade", ylab="Log(Renda)")

fit <- lm(log(renda) ~ escolar)
summary(fit)
yest= fit$fitted.values
res_stud=rstudent(fit)
res_pad=rstandard(fit)

# Qualidade do ajuste
plot(res_stud,ylab="Res?duo Studentizado")
plot(yest,res_pad,xlab="Valores Ajustados", ylab="Res?duos Padronizados")
qqnorm(res_pad,xlab="Score Normal",ylab="Res?duo Padronizado")
lines(c(-2,2),c(-2,2))

fit.model <- fit
source("envel_norm.txt")

X=model.matrix(fit)
n=dim(X)[1]
p=dim(X)[2]

fit.res=influence.measures(fit)
fit.res
deffits=fit.res$infmat[,3] # conferir coluna
cook=fit.res$infmat[,5] # conferir coluna
Hii=fit.res$infmat[,6] # conferir coluna

corte=2*p/n
plot(Hii)
lines(1:n,corte*rep(1,n))
identify(Hii,n=1)

corte=pf(0.5,p,n-p)
plot(cook)
lines(1:n,corte*rep(1,n))

corte=2*sqrt(p/(n-p))
plot(deffits)
lines(1:n,corte*rep(1,n))

plot(cook)

# Influ?ncia global: Di
# p=length(fit$coefficients)
# Di=Hii*(res_pad^2)/(p*(1-Hii))
# plot(Di)

plot(escolar,log(renda))
lines(escolar,yest)
y1=log(renda)
c(escolar[27],y1[27])
identify(escolar,yest,n=1)

rm(list = ls())