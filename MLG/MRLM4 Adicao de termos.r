rm(list=ls(all=TRUE))
# Gerar X%*%beta com logx, expx, etc...
n=100
sigma2=1
p=3
beta=matrix(c(5,2,-3),p,1)
X=matrix(1,n,p)
X[,2]=rexp(n,2)  # não importa a distribuição. somente porque vou aplicar log depois nela
X[,3]=rnorm(n,1,1)  
X22=log(X[,2])
#X22= X[,2]^2
erro=rnorm(n,0,sigma2)
y=X%*%beta+erro
y1=X%*%beta+ 3*X22 +erro
ajusteN=lm(y1~-1+X)
summary(ajusteN)
resN=ajusteN$residuals
yest=ajusteN$fitted.values

par(mfrow=c(2,2))
plot(resN)
plot(resN,yest) # deu tendência, verifique se ainda existe relação entre resíduos e Xk (linearidade)
plot(X[,2],resN)  # apresentou tendência nesta. buscar possíveis adições em X2
plot(X[,3],resN)

# modelo correto
ajustef=lm(y1~X[,2]+X[,3]+X22)
summary(ajustef)
resf=ajustef$residuals
yest=ajustef$fitted.values

par(mfrow=c(1,2))
plot(resf)
plot(resf,yest)



# Gráficos Resíduos:
res=ajustef$residuals
res_pad=rstandard(ajustef)
res_stud=rstudent(ajustef)
y_est=ajustef$fitted.values

par(mfrow=c(2,2))
plot(res_pad) # verificar independência dos erros
plot(res_stud) # Verificar qualidade do ajuste: pontos aberrantes (mal ajustados)
plot(res_pad,y_est) # Verificar Homogeneidade de variâncias ou Linearidade dos efeitos das var. explicativas
qqnorm(res_pad) # Normalidade
lines(c(-2,2),c(-2,2))

# Envelope simulado
fit.model=ajustef
source("envel_norm.txt")


################################################################################
rm(list=ls(all=TRUE))
# Gráfico da variável adicionada (Gilberto, 2013, seção 1.9.8)

dado=read.table("Combustivel2.txt",header=TRUE)
attach(dado)
names(dado)

## AJuste
fit0=lm(Con~Tax+Ren)

# Será que vale a pena adicionar 'Lic'?
# O problema é que as var. explicativas sempre mantém uma dependência entre elas
# não é apenas questão de entrar/testar mais uma variável

res0=fit0$residuals

fit1= lm(Lic~Tax+Ren)
res1=fit1$residuals

plot(res1,res0, xlab="Resíduo de Lic", ylab="Resíduo de Y")

summary(lm(res1 ~res0))
# Se gráfico apresentar inclinação, é porque gamma # 0 => incluir nova variável

fit0= lm(Con~Tax+Ren+Lic)
res0=fit0$residuals

fit1= lm(Rod~Tax+Ren+Lic)
res1=fit1$residuals

plot(res1,res0, xlab="Resíduo de Rod", ylab="Resíduo de Y")

fitr=lm(res0 ~ res1)
summary(fitr)

