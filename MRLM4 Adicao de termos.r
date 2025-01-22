rm(list=ls(all=TRUE))
# Gerar X%*%beta com logx, expx, etc...
n=100
sigma2=1
p=3
beta=matrix(c(5,2,-3),p,1)
X=matrix(1,n,p)
X[,2]=rexp(n,2)  # n?o importa a distribui??o. somente porque vou aplicar log depois nela
X[,3]=rnorm(n,1,1)  
#X22=log(X[,2])
#X22= X[,2]^2
X22= exp(X[,2])

erro=rnorm(n,0,sigma2)
y=X%*%beta+erro
y1=X%*%beta+ 3*X22 +erro
ajusteN=lm(y1~X)
summary(ajusteN)
resN=ajusteN$residuals
yest=ajusteN$fitted.values

par(mfrow=c(2,2))
plot(resN)
plot(resN,yest) # deu tend?ncia, verifique se ainda existe rela??o entre res?duos e Xk (linearidade)
plot(X[,2],resN)  # apresentou tend?ncia nesta. buscar poss?veis adi??es em X2
curve(exp,0,2.5, add=T, col="maroon2")
plot(X[,3],resN)

# modelo correto
ajustef=lm(y1~X[,2]+X[,3]+X22)
summary(ajustef)
resf=ajustef$residuals
yest=ajustef$fitted.values

par(mfrow=c(2,2))
plot(resf)
plot(resf,yest) 
plot(X[,2],resf)  
plot(X[,3],resf)


par(mfrow=c(1,2))
plot(resf)
plot(resf,yest)



# Gr?ficos Res?duos:
res=ajustef$residuals
res_pad=rstandard(ajustef)
res_stud=rstudent(ajustef)
y_est=ajustef$fitted.values

par(mfrow=c(2,2))
plot(res_pad) # verificar independ?ncia dos erros
plot(res_stud) # Verificar qualidade do ajuste: pontos aberrantes (mal ajustados)
plot(res_pad,y_est) # Verificar Homogeneidade de vari?ncias ou Linearidade dos efeitos das var. explicativas
qqnorm(res_pad) # Normalidade
lines(c(-2,2),c(-2,2))

# Envelope simulado
fit.model=ajustef
source("envel_norm.txt")


################################################################################
rm(list=ls(all=TRUE))
# Gr?fico da vari?vel adicionada (Gilberto, 2013, se??o 1.9.8)

dado=read.table("Combustivel2.txt",header=TRUE)
attach(dado)
names(dado)

## AJuste
fit0=lm(Con~Tax+Ren)

# Ser? que vale a pena adicionar 'Lic'?
# O problema ? que as var. explicativas sempre mant?m uma depend?ncia entre elas
# n?o ? apenas quest?o de entrar/testar mais uma vari?vel

res0=fit0$residuals

fit1= lm(Lic~Tax+Ren) # A candidata contra as preditoras no modelo base
res1=fit1$residuals # seria a contribuição pura da variavel lic, 

plot(res1,res0, xlab="Res?duo de Lic", ylab="Res?duo de Y")

summary(lm(res1 ~res0))
# Se gr?fico apresentar inclina??o, ? porque gamma # 0 => incluir nova vari?vel

fit0= lm(Con~Tax+Ren+Lic)
res0=fit0$residuals

fit1= lm(Rod~Tax+Ren+Lic)
res1=fit1$residuals

plot(res1,res0, xlab="Res?duo de Rod", ylab="Res?duo de Y")

fitr=lm(res0 ~ res1)
summary(fitr)

