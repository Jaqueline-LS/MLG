

####################### Ajustes de MLGs ##########################################
## AJuste

fit0=glm(y~ Var1 + Var2 + Var3..., family = )

family=
binomial(link = "logit")
links: logit, probit, cauchit, log and cloglog (complementary log-log); 

# testar com todas as fun??es de liga??o

# Testes de Hip?teses Assint?ticos
# Modelos Normal e Gamma -> teste F ? + indicado
# Modelos Log?stico: teste RV + indicado

## Testar Modelo (=~ANOVA) -> usar distribui??o X^2 da deviance do modelo ajustado

# Testando hip?teses simples
# TESTE F
# q: n? de restri??es; p: n? de par?metros
n=length(tipo)
F=((deviance(fit_h0)-deviance(fitH1))/q)/(deviance(fitH1)/(n-p))
F
p_valor=1-pchisq(F,q,n-p)
# Descri??o e detalhes:
# Esta fun??o calcula o valor da estat?stica F para testar dois modelos lineares generalizados ENCAIXADOS.

# Modelo Nulo: Teste g(y)=beta_0   (inicial -  sem var. explicativas)
# glm(y~1, family=...)
# graus de liberdade: n-1

###############################################################################

# Modelo Binomial: Ajuste, Interpreta??o, Testes de Hip?teses 
# (Teste pela Deviance e Teste F), Qualidade do Ajuste (envelope), Teste da Intera??o
# Sele??o de Vari?veis.


# Exemplo 1 (Gilberto): marketing do supermercado (Gilberto)
# Abrir arquivo "Aula Gilberto Logistica.pdf"
dado=read.table(file="MLG/Dados/Supermercado1.txt",header=T)
attach(dado)
dado
# Binomial: y pode ser um vetor de 0s e 1s ou uma matriz de 2 vetores, o 1? o n? de sucessos e o 2? o n? de fracassos
n_sucessos= Cupons_Usados
n_fracassos=Cupons_Enviados-Cupons_Usados
plot(Desconto,Cupons_Usados,pch=19)   

fit <- glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial)
summary(fit)
fit$family
names(fit)
pi_adj=fit$fitted.values # proporcoes estimadas
plot(Desconto,pi_adj)

deviance_nulo=fit$null.deviance
deviance_ajustado=fit$deviance
n=length(fit$y)

# Testar signific?ncia do modelo: ANOVA 
# 1. C?lculo do p_valor pela distrib. da deviance (X^2)
p=length(fit$coefficients)
p_valor=1-pchisq(deviance_ajustado,n-p)
p_valor
# ? NECESS?RIO TAMANHO DE AMOSTRA GRANDE. NESTE CASO, N=7 (PEQUENO)

# 2. Teste F
qa=1 # gl sob H0-gl sob H1 = (n-n? de parametros sob H0) - (n-n? de parametros sob H1)
# n? de parametros sob H1 - n? de parametros sob H0
F=((deviance_nulo-deviance_ajustado)/qa)/(deviance_ajustado/(n-p))
F
p_valor=1-pf(F,qa,n-p)
p_valor

# 2.1 Uso do programa "testef.txt"
fit0=glm(cbind(n_sucessos,n_fracassos)~1, family=binomial)
source("testef.txt")
testef(fit0,fit)

# 3. Teste RV
source('rv.poisson.binomial_simples.txt')
rv.glm(fit0,fit)
   


# Interpretando parametros + grafico
fit <- glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial)
fit$coefficients
exp(fit$coefficients)

X <- model.matrix(fit)
li=X%*%matrix(fit$coefficients)
mu=exp(li)/(1+exp(li))
propS=n_sucessos/Cupons_Enviados
plot(Desconto,propS,pch=19)
lines(Desconto,mu,lwd=2,col='grey')



# Sele??o do melhor modelo binomial: logit, probit, cauchit, cloglog (tem log, mas melhor n?o usar)
tab=matrix(0,2,4)
colnames(tab)=c('Logit','Probit','Cauchit','cloglog')
rownames(tab)=c('BIC','Envelope')
n=length(Desconto)

j=1
fit.model <- glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial)   # logit
source('envel_bino.txt')
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Otimo'

j=2
fit.model <- glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial(link=probit)) 
source('envel_bino_probit.txt')
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Otimo'

j=3
fit.model <- glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial(link=cauchit))  
source('envel_bino_cauchit.txt')
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Otimo'

j=4
fit.model <- glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial(link=cloglog))  
source('envel_bino_cloglog.txt')
tab[1,j]=AIC(fit.model,k=log(n))
tab[2,j]='Ruim'

tab    # Logito melhor modelo

############## Teste de adequa??o da fun??o de liga??o para o logito
fit.model=glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial)
pl2 <- predict(fit.model)^2
modelo2=glm(cbind(n_sucessos,n_fracassos)~Desconto + p12, family=binomial) 
summary(modelo2)

source("testef.txt")
testef(fit.model,modelo2)
# Conclus?o: teste F n?o rejeitou H0. Portanto, eta^2  n?o ? significativo. Logo, a fun??o de liga??o ? apropriada.



########################################################################################
# An?lise de res?duos e Diagnostico

# T?cnicas gr?ficas
#As t?cnicas gr?ficas mais recomendadas para os MLGs s?o as seguintes:
#(i) gr?ficos de 'tD_i' contra a ordem das observa??es, contra os valores ajustados e contra as vari?veis
#explicativas, ou contra o tempo ou alguma ordem em suspeita-se haver correla??o entre as observa??es;
#(ii) gr?fico normal de probabilidades para 'tD_i' com envelopes,
#(iii) gr?fico de 'z_i' contra 'eta_i' ou 'eta_i^2' para verificar a adequa??o da fun??o de liga??o
#(iv) gr?ficos de LDi e Di contra a ordem das observa??es.
# Os envelopes, no caso de MLGs com distribui??es diferentes da normal, s?o constru?dos com os res?duos sendo gerados a partir do modelo ajustado.




fit.model <- glm(cbind(n_sucessos,n_fracassos)~Desconto, family=binomial)
X <- model.matrix(fit.model)
V <- fitted(fit.model)  # no Binomial, sao as proporcoes estimadas
Vm <- diag(V)
w <- fit.model$weights  # vetor de pesos
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)  # vetor diagonal de H


# Res?duos
rD <- resid(fit.model, type= "deviance")
fi=summary(fit.model)$dispersion
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit.model, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado

# LDi
LD = hii*(tS^2)/(1-hii) # pode falhar se pi_i<0.1 ou pi_i>0.9. Neste caso, fazer Ldi x pi_i

################################ Graficos
plot(tD)
plot(V,tD)
plot(Desconto,tD)
# Envelope simulado
source("envel_bino.txt")

plot(hii)
plot(LD)


rm(list = ls())


#############################################################################################################

#Exemplo 2 (Gilberto): estudo para avaliar o poss?vel efeito cancer?geno do fungicida Avadex
# Abrir arquivo "Aula Gilberto Logistica.pdf", pag. 14
dado=read.table(file="Ratos.txt",header=T)
attach(dado)
n=length(Tumor)

# Vamos testar se existe intera??o
fit <- glm(Tumor~Sexo + Tratamento + Sexo*Tratamento, family=binomial)

# Teste t
summary(fit)

deviance_inter=fit$deviance
p=length(fit$coefficients)
 
fit0 <- glm(Tumor~Sexo + Tratamento, family=binomial)
summary(fit0)
deviance_nointer=fit0$deviance

# Teste F
source("testef.txt")
testef(fit0,fit)      
                                           
# Pela deviance da diferen?a (e com distrib. X^2)
Desvio=deviance_nointer-deviance_inter
Desvio
# g.l.: n? de par?metros sob intera??o - n? de par?metros sem intera??o=4-3=1
p_valor=1-pchisq(Desvio,1)
p_valor

# Pelo Teste RV
source('rv.poisson.binomial_simples.txt')
rv.glm(fit0,fit)

# Portanto, n?o existe intera??o!
library(MASS)
fitf=stepAIC(fit) 
summary(fitf)
   
# Qualidade do Ajuste: envelope simulado
fit.model=fitf
source("envel_bino_logit.txt")

### Testar Odds Ratio = 1 ? equivalente a testar os par?metros = 0


################### Mais testes (Assint?ticos)

# Se b#0 => usar offset na vari?vel
# se b=0 => usar TH para Hip?teses Simples (ajustar sem a vari?vel)
              
# graus de liberdade de RV, Wald e Escore: n? de restri??es (sob H0)
            

# Exemplo 2: Modelo: Tumor ~ b0 + b1*Sexo + b2*Tratamento; b0=-2; b1=-0.2 e b2=1

fit1 <- glm(Tumor~Sexo + Tratamento, family=binomial)
dev1=fit1$deviance

b0=-2
b1=-0.2
b2=1
beta0=c(b0,b1,b2)
fit0 <- glm(Tumor~0+offset(b0*rep(1,length(Tumor)))+offset(b1*Sexo) + offset(b2*Tratamento),family=binomial)
dev0=fit0$deviance

# Teste RV
source("rv.poisson.binomial_simples.txt")
rv.glm(fit0,fit1,fi=NULL)
                       
# Teste de Wald
source("Wald_Simples.txt")
wald_simples(modelo=fit1,b0=c(b0,b1,b2)) 
# conferindo teste de Wald
W=fit1$weights # vetor de pesos
X=model.matrix(fit1)
p=ncol(X)
phi=summary(fit1)$dispersion
phi
theta=fit1$coef
theta0=c(b0,b1,b2)
dif=as.vector(theta-theta0)
Xsi_Wald= phi*dif%*%t(X)%*%diag(W)%*%X%*%dif
Xsi_Wald
pvalue<-1-pchisq(Xsi_Wald,p)
pvalue

# Teste do Escore    
source("Escore_Simples.txt")
X1=model.matrix(fit1)  
escore_simples(fit0,X1)

# Teste F
source("testef.txt") 
testef(fit0,fit1)     


##### Hip?teses: Modelos Encaixados: Testar se Beta_1=b10 (subconjunto de beta)
# Se b10=0 (basta n?o incluir a s vari?veis. se b10 #0, usar offset)

# Exemplo 1: ANOVA: Beta (das vari?veis)=0 (sem beta_0)

# Teste RV
fit0 <- glm(Tumor~1, family=binomial)
dev0=fit0$deviance
fit1 <- glm(Tumor~Sexo + Tratamento, family=binomial)
dev1=fit1$deviance

source("rv.poisson.binomial.txt")
rv.glm(fit0,fit1,fi=NULL)
# p_valor menor que alpha => modelo ? adequado, significante

# Teste de Wald, equivalente ? ANOVA
source("Wald_Encaixado.txt")
ind1=c(0,1,1)
fit.model=fit1
wald_encaixado(modelo=fit.model,ind1,b0=NULL) 

# Teste do Escore para ANOVA
source("Escore_Encaixado.txt")
X=model.matrix(fit1)
escore_encaixado(fit0,ind1,X)

# Teste F 
testef(fit0,fit1)
                    


# Exemplo 2, Modelos encaixados: testar se a intera??o ? significativa: Beta(Sexo*Tratamento)=0
fit0 <- glm(Tumor~Sexo + Tratamento, family=binomial)
dev0=fit0$deviance
fit1 <- glm(Tumor~Sexo + Tratamento + Sexo*Tratamento, family=binomial)
dev1=fit1$deviance

# Teste RV  
source("rv.poisson.binomial.txt")
rv.glm(fit0,fit1,fi=NULL)

# Teste de Wald Encaixado
source("Wald_Encaixado.txt")
ind1=c(0,0,0,1) # vetor indicador de qual(uais) beta's est?(?o) sendo testado(s)
wald_encaixado(modelo=fit1,ind=ind1,b0=0)
                                  
# Teste de Escore Encaixado
source("Escore_Encaixado.txt")
ind1=c(0,0,0,1) # vetor indicador de qual(uais) beta's est?(?o) sendo testado(s)
X=model.matrix(fit1) # preciso da matriz do modelo irrestrito
escore_encaixado(modelo=fit0,ind=ind1,X)
                
# Teste F 
testef(fit0,fit1)




## Exemplo 3: testar se H0: beta_1=beta_1_0, beta_1 subconjunto de beta
# testar se beta(Tratamento)=2
b=2
fit0 <- glm(Tumor~Sexo + offset(b*Tratamento), family=binomial)
dev0=fit0$deviance
fit1 <- glm(Tumor~Sexo + Tratamento, family=binomial)
dev1=fit1$deviance

# Teste RV  
source("rv.poisson.binomial.txt")
rv.glm(fit0,fit1,fi=NULL)

# Teste de Wald
source("Wald_Encaixado.txt")
ind1=c(0,0,1) # vetor indicador de qual(uais) beta's est?(?o) sendo testado(s)
wald_encaixado(modelo=fit1,ind=ind1,b0=b)   

# Teste de Escore
source("Escore_Encaixado.txt")
ind1=c(0,0,1) # vetor indicador de qual(uais) beta's est?(?o) sendo testado(s)
X=model.matrix(fit1) # preciso da matriz do modelo irrestrito
escore_encaixado(modelo=fit0,ind=ind1,X)

# Teste F
testef(fit0,fit1)   


## Exemplo 4: Testar se beta(Sexo)=-0.5 e beta(Tratamento)=2
b=rbind(-0.5,2)
fit0 <- glm(Tumor~offset(-0.5*Sexo) + offset(2*Tratamento), family=binomial)
dev0=fit0$deviance
fit1 <- glm(Tumor~Sexo + Tratamento, family=binomial)
dev1=fit1$deviance

# Teste RV  
source("rv.poisson.binomial.txt")
rv.glm(fit0,fit1,fi=NULL)

# Teste de Wald
source("Wald_Encaixado.txt")
ind1=c(0,1,1) # vetor indicador de qual(uais) beta's est?(?o) sendo testado(s)
wald_encaixado(modelo=fit1,ind=ind1,b0=b)   

# Teste de Escore
source("Escore_Encaixado.txt")
ind1=c(0,1,1) # vetor indicador de qual(uais) beta's est?(?o) sendo testado(s)
X=model.matrix(fit1) # preciso da matriz do modelo irrestrito
escore_encaixado(modelo=fit0,ind=ind1,X)

# Teste F
testef(fit0,fit1)  
 

## OBSERVA??O: Para phi desconhecido, as estat?sticas mudam: 
## s? usaremos RV ("rv.gamma.txt") e Teste F (fun??o rv.gamma) (elimina phi)


##### Hip?teses restritas: C*Beta=0 (ou b, ao inv?s de 0)
# H0: C*Beta=0 x C*Beta #0
# Mais f?cil: Teste de Wald
# Estat.: phi*beta'*C'[C*(X'*W*X)*C']^{-1}*C*beta
#  coeficiente phi:
# Poisson = 1; Normal: 1/(sigma^2); Binomial: n; Gama: gamma.shape(fit.model)

# Contraste: C*Beta=0, onde sum(C)=0 (linha a linha)
# Exemplos de contrastes:
# wald(modelo,matrix(c(0,1,0,-1, 0,0,1,-1),nrow=2,ncol=4,byrow=T))
# wald(modelo,cbind(matrix(0,nrow=4,ncol=8),diag(4)))

# Exemplo 5: testar se efeito m?dio da intera??o ? igual ? soma dos efeitos principais
Cont=c(0,1/2,1/2,-1)
fit <- glm(Tumor~Sexo + Tratamento + Sexo*Tratamento, family=binomial)

# Teste RV, Teste do Escore e Teste F: Sem fun??es ainda para estimar sob H0
                         
# Teste de Wald
source("wald.txt")
wald(modelo=fit,Cont,b0=NULL)

# Exemplo: testar se efeito m?dio da intera??o ? igual ? soma dos efeitos principais e se efeito(tratamento)=efeito de intera??o 
Cont=matrix(c(0,1/2,1/2,-1, 0,0,1,-1),nrow=2,ncol=4,byrow=T)
wald(modelo=fit,Cont,b0=NULL)



# Interpretando par?metros
# Referencia: Femea e Controle (N?o-tratamento)
fit <- glm(Tumor~Sexo + Tratamento, family=binomial)
exp(fit$coefficients)
# Receber o tratamento (Fungicida Avadex) aumenta muito a chance de desenvolver tumor
# Os machos tem 21% de chance a menos de desenvolver tumor



# An?lise de res?duos e Diagnostico
fit.model <- glm(Tumor~Tratamento, family=binomial)
X <- model.matrix(fit.model)
V <- fitted(fit.model)  # no Binomial, sao as proporcoes estimadas
Vm <- diag(V)
w <- fit.model$weights  # vetor de pesos
W <- diag(w)
H1 <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H1%*%t(X)%*%sqrt(W)
hii <- diag(H)  # vetor diagonal de H


# Res?duos
rD <- resid(fit.model, type= "deviance")
fi=summary(fit.model)$dispersion
tD <- rD*sqrt(fi/(1-hii))   # residuo deviance padronizado
rp1 <- resid(fit.model, type= "pearson")
rP <- as.numeric(sqrt(fi)*rp1)
tS <- rP/sqrt(V*(1 - hii))  # Residuo padronizado

# LDi
LD = hii*(tS^2)/(1-hii) # pode falhar de pi_i<0.1 ou pi_i>0.9. Neste caso, fazer Ldi x pi_i

################################ Graficos
plot(tD)
plot(V,tD)
plot(Desconto,tD)
# Envelope simulado
source("envel_bino_logit.txt")

plot(hii)
plot(LD)


rm(list = ls())   
####################################################################################

# SELE??O DE VARI?VEIS
library(MASS)

## Exemplo: Estudo processo infeccioso pulmonar 


canc3 <- scan("canc3.dat", what=list(tipo=0, idade=0, sexo=0, hl=0,ff=0))
attach(canc3)
canc3
sexo <- C(sexo,treatment) # 1 Masculino, 2 Feminino
sexo
hl <- factor(hl) # 1:ausente, 2: discreta, 3: moderada, 4: intensa
hl <- C(hl,treatment)
ff <- factor(ff)
ff <- C(ff,treatment) # 1: ausente, 2: discreta, 3: moderada, 4: intensa
# Caselas de referencia (Intercepto): Masculino, Ausente em HL e FF

# relevel() # recodificar vari?veis categ?ricas


fit <- glm(tipo ~ sexo + idade + hl + ff, family=binomial)
summary(fit)
fit.AIC=stepAIC(fit)                
summary(fit.AIC)
fit.AIC$coefficients
exp(fit.AIC$coefficients)
# INTERPRETAR CADA PAR?METRO!!!

# via BIC
fit <- glm(tipo ~ sexo + idade + hl + ff, family=binomial)
summary(fit)
fit.BIC=stepAIC(fit,k=log(length(tipo)))                
summary(fit.BIC)
fit.BIC$coefficients
exp(fit.BIC$coefficients)



## SELECIONAR VARI?VEIS VIA AIC ? O "MESMO" QUE FAZER TESTES DE HIP?TESES H0: BETA_k=0
# (via BACKWARD)
            
## Realizando testes
# Vamos testar: H0: beta (ff)=0 x H1: beta(ff) ? diferente de 0
fit0 <- glm( tipo ~ sexo + idade + hl, family=binomial)
summary(fit0)
source("testef.txt")
testef(fit0,fit)

# TESTE RV
source("rv.poisson.binomial.txt")
rv.glm(fit0,fit,fi=NULL)

## Chegamos ?s mesmas conclus?es por AIC, teste F e teste RV 


# Envelope
fit.model=fit0
source("envel_bino_logit.txt")

# Previs?o para nova observa??o
betae=fit0$coefficients

ind1=c(1,1,100,0,0,1)  # Mulher, idade 100 e HL intensa (HL=histiocitos-linfocitos)
xib=sum(ind1*betae)
mui=exp(xib)/(1+exp(xib))
mui

ind1=c(1,1,100,0,0,0)  # Mulher, idade 100 e HL ausente
xib=sum(ind1*betae)
mui=exp(xib)/(1+exp(xib))
mui

ind1=c(1,0,60,1,0,0)  # Homem, idade 60 e HL discreto
xib=sum(ind1*betae)
mui=exp(xib)/(1+exp(xib))
mui



rm(list = ls())
