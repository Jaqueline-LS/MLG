ResultadosNormal<-function(dados, modelo, ligacao, tipo, alfa=0.05)
{
  ligacao<-"identity"
  familia<-paste0("Normal-Link(",ligacao,")")
  n<-nrow(dados)
  fit1<- switch(ligacao,
                identity = glm(modelo, family=gaussian),
                log = glm(modelo, family=gaussian(link=log)),
                inverse = glm(modelo, family=gaussian(link=inverse)),
                stop("Link não reconhecido"))
  
  fit.model<-stepAIC(fit1,k=log(n), trace = 0)
  
  fit.j<-stepJAQ(fit1, trace = 0)
  
  if(deparse(formula(fit.model))==deparse(formula(fit.j)))
  {
    cat("BIC e Seleção via Teste F chegaram no mesmo modelo final!")
  }
  teste<-try(envelope(fit.j, tipo = tipo, ligacao = ligacao, alfa = alfa), silent = TRUE)
  
  aux<-sum(class(teste) != "try-error")
  if(aux==0)
  {
    qualidade<-"Não Rodou"
    bic<-as.numeric(round(AIC(fit.j,k=log(n)),6))
    return(list(Familia=familia,
                QQplot=qualidade,
                BIC=bic))
  }else{
    qualidade<-teste[[1]]
    class(teste[[2]])
    grafico<-recordPlot()
    return(list(Familia=familia,
                QQplot=qualidade,
                BIC=as.numeric(round(AIC(fit.j,k=log(n)),6)),
                grafico=grafico))
    
  }
  
}

modelo<-resistencia ~ voltagem + temperatura

Resultados1<-ResultadosNormal(dados = vidros, modelo, ligacao = "log", 
                              tipo="envel_norm_log")

Resultados2<-ResultadosNormal(dados = vidros, modelo, ligacao = "inverse",
                              tipo="envel_norm_inverse")

Resultados3<-ResultadosNormal(dados = vidros, modelo, ligacao = "identity",
                              tipo="envel_norm_inverse")
fit1<-glm(modelo, family=gaussian(link=inverse))
fit.j<-stepJAQ(fit1, trace = 0)
fit.model<-fit.j
source("MLG/Funcoes/envel_norm_inverse.txt")