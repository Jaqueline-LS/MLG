
testef<-function(fit0,fit1) {
  # Descricaoo e detalhes:
  # Esta fun??o calcula o valor da estat?stica F para testar dois modelos lineares generalizados encaixados.
  #
  # S? ? adequado fazer o teste se o par?metro de forma for grande (para gama e normal inversa) ou conhecido
  # (poisson e binomial) ou no caso normal linear (que ? um teste exato).
  #
  # Os dados devem estar dispon?veis pelo comando attach( ).
  #
  # Argumentos obrigat?rios:
  # fit0: ajuste do modelo sob H0;
  # fit1: ajuste do modelo sob H1.
  #
  # A sa?da ser? o valor da estat?stica F juntamente com os graus de liberdade e o n?vel descritivo.
  #
  # Autor: Frederico Zanqueta Poleto <fred@poleto.com>, arquivo dispon?vel em http://www.poleto.com
  #
  # Refer?ncia:
  # PAULA, G. A. (2003). Modelos de Regress?o com apoio computacional. IME-USP, S?o Paulo. [N?o publicado,
  #    dispon?vel em http://www.ime.usp.br/~giapaula/Book.pdf]
  #
  # Exemplo:
  # testef(ajuste.sobH0,ajuste.sobH1)
  #
  
  if( (class(fit0)[1]=="lm" | class(fit0)[1]=="glm") & (class(fit1)[1]=="lm" | class(fit1)[1]=="glm") & (class(fit0)[1]==class(fit1)[1]) ) {
    
  } else {
    stop(paste("\nA classe dos dois objetos deveriam iguais (lm ou glm) !!!\n"))
  }
  if(class(fit0)[1]=="glm") {
    if(fit0$family[[1]] != fit1$family[[1]]) {
      stop(paste("\nAs familias dos objetos deveriam ser as mesmas !!!\n"))
    }
    if(fit0$family$link != fit1$family$link) {
      stop(paste("\nAs funçõees de ligação dos objetos deveriam ser as mesmas !!!\n"))
    }
  }
  
  dev0<-deviance(fit0)
  dev1<-deviance(fit1)
  df0<-summary(fit0)$df[2]
  df1<-summary(fit1)$df[2]
  
  f<-((dev0-dev1)/(df0-df1))/(dev1/df1)
  
  pvalue<-1-pf(f,df0-df1,df1)
  
  list(f=f,df1=df0-df1,df2=df1,pvalue=pvalue)
}


stepJAQ<- function(object, alpha = 0.05, trace = 1, steps = 1000) {
  
  # Função para calcular o p-valor do teste F
  get_pvalue <- function(fit, term) {

    reduced_model <- try(update(fit, paste("~ . -", term)), silent = T)
    aux<-sum(class(reduced_model) != "try-error")
    if(aux==0)
    {
      stop(paste0("Função glm não rodou sem o termo ( ", term," )===>",  reduced_model))

    }else{
      result.testF <- testef(reduced_model,fit)
      pvalue <- result.testF$pvalue
    }
    return(pvalue)

    
  
   
  }
  
  # Inicialização
  fit <- object

  if(trace)
  {
    cat("\nModelo inicial:", deparse(formula(fit)),"\n")
  }
  # Loop de seleção backward
  while (steps > 0) {
    steps <- steps - 1
    scope <- drop.scope(fit) # Conjunto de variáveis no modelo atual
    
    if (length(scope) == 0) break  # Não há mais variáveis para remover
    
    # Avaliar p-valor para cada variável que ainda esta no modelo
    pvalues <- sapply(scope, function(term) get_pvalue(fit, term))
    # Encontrar a variável com o maior p-valor
    max_pvalue <- max(pvalues)
    term_to_remove <- scope[which.max(pvalues)]
    
    if (max_pvalue > alpha) {
      # Remover a variável com o maior p-valor
      fit <- update(fit, paste("~ . -", term_to_remove))
      if (trace) {
        cat("\nVariável removida:", term_to_remove, "( p-valor =", max_pvalue,")\n")
        cat("Novo modelo:", deparse(formula(fit)), "\n")
      }

    } else {
      break  # Nenhuma variável com p-valor maior que alpha
    }
  }
  if(trace)
  {
    cat("\nModelo selecionado:", deparse(formula(fit)),"\n")
  }
  return(fit)
}


