envelope<-function(fit.model, tipo, ligacao, alfa=0.05)
{
  
  carregar_envel <- function(tipo) {
    # Lista de arquivos disponíveis
    arquivos <- c(
      "envel_bino_cauchit", "envel_bino_cloglog", "envel_bino_logit", 
      "envel_bino_probit", "envel_gama", "envel_gama_ident", 
      "envel_gama_inverse", "envel_gama_log", "envel_nbin", 
      "envel_nbin_ident", "envel_nbin_log", "envel_nbin_raiz", 
      "envel_ninv", "envel_ninv_ident", "envel_ninv_inverse", 
      "envel_ninv_inverse2", "envel_ninv_log", "envel_norm", 
      "envel_norm_inverse", "envel_norm_log", "envel_pois", 
      "envel_pois_ident", "envel_pois_log", "envel_pois_raiz", 
      "envel_bino"
    )
    
    # Verifica se o arquivo existe na lista
    if (!tipo %in% tipo) {
      stop("Arquivo não encontrado. Verifique o nome do arquivo.")
    }
    
    # Constrói o caminho completo do arquivo (assumindo que os arquivos estão no diretório atual)
    caminho_arquivo <- paste0("MLG/Funcoes/",tipo, ".txt")
    
    # Verifica se o arquivo existe no sistema de arquivos
    if (!file.exists(caminho_arquivo)) {
      stop(paste("Arquivo", caminho_arquivo, "não encontrado no diretório atual."))
    }
    
    # Carrega o arquivo com source
    source(caminho_arquivo)
    
  }
  
  fit.model<-fit.model
  carregar_envel(tipo)
  ind <- numeric(n)
  ind <- sort(tsi)<sort(e1) | sort(tsi)>sort(e2)
  prop.fora<-sum(ind)/n
  qualidade<-NULL
  if(prop.fora==0)
  {
    qualidade<-"Ótimo"
  }else if(prop.fora>0 & prop.fora<alfa/2)
  {
    qualidade<-"Bom"
  } else if((prop.fora>=alfa/2 & prop.fora<=alfa))
  {
    qualidade<-"Razoável"
    
  }else if((prop.fora>alfa))
  {
    qualidade<-"Ruim"
  }
  return(qualidade)
  
}
