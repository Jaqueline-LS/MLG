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
fit<-glm(cpue ~ frota + ano + trim + latitude + longitude, family=Gamma(link=log))
object<-fit
stepTesteF<-function(object, steps=1000)
{
  # Funções do Step
  cut.string <- function(string) 
  {
    if (length(string) > 1L) 
      string[-1L] <- paste("\n", string[-1L], sep = "")
    string
  }
  
  Terms <- terms(object)
  object$formula <- Terms
  fdrop <- numeric()
  fadd <- attr(Terms, "factors")
  models <- vector("list", steps)
  keep.list <- vector("list", steps)
  direction="backward"
  backward <- TRUE
  n <- nobs(object, use.fallback = TRUE)
  fit <- object
  nm <- 1
  Terms <- terms(fit)
  cat("Start: ", cut.string(deparse(formula(fit))), 
      "\n\n", sep = "")
  utils::flush.console()
  # Numero de graus de liberdade no modelo inicial
  bAIC <- extractAIC(fit, k=2)
  edf <- bAIC[1L]
  
  mydeviance <- function(x, ...) {
    x<-fit
    dev <- deviance(x)
    if (!is.null(dev)) 
      dev
    else extractAIC(x, k = 0)[2L]
  }
  models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
                         edf, change = "")
  
  
  
}

