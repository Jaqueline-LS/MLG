# Função principal para gerar gráficos de envelope
envelope <- function(tipo, fit.model) {

  # Cálculos comuns a todos os casos
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X) %*% W %*% X)
  H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
  h <- diag(H)
  td <- resid(fit.model, type = "deviance") / sqrt(1 - h)
  
  # Matriz para armazenar os envelopes
  e <- matrix(0, n, 100)
  
  # Geração dos envelopes
  for (i in 1:100) {
    dif <- runif(n) - fitted(fit.model)
    dif[dif >= 0] <- 0
    dif[dif < 0] <- 1
    nresp <- dif
    
    # Ajuste do modelo com base no tipo
    fit <- switch(tipo,
                  bino = glm(nresp ~ X, family = binomial),
                  bino_cauchit = glm(nresp ~ X, family = binomial(link = cauchit)),
                  bino_cloglog = glm(nresp ~ X, family = binomial(link = cloglog)),
                  bino_logit = glm(nresp ~ X, family = binomial),
                  bino_probit = glm(nresp ~ X, family = binomial(link = probit)),
                  gama = glm(nresp ~ X, family = Gamma),
                  stop("Tipo não reconhecido")
    )
    
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X) %*% W %*% X)
    H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
    h <- diag(H)
    e[, i] <- sort(resid(fit, type = "deviance") / sqrt(1 - h))
  }
  
  # Cálculo dos limites dos envelopes
  e1 <- numeric(n)
  e2 <- numeric(n)
  
  for (i in 1:n) {
    eo <- sort(e[i, ])
    e1[i] <- (eo[2] + eo[3]) / 2
    e2[i] <- (eo[97] + eo[98]) / 2
  }
  
  med <- apply(e, 1, mean)
  faixa <- range(td, e1, e2)
  
  # Plotagem do gráfico
  par(pty = "s")
  qqnorm(td, xlab = "Percentis da N(0,1)", ylab = "Componente do Desvio", ylim = faixa, pch = 16)
  par(new = T)
  qqnorm(e1, axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 1)
  par(new = T)
  qqnorm(e2, axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 1)
  par(new = T)
  qqnorm(med, axes = F, xlab = "", ylab = "", type = "l", ylim = faixa, lty = 2)
  
  
}
