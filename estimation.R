# Displaced Geometric -----------------------------------------------------

displaced_Geometric <- function(q){
  -((M - N)*log(1 - q) + N*log(q))
}

getGeometricMLE <- function(qInitValue, lowerBound, upperBound = c(Inf)) {
  mle_geometric <- NULL  
  tryCatch( {
    mle_geometric <- mle(displaced_Geometric,
                         start = list(q = qInitValue),
                         method = "L-BFGS-B",
                         lower = lowerBound,
                         upper = upperBound) 
  },
  error = function(e) {      
   # print(e$message)
   # cat("\n")
  }
  )
  return(mle_geometric)
}

# Displaced Poisson -------------------------------------------------------
get_C <- function(data){
  result <- 0
  for(i in 1:N){  
    part <- 0
    if(data$V1[i] == 1){
      part <- part + log(2)
    }
    else{
      for(j in 2:data$V1[i]){      
        part <- part + log(j)        
      }
    }    
    result <- result + part    
  }
  return(result)
}

displaced_Poisson <- function(lambda){
  C <- get_C(data)
  -(M*log(lambda) - N*(lambda + log(1 - exp(-lambda))) - C)
}

getPoissonMLE <- function(pInitValue, lowerBound, upperBound = c(Inf)) {
  mle_poisson <- NULL  
  tryCatch( {
    mle_poisson <- mle(displaced_Poisson,
                       start = list(lambda = pInitValue),
                       method = "L-BFGS-B",
                       lower = lowerBound, 
                       upper = upperBound)
  },
  error = function(e) {      
   print(e$message)
   cat("\n")
  }
  )
  return(mle_poisson)
}

# Zeta fixed parameter ----------------------------------------------------

zeta_Distribution_Fixed_Gamma <- function(){
  -(-2*M_prime - N*log((pi^2)/6))
}

# Zeta distribution -------------------------------------------------------

zeta_Distribution <- function(gamma){
  -( -gamma*M_prime - N*log(zeta(gamma)))
}

getZetaMLE <- function(gInitValue, lowerBound, upperBound = c(Inf)) {
  mle_zeta <- NULL  
  tryCatch( {
    mle_zeta <- mle(zeta_Distribution,
                    start = list(gamma = gInitValue),
                    method = "L-BFGS-B",
                    lower = lowerBound,
                    upper = upperBound)
  },
  error = function(e) {      
   # print(e$message)
  #  cat("\n")
  }
  )
  return(mle_zeta)
}

# Right-truncated zeta ----------------------------------------------------

get_Harmonic <- function(gamma, kMax) {
  result <- 0   
  for (i in 1:kMax){
    result <- result + 1/(i^gamma) 
  }
  return(result)
}

right_Truncated_Zeta <- function(gamma, kMax){
  harmonic <- get_Harmonic(gamma, kMax)
  -(-gamma*M_prime - N*log(harmonic))
}

getRightTruncZetaMLE <- function(gInitValue, kmaxInitValue, lowerBound, upperBound = c(Inf, Inf)) {
  mle_right_truncated_zeta <- NULL  
  tryCatch( {    
    mle_right_truncated_zeta <- mle(right_Truncated_Zeta,
                                    start = list(gamma = gInitValue, kMax = kmaxInitValue),
                                    method = "L-BFGS-B",
                                    lower = lowerBound,
                                    upper = upperBound)
  },
  error = function(e) {      
   # print(e$message)
  #  cat("\n")
  }
  )
  return(mle_right_truncated_zeta)
}

# Altmann Distribution ----------------------------------------------------

get_F_Altmann <- function(gamma, delta){
  result <- 0
  for(k in 1:N){
    result <- ((k^(-gamma)) * exp(-delta * k)) + result
  }
  return(log(result))
}

altmann_Distribution <- function(gamma, delta){
  f <- get_F_Altmann(gamma, delta)
  -(-(f*N + gamma*M_prime + delta*M))
}

getAltmannMLE <- function(gInitValue, dInitValue, lowerBound, upperBound = c(Inf, Inf)) {
  mle_Altmann <- NULL  
  tryCatch( {    
    mle_Altmann <- mle(altmann_Distribution,
                       start = list(gamma = gInitValue, delta = dInitValue),
                       method = "L-BFGS-B", 
                       lower = lowerBound,
                       upper = upperBound)  
  },
  error = function(e) {      
    #print(e$message)
    #cat("\n")
  }
  )
  return(mle_Altmann)
}

# MOEZipf Distribution ----------------------------------------------------

QValue <- function(gamma, delta, d){
  log(zeta(gamma) - (1-delta)*zeta_x(gamma, d))  
}

MOEZipf <- function(gamma, delta){
  -(N*log(delta) + N*log(zeta(gamma)) - gamma*M_prime 
    - sum(freq * sapply(degrees, QValue, gamma=gamma, delta=delta))
    - sum(freq * sapply(degrees + 1, QValue, gamma=gamma, delta=delta))
  )
}

getMOEZipfMLE <- function(gInitValue, dInitValue, lowerBound, upperBound = c(Inf, Inf)) {
  mle_MOEZipf <- NULL  
  tryCatch( {    
    mle_MOEZipf <- mle(MOEZipf,
                       start = list(gamma = gInitValue, delta = dInitValue),
                       method = "L-BFGS-B", 
                       lower = lowerBound,
                       upper = upperBound)    
  },
  error = function(e) {      
    #print(e$message)
    #cat("\n")
  }
  )
  return(mle_MOEZipf)
}
  

# Negative Binomial -------------------------------------------------------

NB <- function(gamma, p){
  P <- sum(lgamma(gamma + degrees) * freq)
  S <- sum(lfactorial(degrees)* freq)
  -(P - S - N*log(gamma(gamma)) + M*log(p) + gamma*N*log(1-p))  
}
Nb <- function(gamma, p){
  P <- sum(lgamma(gamma + degrees) * freq)
  S <- sum(lfactorial(degrees)* freq)
  -(P - S - N*log(gamma(gamma)) + M*log(p) + N*gamma*log(1-p) - N*log(1-(1-p)^gamma))
}


getNBMLE <- function(gInitValue, pInitValue, lowerBound, upperBound = c(Inf, Inf)) {
  mle_NB <- NULL  
  tryCatch( {    
    mle_NB <- mle(Nb,#NB,
                  start = list(gamma = gInitValue, p = pInitValue),
                  method = "L-BFGS-B", 
                  lower = lowerBound,
                  upper = upperBound)
    
  },
  error = function(e) {      
    #    print(e$message)
    #    cat("\n")
  }
  )
  return(mle_NB)
}

# Discrete Weibull --------------------------------------------------------

discreteWeibull <- function(v, p){
  -(sum(log(p^((data$V1)^(v)) - p^((data$V1 + 1)^(v)))))   
}

dW <- function(v, p){
  -(sum(log(p^((data$V1 - 1)^(v)) - p^((data$V1)^(v)))))
}

d1 <- function(v, p){
  -(sum(log(p^((data$V1^v)-1) - p^(((data$V1 + 1)^v)-1))))
}


getDWeibullMLE <- function(vInitValue, pInitValue, lowerBound, upperBound = c(Inf, Inf)) {
  mle_DWeibull <- NULL  
  tryCatch( {    
    mle_DWeibull <- mle(d1,#discreteWeibull,
                        start = list(v = vInitValue, p = pInitValue),
                        method = "L-BFGS-B", 
                        lower = lowerBound,
                        upper = upperBound)
    
  },
  error = function(e) {      
   # print(e$message)
  #  cat("\n")
  }
  )
  return(mle_DWeibull)
}

# Zeta-Mandelbrot ---------------------------------------------------------

# ZMandelbrot <- function(rho, a){
#   -(-(1+rho)*sum(log(degree + a)) - N*log(zeta_x(1+rho, a)))
# }


# mle_ZMandelbrot <- mle(ZMandelbrot,
#                        start = list(rho = 1, a = 1),
#                        method = "L-BFGS-B", 
#                        lower = c(0.01, 0.01))

# Extra functions ---------------------------------------------------------

zeta_x<-function(alpha,x) {
  aux <- 0
  if(x == 1) {
    zeta(alpha)
  } 
  else {
    zeta(alpha) - sum((1:(x-1))^(-alpha))
  }
}

get_AIC <- function(m2logL, K, N) {
  m2logL + 2*K*N/(N - K - 1)
}

get_BIC <- function(m2logL, K, N) {
  m2logL + K*log(N)
}

write_AIC_Values <- function(language){
  geometricModel <- ifelse(is.null(mle_geometric), "-", as.numeric(round(get_AIC(attributes(summary(mle_geometric))$m2logL, 1, N), 4)))
  poissonModel <- ifelse(is.null(mle_poisson), "-", as.numeric(round(get_AIC(attributes(summary(mle_poisson))$m2logL, 1, N), 4)))
  zetaModel2 <- round(get_AIC(2*zeta_Distribution_Fixed_Gamma(), 0, N), 4)
  zetaModel <- ifelse(is.null(mle_zeta), "-", as.numeric(round(get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N), 4)))
  zetaTruncatedModel <- ifelse(is.null(mle_right_truncated_zeta), "-", as.numeric(round(get_AIC(attributes(summary(mle_right_truncated_zeta))$m2logL, 2, N), 4)))
  altmannModel <- ifelse(is.null(mle_Altmann), "-", as.numeric(round(get_AIC(attributes(summary(mle_Altmann))$m2logL, 2, N), 4)))
  moeZipfModel <- ifelse(is.null(mle_MOEZipf), "-", as.numeric(round(get_AIC(attributes(summary(mle_MOEZipf))$m2logL, 2, N), 4)))
  nbModel <- ifelse(is.null(mle_NB), "-", as.numeric(round(get_AIC(attributes(summary(mle_NB))$m2logL, 2, N), 4)))
  dWeibullModel <- ifelse(is.null(mle_DWeibull), "-", as.numeric(round(get_AIC(attributes(summary(mle_DWeibull))$m2logL, 2, N), 4)))
  c(geometricModel, poissonModel, zetaModel2, zetaModel, zetaTruncatedModel, altmannModel, moeZipfModel, nbModel, dWeibullModel)
}

write_BIC_Values <- function(language){
  geometricModel <- ifelse(is.null(mle_geometric), "-", as.numeric(round(get_BIC(attributes(summary(mle_geometric))$m2logL, 1, N), 4)))
  poissonModel <- ifelse(is.null(mle_poisson), "-", as.numeric(round(get_BIC(attributes(summary(mle_poisson))$m2logL, 1, N), 4)))
  zetaModel2 <- round(get_BIC(2*zeta_Distribution_Fixed_Gamma(), 0, N), 4)
  zetaModel <- ifelse(is.null(mle_zeta), "-", as.numeric(round(get_BIC(attributes(summary(mle_zeta))$m2logL, 1, N), 4)))
  zetaTruncatedModel <- ifelse(is.null(mle_right_truncated_zeta), "-", as.numeric(round(get_BIC(attributes(summary(mle_right_truncated_zeta))$m2logL, 2, N), 4)))
  altmannModel <- ifelse(is.null(mle_Altmann), "-", as.numeric(round(get_BIC(attributes(summary(mle_Altmann))$m2logL, 2, N), 4)))
  moeZipfModel <- ifelse(is.null(mle_MOEZipf), "-", as.numeric(round(get_BIC(attributes(summary(mle_MOEZipf))$m2logL, 2, N), 4)))
  nbModel <- ifelse(is.null(mle_NB), "-", as.numeric(round(get_BIC(attributes(summary(mle_NB))$m2logL, 2, N), 4)))
  dWeibullModel <- ifelse(is.null(mle_DWeibull), "-", as.numeric(round(get_BIC(attributes(summary(mle_DWeibull))$m2logL, 2, N), 4)))
  c(geometricModel, poissonModel, zetaModel2, zetaModel, zetaTruncatedModel, altmannModel, moeZipfModel, nbModel, dWeibullModel)
}

getMinValue <- function(values){
  m <- Inf
  for(i in values){
    if(suppressWarnings(!is.na(as.numeric(i)))){
      if(as.numeric(i) < m){
        m <- as.numeric(i)
      }
    }
  }
  return(m)
}

getDeltaValues <- function(aicValues){
  minVal <- getMinValue(aicValues)
  result <- c()
  for(i in aicValues){
    if(suppressWarnings(!is.na(as.numeric(i)))){
      result <- c(result, round(as.numeric(i) - minVal, 4))
    }
    else { 
      result <- c(result, '-')
    }
  }
  return(result)
}

# getDeltaValues <- function(aicValues){
#   minVal <- min(aicValues)
#   result <- c()
#   for(i in 1:length(aicValues)){
#     if(is.numeric(aicValues[i])){
#       result <- c(result, round(aicValues[i] - minVal, 4))
#     }
#   }
#   return(result)
# }

write_parameters <- function(language){
  qValue <- ifelse(is.null(mle_geometric), "-", as.numeric(round(attributes(mle_geometric)$coef[1], 4)))
  lambdaValue <- ifelse(is.null(mle_poisson), "-", as.numeric(round(attributes(mle_poisson)$coef[1], 4)))
  gammaValue1 <- ifelse(is.null(mle_zeta), "-", as.numeric(round(attributes(mle_zeta)$coef[1], 4)))
  gammaValue2 <- ifelse(is.null(mle_right_truncated_zeta), "-", as.numeric(round(attributes(mle_right_truncated_zeta)$coef[1], 4)))
  kValue <- ifelse(is.null(mle_right_truncated_zeta), "-", as.numeric(round(attributes(mle_right_truncated_zeta)$coef[2], 4)))
  gammaAltmann <- ifelse(is.null(mle_Altmann), "-", as.numeric(round(attributes(mle_Altmann)$coef[1], 4)))
  deltaAltmann <- ifelse(is.null(mle_Altmann), "-", as.numeric(round(attributes(mle_Altmann)$coef[2], 4)))
  gammaMOEZipf <- ifelse(is.null(mle_MOEZipf), "-", as.numeric(round(attributes(mle_MOEZipf)$coef[1], 4)))
  deltaMOEZipf <- ifelse(is.null(mle_MOEZipf), "-",as.numeric( round(attributes(mle_MOEZipf)$coef[2], 4)))
  gammaNB <- ifelse(is.null(mle_NB), "-", as.numeric(round(attributes(mle_NB)$coef[1], 4)))
  pNB <- ifelse(is.null(mle_NB), "-", as.numeric(round(attributes(mle_NB)$coef[2], 4)))
  vWeib <- ifelse(is.null(mle_DWeibull), "-", as.numeric(round(attributes(mle_DWeibull)$coef[1], 4)))
  pWeib <- ifelse(is.null(mle_DWeibull), "-", as.numeric(round(attributes(mle_DWeibull)$coef[2], 4)))
  c(qValue, lambdaValue, gammaValue1, gammaValue2, kValue, gammaAltmann, 
    deltaAltmann, gammaMOEZipf, deltaMOEZipf, gammaNB, pNB, vWeib, pWeib)  
}


# Executions --------------------------------------------------------------
mle_geometric <- getGeometricMLE(N/M, c(1*10^(-6)), c(1 - 1*10^(-6)))
mle_poisson <- getPoissonMLE(M/N, c(1*10^(-6)))
mle_zeta <- getZetaMLE(2, c(1.00001))
rtGammaInit <- ifelse(is.null(mle_zeta), 2, attributes(mle_zeta)$coef[1])
kmaxInit <- max(degrees)
mle_right_truncated_zeta <- getRightTruncZetaMLE(rtGammaInit, kmaxInit, c(1.00001, kmaxInit))
mle_Altmann <- getAltmannMLE(2, 2, c(5*10^-6, 5*10^-6))
moeGammaInit <- ifelse(is.null(mle_zeta), 2, as.numeric(attributes(mle_zeta)$coef[1]))
mle_MOEZipf <- getMOEZipfMLE(moeGammaInit, 1, c(1.0001, 1))
mle_NB <- getNBMLE(1*10^(-6), 1*10^(-6), c(1*10^(-6), 1*10^(-6)), c(Inf, 1 - 1*10^(-6)))
mle_DWeibull <- getDWeibullMLE(0.5, N/M, c(1*10^(-6), 1*10^(-6)), c(Inf, 1 - 1*10^(-6))) 
