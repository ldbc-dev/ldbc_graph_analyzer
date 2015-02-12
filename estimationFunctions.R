# ---- Extra Functions ---- #
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

get_Harmonic <- function(gamma, kMax) {
  result <- 0   
  for (i in 1:kMax){
    result <- result + 1/(i^gamma) 
  }
  return(result)
}

get_F_Altmann <- function(gamma, delta){
  result <- 0
  for(k in 1:N){
    result <- ((k^(-gamma)) * exp(-delta * k)) + result
  }
  return(log(result))
}

# ---- Log-Likelihood Functions ----#
displaced_Poisson <- function(lambda){
  C <- get_C(data)
  -(M*log(lambda) - N*(lambda + log(1 - exp(-lambda))) - C)
}

displaced_Geometric <- function(q){
  -((M - N)*log(1 - q) + N*log(q))
}

zeta_Distribution_Fixed_Gamma <- function(){
  -(-2*M_prime - N*log((pi^2)/6))
}

zeta_Distribution <- function(gamma){
  -( -gamma*M_prime - N*log(zeta(gamma)))
}

rigth_Truncated_Zeta <- function(gamma, kMax){
  harmonic <- get_Harmonic(gamma, kMax)
  -(-gamma*M_prime - N*log(harmonic))
}

altmann_Distribution <- function(gamma, delta){
  f <- get_F_Altmann(gamma, delta)
  -(-(f*N + gamma*M_prime + delta*M))
}

# MOEZipf <- function(gamma, delta){
#   N*log(delta) + Nlog(zeta(gamma)) - gamma*M_prime - sum()
# }

# ---- mle - Values ---- #

mle_poisson <- mle(displaced_Poisson,
                   start = list(lambda = M/N),
                   method = "L-BFGS-B",
                   lower = c(0.000001))

mle_geometric <- mle(displaced_Geometric,
                     start = list(q = N/M),
                     method = "L-BFGS-B",
                     lower = c(0.000001),
                     upper = c(1 - 0.000001))

mle_zeta <- mle(zeta_Distribution,
                start = list(gamma = 2),
                method = "L-BFGS-B",
                lower = c(1.00001))

mle_rigth_truncated_zeta <- mle(rigth_Truncated_Zeta,
                                start = list(gamma = 2, kMax = data.max),
                                method = "L-BFGS-B",
                                lower = c(1.00001, data.max))

mle_Altmann <- mle(altmann_Distribution,
                   start = list(gamma = 2, delta = 2),
                   method = "L-BFGS-B", 
                   lower = c(5*10^-6, 5*10^-6))

write_parameters <- function(language){
  qValue <- round(attributes(mle_geometric)$coef[1], 4)
  lambdaValue <- round(attributes(mle_poisson)$coef[1], 4)
  gammaValue1 <- round(attributes(mle_zeta)$coef[1], 4)
  gammaValue2 <- round(attributes(mle_rigth_truncated_zeta)$coef[1], 4)
  kValue <- round(attributes(mle_rigth_truncated_zeta)$coef[2], 4)
  gammaAltmann <- round(attributes(mle_Altmann)$coef[1], 4)
  deltaAltmann <- round(attributes(mle_Altmann)$coef[2], 4)
  c(qValue, lambdaValue, gammaValue1, gammaValue2, kValue, gammaAltmann, deltaAltmann)  
}

get_AIC <- function(m2logL, K, N) {
  m2logL + 2*K*N/(N - K - 1)
}

write_AIC_Values <- function(language){
  geometricModel <- round(get_AIC(attributes(summary(mle_geometric))$m2logL, 1, N), 4)
  poissonModel <- round(get_AIC(attributes(summary(mle_poisson))$m2logL, 1, N), 4)
  zetaModel2 <- round(get_AIC(2*zeta_Distribution_Fixed_Gamma(), 0, N), 4)
  zetaModel <- round(get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N), 4)
  zetaTruncatedModel <- round(get_AIC(attributes(summary(mle_rigth_truncated_zeta))$m2logL, 2, N), 4)
  altmannModel <- round(get_AIC(attributes(summary(mle_Altmann))$m2logL, 2, N), 4)
  c(geometricModel, poissonModel, zetaModel2, zetaModel, zetaTruncatedModel, altmannModel)
}

getDeltaValues <- function(aicValues){
  minVal <- min(aicValues)
  result <- c()
  for(i in 1:length(aicValues)){
    if(is.numeric(aicValues[i])){
    result <- c(result, round(aicValues[i] - minVal,4))
    }
  }
  return(result)
}

