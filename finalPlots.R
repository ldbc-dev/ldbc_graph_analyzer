get_frequencies <- function(ds){
  result <- c()
  for(i in 1:length(ds)){
    result <- c(result, ds[[i]])
  }
  return(result)
}

get_geometric_line <- function(N, q, k){
  prob <- N*((1-q)^(k-1)*q)
  return(prob)
}

get_poisson_line <- function(N, lambda, k){
  prob <-N*((lambda^(k)*exp(-lambda))/(factorial(k)*(1-exp(-lambda))))
  return(prob)
}

get_zeta2_line <- function(N, k){
  prob <- N*((k^-2)/zeta(2))
  return(prob)
}

get_zeta_line <- function(N, gamma1, k){
  prob <- N*((k^(-gamma1))/zeta(gamma1))
  return(prob)
}

get_Harmonic <- function(gamma, kMax) {
  result <- 0   
  for (i in 1:kMax){
    result <- result + 1/(i^gamma) 
  }
  return(result)
}

get_truncZeta_line <- function(N, gamma2, kMax, k){
  harmonic <- get_Harmonic(gamma2, kMax)
  prob <- N*((k^-gamma2)/harmonic)
  return(prob)
}

getcAltmann <- function(gamma, delta){
  resp <- 0
  for(j in 1:N){
    resp <- resp +(j^(-gamma))*exp(-delta*j) 
  }
  return(1/resp)
}

get_Altamann_line <- function(N, gamma, delta, k){
  cAltmann <- getcAltmann(gamma, delta)
  prob <- N*(cAltmann*(k^-gamma)*exp(-delta*k))
  return(prob)
}

zeta_x<-function(alpha,x) {
  aux <- 0
  if(x == 1) {
    zeta(alpha)
  } 
  else {
    zeta(alpha) - sum((1:(x-1))^(-alpha))
  }
}

get_MOEZipf_value <- function(N, gamma, delta, k){
  num <- (1/k^(gamma)) * delta * zeta(gamma)
  den1 <- (zeta(gamma) - (1 - delta) * zeta_x(gamma, k))
  den2 <- (zeta(gamma) - (1 - delta) * zeta_x(gamma, k + 1))
  prob <- N * num/(den1 * den2)
  return(prob)
}

get_MOEZipf_line <- function(N, gamma, delta, degrees){
  values <-c()
  for(i in degrees){
    values <- c(values, get_MOEZipf_value(N, gamma, delta, i))
  }
  return(values)
}

get_NB_line <- function(N, gamma, p, degrees){
  t1 <- exp(lgamma(gamma + degrees))/(exp(lfactorial(degrees))*exp(lgamma(gamma)))
  #t1 <- gamma(gamma + degrees)/(factorial(degrees)*gamma(gamma))
  t2 <- p^(degrees)*(1-p)^(gamma)
  return(N * t1 * t2)
}

get_DiscWeibull_line <- function(N, v, p, degrees){
  prob <- p^(degrees^(v)) - p^((degrees + 1)^(v)) 
  return (prob * N)  
}

getLegendData <- function(q, lambda, gamma1, gamma2, kMax, gamma3, delta, gamma4, delta2, gammaNB, pNB, v, p){
  names <- c("Real data")
  colors <- c("black")
    
  if(is.numeric(q)){
    names <- c(names, "Geometric Distribution")
    colors <-c(colors, "limegreen")
  }

  if(is.numeric(lambda)){
    names <- c(names, "Poisson Distribution")
    colors <-c(colors, "firebrick1")
  }

  names <- c(names, "Zeta fixed param")
  colors <-c(colors, "darkmagenta")

  if(is.numeric(gamma1)){
    names <- c(names, "Zeta")
    colors <-c(colors, "brown4")
  }

  if(is.numeric(gamma2) && is.numeric(kMax)){
    names <- c(names, "Rigth Truncated Zeta")
    colors <-c(colors,  "darkgreen")
  }

  if(is.numeric(gamma3) && is.numeric(delta)){
    names <- c(names,  "Altmann Distribution")
    colors <-c(colors, "blue4")
  }

  if(is.numeric(gamma4) && is.numeric(delta2)){
    names <- c(names,  "MOEZipf")
    colors <-c(colors, "darkorange")
  }

  if(is.numeric(gammaNB) && is.numeric(pNB)){
    names <- c(names, "Negative Binomial")
    colors <-c(colors, "darkturquoise")
  }

  if(is.numeric(v) && is.numeric(p)){
    names <- c(names, "Discrete Weibull")
    colors <-c(colors, "mediumseagreen")
  }
  
  values <- matrix(ncol = 2, nrow=length(names))
  values[, 1] <- names
  values[, 2] <- colors
  
  return(values)
}

# #c("Real data", "Geometric Distribution\n",
#   "Poisson Distribution\n", "Zeta fixed param\n", "Zeta\n", 
#   "Rigth Truncated Zeta\n", "Altmann Distribution\n", 
#   "MOEZipf\n", "Negative Binomial\n", "Discrete Weibull\n")
# 
# c("black",  "limegreen","firebrick1", "darkmagenta", "brown4", 
#   "darkgreen", "blue4", "darkorange", "darkturquoise", "mediumseagreen")

write_final_plots <- function(language, q, lambda, gamma1, gamma2, kMax, gamma3, delta, gamma4, delta2, gammaNB, pNB, v, p){
  plot(degrees, freq, log="xy", type="p", xlab="Degree", ylab="Frequency", pch=20)
  title(language)
  lengendData <- getLegendData(q, lambda, gamma1, gamma2, kMax, gamma3, delta, gamma4, delta2, gammaNB, pNB, v, p)
  legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
         bty="n", lengendData[,1], 
         lty=rep(1, nrow(lengendData)), lwd=rep(2, nrow(lengendData)), 
         col=lengendData[, 2], 
         cex = 0.9)
  
  if(is.numeric(q)){
    geo <- get_geometric_line(N, q, degrees)
    lines(degrees, geo, col="limegreen")
  }
  
  if(is.numeric(lambda)){
    poiss <- get_poisson_line(N, lambda, degrees)
    lines(x = degrees, y = poiss, col="firebrick1")
  }
  
  zeta2 <- get_zeta2_line(N, degrees)
  lines(x = degrees, y = zeta2, col = "darkmagenta")
  
  if(is.numeric(gamma1)){
    zeta <- get_zeta_line(N, gamma1, degrees)
    lines(x = degrees, y = zeta, col="brown4")
  }
  
  if(is.numeric(gamma2) && is.numeric(kMax)){
    trunc <- get_truncZeta_line(N, gamma2, kMax, degrees)
    lines(x = degrees, y = trunc, col = "darkgreen")
  }
  
  if(is.numeric(gamma3) && is.numeric(delta)){
    altmann <- get_Altamann_line(N, gamma3, delta, degrees)
    lines(degrees, altmann, col = "blue4")
  }
  
  if(is.numeric(gamma4) && is.numeric(delta2)){
    moeZipf <- get_MOEZipf_line(N, gamma4, delta2, degrees)
    lines(degrees, moeZipf, col = "darkorange")
  }
  
  if(is.numeric(gammaNB) && is.numeric(pNB)){
    negBinom <- get_NB_line(N, gammaNB, pNB, degrees)
    lines(degrees, negBinom, col = "darkturquoise")
  }
  
  if(is.numeric(v) && is.numeric(p)){
    discWeib <- get_DiscWeibull_line(N, v, p, degrees)
    lines(degrees, discWeib, col = "mediumseagreen")
  }
}

bestPlot <- function(bestIndex, name, paramsValues){
  plot(degrees, freq, log="xy", type="p", xlab="Degree", ylab="Frequency", main = name, pch=20)  
  if(bestIndex == 1){
    geo <- get_geometric_line(N, as.numeric(paramsValues[1,2]), degrees)
    lines(degrees, geo, col="limegreen")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Geometric Distribution"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "limegreen"), cex = .9)
  }
  if(bestIndex == 2){
    poiss <- get_poisson_line(N, as.numeric(paramsValues[1,3]), degrees)
    lines(degrees, poiss, col="firebrick1")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Poisson Distribution"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "firebrick1"), cex = .9)
  }
  if(bestIndex == 3){
    zeta2 <- get_zeta2_line(N, degrees)
    lines(degrees, zeta2, col = "darkmagenta")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Zeta free parameter"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "darkmagenta"), cex = .9)
  }
  if(bestIndex == 4){
    zeta <- get_zeta_line(N, as.numeric(paramsValues[1,4]), degrees)
    lines(degrees, zeta, col="brown4")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Zeta Distribution"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "brown4"), cex = .9)
  }
  if(bestIndex == 5){
    trunc <- get_truncZeta_line(N, as.numeric(paramsValues[1,5]), as.numeric(paramsValues[1,6], degrees))
    lines(degrees, trunc, col = "darkgreen")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Right-truncated Zeta"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "darkgreen"), cex = .9)
  }
  if(bestIndex == 6){
    altmann <- get_Altamann_line(N, as.numeric(paramsValues[1,7]), as.numeric(paramsValues[1,8]), degrees)
    lines(degrees, altmann, col = "blue4")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Altmann Distribution"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "blue4"), cex = .9)
  }
  if(bestIndex == 7){
    moeZipf <- get_MOEZipf_line(N, as.numeric(paramsValues[1,9]), as.numeric(paramsValues[1,10]), degrees)
    lines(degrees, moeZipf, col = "darkorange")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "MOEZipf"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "darkorange"), cex = .9)
  }
  if(bestIndex == 8){
    negBinom <- get_NB_line(N, as.numeric(paramsValues[1,11]), 
                           as.numeric(paramsValues[1,12]), degrees)
    lines(degrees, negBinom, col = "darkturquoise")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Negative Binomial"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "darkturquoise"), cex = .9)
  }
  if(bestIndex == 9){
    discWeib <- get_DiscWeibull_line(N, as.numeric(paramsValues[1,13]), 
                            as.numeric(paramsValues[1,14]), degrees)
    lines(degrees, discWeib, col = "mediumseagreen")
    legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, 
           bty="n", c("Real data", "Discrete Weibull"), lty=c(1, 1), lwd=c(2, 2), 
           col=c("black",  "mediumseagreen"), cex = .9)
  }
}
