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

write_final_plots <- function(language, q, lambda, gamma1, gamma2, kMax, gamma3, delta){
  plot(degrees, sort(freq, decreasing=T), log="xy", type="l", xlab="Degree", ylab="Frequency")
  title(language)
  legend(grconvertX(0.85,'nfc'), grconvertY(0.8,'nfc'), xjust=0.85, yjust=0.8, bty="n", c("Real data", "Geometric Distribution\n","Poisson Distribution\n", "Zeta fixed param\n", "Zeta\n", "Rigth Truncated Zeta\n", "Altmann Distribution\n"), lty=c(1, 1, 1, 1, 1, 1, 1), lwd=c(2, 2, 2, 2, 2, 2, 2), col=c("black",  "limegreen","firebrick1", "darkmagenta", "brown4", "darkgreen", "blue4"), cex = 0.9)
  geo <- get_geometric_line(N, q, degrees)
  lines(degrees, geo, col="limegreen")
  poiss <- get_poisson_line(N, lambda, degrees)
  lines(x = degrees, y = poiss, col="firebrick1")
  zeta2 <- get_zeta2_line(N, degrees)
  lines(x = degrees, y = zeta2, col = "darkmagenta")
  zeta <- get_zeta_line(N, gamma1, degrees)
  lines(x = degrees, y = zeta, col="brown4")
  trunc <- get_truncZeta_line(N, gamma2, kMax, degrees)
  lines(x = degrees, y = trunc, col = "darkgreen")
  altmann <- get_Altamann_line(N, gamma3, delta, degrees)
  lines(degrees, altmann, col = "blue4")
}

bestPlot <- function(bestIndex, name, degrees, paramsValues){
  plot(degrees, sort(freq, decreasing=T), log="xy", type="l", xlab="Degree", ylab="Frequency", main = name)  
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
}
