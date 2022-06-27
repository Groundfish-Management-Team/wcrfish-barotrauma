# Functions

# Function from 2013 file that is was not used in the 2013 code
confints <- function(qvec = c(0.50, 0.60, 0.75, 0.90, 0.95), Ntotal, Ndead, mult = 0.20){
  Mvec <- rep(NA,length(qvec))
  for(i in 1:length(qvec)) {
    tmp <- binom.confint(x = Ndead * mult, n = Ntotal * mult,
                         conf.level = 1 - 2 * (1 - qvec[i]), method = "agresti-coull")
    Mvec[i] <- tmp$upper
  }
  return(Mvec)
}

# Figure labeling function usede in the hierarchical function
perc <- function(x, digits = 0){
  text <- ifelse(is.na(x),"NA", paste(round(100 * x, digits), "%", sep = ""))
  return(text)
}


# Get mu quantiles and link to species
get_mu <- function(data, ci = c(0.5, 0.6, 0.75, 0.9, 0.95)){

  find_mu <- grep("mu", colnames(data$Jags$BUGSoutput$sims.matrix))
  ci_values <- apply(data$Jags$BUGSoutput$sims.matrix[,find_mu[2:length(find_mu)]], 2, quantile, ci)
  
  samps <- data[[2]]
  quants <- NULL
  for(a in data[[3]]) {
    n <- sum(samps$Species == a)
    dead <- sum(samps[samps$Species == a, "dead"])
    mort <- dead / n
    quants <- cbind(quants, c(dead, n, mort))
  }
  total_n <- sum(quants[2,])
  total_dead <- sum(quants[1,])
  total_mort <- total_dead / total_n
  quants <- cbind(quants, c(total_dead, total_n, total_mort))

  out <- rbind(ci_values, quants)
  colnames(out) <- c(data$specs, "unobserved")
  rownames(out)[(length(ci)+1):nrow(out)] = c("dead", "total_samples", "mortality")
  return(out)
}