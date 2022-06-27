
plot_post_model_pre_data <- function(dir, data_list, line_names, file_add = "", ymax = 13) {
 
  # post-model pre-data
  n <- 1e6
  eta <- rgamma(n, 1, 0.1)
  beta.mu <- rbeta(n, 1, 1)
  alpha <- beta.mu * eta
  beta <- (1 - beta.mu) * eta
  beta.var  <- beta.mu * (1 - beta.mu) / (eta + 1)
  beta.var2 <- alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
  beta.sd <- sqrt(beta.var)
  mupred <- rbeta(n, alpha, beta)

  alpha_list <- beta_list <- sd_list <- list()
  for(i in 1:length(data_list)){
     alpha_list[[i]] <- data_list[[i]]$Jags$BUGSoutput$sims.matrix[,"beta.mu"]*
                 data_list[[i]]$Jags$BUGSoutput$sims.matrix[,"eta"] 
     beta_list[[i]] <- (1 - data_list[[i]]$Jags$BUGSoutput$sims.matrix[,"beta.mu"])*
                     data_list[[i]]$Jags$BUGSoutput$sims.matrix[,"eta"]
     sd_list[[i]] <- sqrt(alpha_list[[i]] * beta_list[[i]] / ((alpha_list[[i]] + beta_list[[i]])^2 * 
          (alpha_list[[i]] + beta_list[[i]] + 1)))
  }
  
  
  png(file.path(dir, paste0('Barotrauma_hierarchical_modeling_post-model-pre-data_', file_add, '.png')),
      width = 7, height = 7, res = 300, units = 'in')
  par(mfrow = c(3,2), mar = c(3,1,2,1), oma = c(2,2,3,0), cex.main = 1.3)
  
  hist(beta.mu, col = 'grey', border = 'grey', breaks = seq(0, 1, 0.01), freq = FALSE, axes = FALSE,
       xlab = "", ylab = "", main = expression(paste("Hyperparameter ", mu)), ylim = c(0, ymax))
  for(a in 1:length(data_list)){
     lines(density(data_list[[a]]$Jags$BUGSoutput$sims.matrix[, "beta.mu"]), col = 1 + a)
  }
  axis(1)
  legend("topright", lty = 1, col = 2:4, bty = 'n', legend = line_names)

  hist(eta, col = 'grey', border = 'grey', breaks = 1000, xlim = c(0, 30), ylim = c(0, ymax / 40), 
       freq = FALSE, axes = FALSE,
       xlab = "",ylab = "",main = expression(paste("Hyperparameter ",eta)))
  for(b in 1:length(data_list)){
     lines(density(data_list[[b]]$Jags$BUGSoutput$sims.matrix[, "eta"]), col = 1 + b) 
  } 
  axis(1)

  hist(alpha, col = 'grey', border = 'grey', breaks = 1000, xlim = c(0, 20), freq = FALSE, axes = FALSE,
       xlab = "",ylab = "", ylim = c(0, ymax / 10), main = expression(paste("Derived parameter ", alpha)))
  for (c in 1:length(data_list)){
     lines(density(alpha_list[[c]], from = 0), col = 1 + c)
  }
  axis(1)

  hist(beta,  col = 'grey', border = 'grey', breaks = 1000, xlim = c(0,20), freq = FALSE, axes = FALSE,
       xlab = "", ylab = "", ylim = c(0, ymax / 20), main = expression(paste("Derived parameter ", beta)))
  for(d in 1:length(data_list)){
     lines(density(beta_list[[d]], from = 0), col = d + 1)
  }  
  axis(1)

  hist(beta.sd, col = 'grey', border = 'grey', breaks = seq(0, 1, 0.01), xlim = c(0, 0.5),
       ylim = c(0, 12), freq = FALSE, axes = FALSE,
       xlab = "", ylab = "", main = expression(paste("Standard deviation of beta distribution")))
  for(e in 1:length(data_list)){
     lines(density(sd_list[[e]], from = 0), col = 1 + e)    
  }
  axis(1)

  hist(mupred, col = 'grey', border = 'grey', breaks = seq(0, 1, 0.01), freq = FALSE, axes = FALSE,
       xlab = "", ylab = "", main = expression(paste("Derived parameter ", italic(p[total]))))
  axis(1)
  for(f in 1:length(data_list)){
     lines(density(data_list[[f]]$Jags$BUGSoutput$sims.matrix[, "mupred"], from = 0, to = 1), 
          col = f + 1)     
  }

  mtext(side = 1, line = 0.5, outer = TRUE, "Parameter value", cex = 0.8)
  mtext(side = 2, line = 0.5, outer = TRUE, "Posterior density", cex = 0.8)
  mtext(side = 3, line = 1.0, outer = TRUE, "Post-model pre-data distributions", cex = 1)

  dev.off()

}