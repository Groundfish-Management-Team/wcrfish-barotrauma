
hierarchical_plot <- function(dir, 
                              dat,
                              Jags = NULL,
                              filenote,
                              titlenote,
                              Nsim = 1e4,
                              ymax = 7){
  

  Nburnin <- Nsim
  dead    <- 1 - dat$alive
  Indicator <- as.numeric(as.factor(dat$Species))
  Nspecies <- max(Indicator)
  Nobs <- length(dead)
  Data <- list(dead = dead, 
               Indicator = Indicator, 
               Nspecies = Nspecies, 
               Nobs = Nobs)

  if(is.null(Jags)){
    Jags <- jags(model.file = file.path(dir, "model4.bug"), 
                 working.directory = NULL,
                 data = Data, 
                 parameters.to.save = c("beta.mu", "eta", "mu", "mupred"),
                 n.chains = 3, 
                 n.thin = 1, 
                 n.iter = Nsim + Nburnin, 
                 n.burnin = Nburnin)
  }
  

  specs <- levels(as.factor(dat$Species))
  #Nspecies <- length(specs)
  N <- ifelse(Nspecies %% 2 == 0, ceiling(Nspecies/2 + 1), ceiling(Nspecies/2))

  # plot distributions
  png(paste0(dir, "/", 'Barotrauma_hierarchical_modeling_', filenote, '.png',sep = ''),
      width = 7, height = 7,res = 300, units = 'in')
  par(mfrow = c(N, 2), mar = c(3, 1, 2, 1), oma = c(2, 2, 5, 0))

  for(i in 1:(Nspecies + 1)) {

    Nspec <- sum(Data$Indicator == i)
    Ndead <- sum(Data$Indicator == i & Data$dead == 1)  

    if(i == Nspecies + 1 | Nspecies == 1) {
      col <- "mupred"
      main <- "Unobserved species  (N = 0)"
      if (Nspecies == 1 & i == 1) { 
        main <- paste(specs[i],"  (N = ",Nspec,")", sep = "") 
        col = "mu"
      }
      if (is.na(specs[i])) { main <- "Unobserved species (N = 0)" }
    } else {
      col <- paste("mu[",i,"]", sep = "")
      main <- paste(specs[i],"  (N = ",Nspec,")", sep = "")
    }

    h1 <- hist(Jags$BUGSoutput$sims.matrix[,col], col = 'grey', border = 'grey',
               breaks = seq(0,1, 0.01), main = main,
               freq = FALSE, ylim = c(0, ymax), xlab = "", ylab = "", axes = FALSE)

    axis(1, at = seq(0, 1, 0.2), lab = perc(seq(0, 1, 0.2)))
    est.med <- median(Jags$BUGSoutput$sims.matrix[, col])
    obs.med <- Ndead / Nspec
    est.90  <- quantile(Jags$BUGSoutput$sims.matrix[, col], 0.9)
    # add vertical lines
    colvec <- c(1, 2, 4)
    ltyvec <- c(1, 2, 1)

    for(iline in 1:3) {
      x <- c(est.med, obs.med, est.90)[iline]
      y <- h1$density[abs(x - h1$mids) == min(abs(x - h1$mids))][1]
      if(iline==2) y <- ymax # longer line for thing that isn't related to density
      col <- colvec[iline]
      lty <- ltyvec[iline]
      lines(x = rep(x,2), c(0,y),
            col = col, lwd = 2, lty = lty)
    }

    # add legend
    legend('topright', bty = 'n', seg.len = 3,
           legend = c(paste("Mort. obs. =", perc(obs.med)),
                      paste("Mort. est. =",      perc(est.med)),
                      paste("Mort. 90% CI = ",   perc(est.90))),
           col = colvec[c(2, 1, 3)], lwd = 2, lty = ltyvec[c(2, 1, 3)])
  } #end species loop

  # add axis labels
  mtext(side = 1, line = 0.5, outer = TRUE, "Mortality rate", cex = 0.66)
  mtext(side = 2, line = 0.5, outer = TRUE, "Posterior density", cex = 0.8)
  mtext(side = 3, line = 2, outer = TRUE,paste("Hierarchical modeling results for \n",
                          titlenote, sep = ""), cex = 1)
  dev.off()

  return(list(Jags = Jags, dat = dat, specs = specs))
} #hierarchical_plot end