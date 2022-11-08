create_jags_model <- function(save_dir, gamma = gamma){
	# Create the JAGS model and save
	# Model4 has mu and eta with beta and gamma or log-normal priors, respectively
	# dbeta(a, b)
	# dgamma(shape, rate)
	if (gamma == "informed"){
	Model4 = "
	    model {
	      beta.mu ~ dbeta(1,1)
	      eta ~ dgamma(1,0.1)
	      for(ispecies in 1:Nspecies){
	        mu[ispecies] ~ dbeta(beta.mu*eta, (1-beta.mu)*eta)
	      }
	      mupred ~ dbeta(beta.mu*eta, (1-beta.mu)*eta)
	      for(iobs in 1:Nobs){
	        dead[iobs] ~ dbern(mu[Indicator[iobs]])
	      }
	    }
	"
	}

	if (gamma == "uninformed"){
	Model4 = "
	    model {
	      beta.mu ~ dbeta(1,1)
	      eta ~ dgamma(0.01,0.01)
	      for(ispecies in 1:Nspecies){
	        mu[ispecies] ~ dbeta(beta.mu*eta, (1-beta.mu)*eta)
	      }
	      mupred ~ dbeta(beta.mu*eta, (1-beta.mu)*eta)
	      for(iobs in 1:Nobs){
	        dead[iobs] ~ dbern(mu[Indicator[iobs]])
	      }
	    }
	"
	}
	
	cat(Model4, file = file.path(save_dir, "model4.bug", sep = ""))
}