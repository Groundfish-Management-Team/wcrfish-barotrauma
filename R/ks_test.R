save_dir <- save_dir
file <- mod_pelagic3050
load(file.path(save_dir, "datagrouping.Rdata"))
load(file.path(save_dir, "model_estimates.Rdata"))

do_ks_test(save_dir = save_dir, file = mod_pelagic1030, 
	main = "Pelagic (10-30 fm)")
do_ks_test(save_dir = save_dir, file = mod_pelagic3050, leg_loc = "bottomright", 
	main = "Pelagic (30-50 fm)")
do_ks_test(save_dir = save_dir, file = mod_pelagic50plus, leg_loc = "left",
	main = "Pelagic (50+ fm)")
do_ks_test(save_dir = save_dir, file = mod_demersal1030, 
	main = "Demersal (10-30 fm)")
# ties in the demersal 10-30
do_ks_test(save_dir = save_dir, file = mod_demersal3050, 
	main = "Demersal (30-50 fm)")
do_ks_test(save_dir = save_dir, file = mod_demersal50plus, 
	main = "Demersal (50+ fm)")
do_ks_test(save_dir = save_dir, file = mod_all1030, 
	main = "Demersal & Pelagic (10-30 fm)")
# ties in the demersal & pelagic group 10-30
do_ks_test(save_dir = save_dir, file = mod_all3050, 
	main = "Demersal & Pelagic (30-50 fm)", leg_loc = "bottomright")
do_ks_test(save_dir = save_dir, file = mod_all50plus, 
	main = "Demersal & Pelagic (50+ fm)", leg_loc = "bottomright")
do_ks_test(save_dir = save_dir, file = mod_dwarf3050, leg_loc = "bottomright", 
	main = "Dwarf (30-50 fm)")

do_ks_test <- function(save_dir, file, main, leg_loc = "right"){

	set.seed(1212)
	data <- file$Jags$BUGSoutput$sims.matrix
	ind <- as.numeric(as.factor(file$dat$Species))
	species_name <- file$specs
	N <- length(data[1, 4:ncol(data)]) - 1
	unobs <- sample(x = data[, "mupred"], size = length(ind), replace = FALSE)

	#df <- as.data.frame(data[, 4:ncol(data)])
	#df_long <- df %>%
	#	pivot_longer(cols = 1:(N+1), names_to = "group", values_to = "value")
	png(paste0(save_dir, "/", 'KS_', main, '.png',sep = ''),
      width = 10, height = 10,res = 300, units = 'in')
	
	colors <- rainbow(N)
	legend_name <- species_vec <- NULL
	par(mfrow = c(1,1))
	set.seed(1)
	for (i in 1:N){
		n <- sum(ind == i)
		if(N == 1){
			col = "mu"
		} else{
			col <- paste("mu[",i,"]", sep = "")
		}
		species <- sample(x = data[, col], size = n, replace = FALSE)
		if(i == 1){
		plot(ecdf(unobs), ylim = c(0, 1.04), xlim = c(0, 1.04), main = main,
			xlab = "value", ylab = "Cumulative Probability", col = 1)	
	    }
		lines(ecdf(species), col = colors[i])
		ks <- ks.test(species, unobs)
		#print.letter(label = paste("D = ", round(ks$statistic, 2)), c(0.8, 0.5))
		#print.letter(label = paste("p-value = ", round(ks$p.value, 2)), c(0.8, 0.25))
  		t <- t.test(species, unobs, mu = 0.15)
  		legend_name <- c(legend_name,
  			paste(species_name[i], "(N =", n, ", D =", round(ks$statistic, 2), ", p-value =", round(ks$p.value, 2), ")"))
  	} 
  	legend(leg_loc, bty = 'n', legend = c("Unobserved", legend_name),
  		col = c("black", colors), pch = 16)
  	#boxplot(cbind(species_vec, unobs), col = c(colors, "grey"))
  	#abline(h = median(data[,"mupred"]))
  	dev.off()
}