
calc_cum_mort <- function(dir, 
                          dat_list,
                          depth_bins = c("0-30", "30-50", "50-100"),
                          pi = "90%",
                          guild = NULL,
	                      long_term_mort = 0.15,
	                      add_mort = 0.05,
	                  	  plus50_lt_mort = 0,
	                  	  plus50_add_mort = 0.10){

	s = NULL
	for (a in 1:length(dat_list)){
		s = c(s, colnames(dat_list[[a]]))
	}
	species = unique(s)

	out_matrix <- NULL
	for (b in 1:length(species)){
		for (c in 1:length(dat_list)){
			grab = which(colnames(dat_list[[c]]) == species[b])
			if(length(grab) > 0) {
				out_matrix = rbind(out_matrix, c(species[b], depth_bins[c], dat_list[[c]][, grab]))
			}
		}
	}

	species_mort_ests_ci = as.data.frame(out_matrix)
	colnames(species_mort_ests_ci)[1:2] = c("species", "depth_bin")

	find = which(colnames(species_mort_ests_ci) == pi)
	cum_mort_est =  1-(1-as.numeric(species_mort_ests_ci[,find]))*(1-long_term_mort)*(1-add_mort)
	species_mort_ests = cbind(species_mort_ests_ci[,1:2],
							  species_mort_ests_ci[,find],
							  long_term_mort,
							  add_mort,
							  cum_mort_est)
	colnames(species_mort_ests)[3] = pi

	# Overwrite the long-term mortality value for the 50+ depth interval to be only additional
	# unaccounted for mortality
	find = which(species_mort_ests$depth_bin == "50-100")
	species_mort_ests[find, "cum_mort_est"] = 
		1-(1-as.numeric(species_mort_ests[find, 3]))*(1-plus50_lt_mort)*(1-plus50_add_mort)
	species_mort_ests[find, "long_term_mort"] = plus50_lt_mort
	species_mort_ests[find, "add_mort"] = plus50_add_mort


	write.csv(species_mort_ests_ci,
		file.path(dir, paste0(guild, "_species_mort_ests_ci.csv")))
	write.csv(species_mort_ests,
		file.path(dir, paste0(guild, "_species_mort_ests.csv")))

	return(species_mort_ests_ci)

}