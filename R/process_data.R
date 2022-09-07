clean_data <- function(data, save_dir, dir="C:/Assessments/Council/GMT/wcrfish-barotrauma"){


	# do some minor data clean-up #########################################################

	data$Depth.Interval <- data$Depth.Interval.fm
	fix <- which(data$Depth.Interval. == "20-Oct")
	data$Depth.Interval[fix] <- "10-20"
	
	data$Species <- tolower(data$Species)
	
	data$Deploy.Date <- as.Date(data$Deploy.Date, format = "%m/%d/%Y")
	data$Year <- as.numeric(substring(data$Deploy.Date, first = 1, last = 4))
	 
	# add space before "0" so cutting by depthin interval works  
	fix <- which(data$Depth.Interval == "0-10") 
	data$Depth.Interval[fix] <- " 0-10"	
	
	# Determine fish alive vs dead based on the data$Mortality..0.Dead. column where
	# 0 = dead and 1 = alive
	data$alive <- data$Mortality..0.Dead.
	data$dead <- 1 - data$Mortality..0.Dead.
	
	data$alive_lt <- data$X10.Day.Mortality..0...Dead.
	data$dead_lt <- 1 - data$X10.Day.Mortality..0...Dead.
	data$Species_orig <- data$Species
	# Value in the Days Held column from the raw data file is listed as hours
	# However, the study retained fish on average of 48 hours and up to 96 hours according to 
	# the publications.
	#data[data$Study.Type == "Cage Study - Hannah", "Days.Held"] <- round(data[data$Study.Type == "Cage Study - Hannah", "Days.Held"] / 24, 0)
	
	###############################################################################################
	# There are additional observations in the deep interval (50-100) that come from Hannah
	# so we may need to deal with this when determining long-term and unaccounted for mortality
	# for this depth interval. 
	# Deep demersal (Hannah) yelloweye N = 10, 1 dead, 9 alive --> 10% mortality
	# Deep pelagic (Hannah) canary N = 10, 8 dead, 2 alive --> 80% mortality
	# All held less than 3 days
	#	
	# Decision: remove the Wegner observations (represent long-term mortality) from the 30-50 fm depth 
	# interval and remove the Hannah data for depths > 50 fathoms
	remove = which(data$Study.type == "Accoustic Tagging - Wegner and Hyde" & 
	    data$Depth.Bin.fm != "50-100")
	# Removes 10 fish: 8 bocaccio and 2 cowcod
	removed_data = data[remove, ]
	removed_data$reason = "Long_term_mort_30-50"
	sub_data = data[-remove, ]
	
	if (save_dir == file.path(dir, "analysis")) {
	
	    remove = which(sub_data$Study.type == "Cage Study - Hannah" & 
	        sub_data$Depth.Bin.fm == "50-100")
	    tmp = sub_data[remove, ]
	    tmp$reason = "Hannah_50-100"
	    removed_data = rbind(removed_data, tmp)
	    sub_data = sub_data[-remove, ]
	    # 
	    # Remove the short-term mort estimates for the Wegner observations to be based on the long-term estimates
	    remove = which(sub_data$Study.type == "Accoustic Tagging - Wegner and Hyde" & 
	        sub_data$Depth.Bin.fm == "50-100" & is.na(sub_data$X10.Day.Mortality..0...Dead.))
	        #sub_data$Days.Held < 3) <-- originally did it this way based on the 2013 write-up but they
	        #actually used the X10.Day.Mortality..0...Dead. field since the Days.Held was not fully populated
	    tmp = sub_data[remove, ]
	    tmp$reason = "Wegner_short_term_mort_50-100"
	    removed_data = rbind(removed_data, tmp)
	    # Removes 29 fish: 7 bank, 6 bocaccio, 10 cowcod, and 6 sunset 
	    #                 0 1
	    #  bank rockfish   4 3
	    #  bocaccio        3 3
	    #  cowcod          9 1
	    #  sunset rockfish 3 3
	    sub_data = sub_data[-remove,]
	    # After removing any fish held < 3 days we are left with in the 50-100 bin:
	    #                   0  1
	    #  bank rockfish    0  3
	    #  bocaccio         1 26 <-- only species in the pelagic guild
	    #  cowcod          10 24
	    #  starry rockfish  0  2
	    #  sunset rockfish  0  7   
	
	    ###########################################################################################
	    # Mortality observed post release - included in the alive vs. dead counts
	    # Calculate the long-term mortality prox by guild 
	    # Only available for deep demersal or deep pelagic from the Wegner study
	    #
	    # Deep Pelagic Long-Term Mortality
	    find = which(!is.na(sub_data$X10.Day.Mortality..0...Dead.) & sub_data$Guild == "Deep Pelagic")
	    pelagic_lt_mort = round(1 - sum(sub_data$X10.Day.Mortality..0...Dead.[find]) / length(sub_data$X10.Day.Mortality..0...Dead.[find]),2) 
	    # N = 28, 1 dead and 27 alive 
	    # 4% long-term mortality
	    # Combined Guilds
	    find = which(!is.na(sub_data$X10.Day.Mortality..0...Dead.))
	    grouped_lt_mort = round(1 - sum(sub_data$X10.Day.Mortality..0...Dead.[find]) / length(sub_data$X10.Day.Mortality..0...Dead.[find]),2)
	    # 18% long-term mortality if only filered by NAs 
	    #
	    # if data are filtered by Days.Held > 3 days:
	    # N = 73, 11 dead and 62 alive --> 15% long-term mortality
	    #
	   	# Deep Demersal Long-Term Mortality
	    find = which(!is.na(sub_data$X10.Day.Mortality..0...Dead.) & sub_data$Guild == "Deep Demersal")
	    demersal_lt_mort = grouped_lt_mort
	    #round(1 - sum(sub_data$X10.Day.Mortality..0...Dead.[find]) / length(sub_data$X10.Day.Mortality..0...Dead.[find]),2)
	    # bank rockfish          cowcod starry rockfish sunset rockfish 
	    #             4              37               2               7 
	    # 26% long-term mortality

	    # Group Decision: apply the demersal long-term mortality estimate of % to the demersal species
	    # and apply a demersal + pelagic estimate to the pelagic since there are limited species observations
	    # in the pelagic group
	    # 
	}

	if (save_dir == file.path(dir, "shortterm")) {
	
	    remove = which(sub_data$Study.type == "Accoustic Tagging - Wegner and Hyde" & 
	        sub_data$Days.Held >= 3)
	    tmp = sub_data[remove, ]
	    tmp$reason = "Long_term_mort_50-100"
	    removed_data = rbind(removed_data, tmp)
	    sub_data = sub_data[-remove, ]
	    # 
	    # Removes 29 fish: 7 bank, 6 bocaccio, 10 cowcod, and 6 sunset 
	    #                  0 1
	    #  bank rockfish   0 3
	    #  bocaccio        1 27
	    #  cowcod          10 24
	    #  starry rockfish 0 2
	    #  sunset rockfish 0 7
	   	demersal_lt_mort = 0.26
	    pelagic_lt_mort  = 0.18
	    grouped_lt_mort  = 0.18
	}

	if (save_dir == file.path(dir, "analysis_longterm_adj")) {
	
	    # Need to find good way to define long-term mortality if we are using both short-term and 
	    # long-term in the 50-100 fm bin (Hannah + Wegner observations)
	    #find = which(sub_data$Guild %in% c("Deep Demersal", "Shallow Demersal") & sub_data$Depth.Bin.fm == "50-100")
	    #demersal_lt_mort = round(1 - sum(sub_data$alive[find]) / length(find), 2)
		# 34%
	    # find = which(sub_data$Guild %in% c("Deep Pelagic", "Shallow Pelagic") & sub_data$Depth.Bin.fm == "50-100")
	    # pelagic_lt_mort = round(1 - sum(sub_data$alive[find]) / length(find), 2)
		# 27%
	    #find = which(!sub_data$Guild %in% c("Dwarf") & sub_data$Depth.Bin.fm == "50-100")
	    #grouped_lt_mort = round(1 - sum(sub_data$alive[find]) / length(find), 2)
		# 32%
	    demersal_lt_mort = 0.26
	    pelagic_lt_mort  = 0.18
	    grouped_lt_mort  = 0.18
	
	}

	if (save_dir == file.path(dir, "remove_low_n")) {

		# 10 - 30 fathoms
		remove = which(sub_data$Guild %in% c("Shallow Demersal", "Deep Demersal") & 
			sub_data$Depth.Bin.fm == "0-30"&
			sub_data$Species == "china rockfish")
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		# 50 - 100 fathoms
		remove = which(sub_data$Guild %in% c("Shallow Demersal", "Deep Demersal") & 
			sub_data$Depth.Bin.fm == "50-100"&
			sub_data$Species %in% c("starry rockfish"))
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		remove = which(sub_data$Guild %in% c("Shallow Demersal", "Deep Demersal") & 
			sub_data$Depth.Bin.fm == "30-50" &
			sub_data$Species %in% c("copper rockfish", "greenspotted rockfish", "tiger rockfish", 'greenstriped rockfish', 'quillback rockfish', 'rosy rockfish'))
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		
		#Pelagic
		# 10 - 30 fathoms
		remove = which(sub_data$Guild %in% c("Shallow Pelagic", "Deep Pelagic") & 
			sub_data$Depth.Bin.fm == "0-30"&
			sub_data$Species == "blue rockfish")
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		# 30 -50 fathoms
		remove = which(sub_data$Guild %in% c("Shallow Pelagic", "Deep Pelagic") & 
			sub_data$Depth.Bin.fm == "30-50" &
			sub_data$Species %in% c("black rockfish", "chilipepper", "olive rockfish", 'yellowtail rockfish'))
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]

	    demersal_lt_mort = 0.26
	    pelagic_lt_mort  = 0.18
	    grouped_lt_mort  = 0.18
	}

	if (save_dir == file.path(dir, "combine_low_n")) {

		# 10 - 30 fathoms
		remove = which(sub_data$Guild %in% c("Shallow Demersal", "Deep Demersal") & 
			sub_data$Depth.Bin.fm == "0-30"&
			sub_data$Species == "china rockfish")
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		# 50 - 100 fathoms
		remove = which(sub_data$Guild %in% c("Shallow Demersal", "Deep Demersal") & 
			sub_data$Depth.Bin.fm == "50-100"&
			sub_data$Species %in% c("starry rockfish"))
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		find = which(sub_data$Guild %in% c("Shallow Demersal", "Deep Demersal") & 
			sub_data$Depth.Bin.fm == "30-50" &
			sub_data$Species %in% c("copper rockfish", "greenspotted rockfish", "tiger rockfish", 'greenstriped rockfish', 'quillback rockfish', 'rosy rockfish'))
		sub_data[find, "Species"] = "combined rockfish"
		
		#Pelagic
		# 10 - 30 fathoms
		remove = which(sub_data$Guild %in% c("Shallow Pelagic", "Deep Pelagic") & 
			sub_data$Depth.Bin.fm == "0-30"&
			sub_data$Species == "blue rockfish")
		tmp = sub_data[remove,]
		tmp$reason = "single_low_species_sample"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		# 30 -50 fathoms
		find = which(sub_data$Guild %in% c("Shallow Pelagic", "Deep Pelagic") & 
			sub_data$Depth.Bin.fm == "30-50" &
			sub_data$Species %in% c("black rockfish", "chilipepper", "olive rockfish", 'yellowtail rockfish'))
		sub_data[find, "Species"] = "combined rockfish"

	    demersal_lt_mort = 0.26
	    pelagic_lt_mort  = 0.18
	    grouped_lt_mort  = 0.18
	}
	######################################################################################################
	#
	# 10-day survival estimates
	#                  50-100
	#  bank rockfish        3
	#  bocaccio            27
	#  cowcod              34
	#  starry rockfish      2
	#  sunset rockfish      7
	#
	# M(1) = 1 - (1 - Short Term Mort.) * (1 - Long Term. Mort.)*(1 - Unaccounted for Mortality)
	#
	###########################################################################################
	if (save_dir != file.path(dir, "combine_low_n") &&
		save_dir != file.path(dir, "remove_low_n")) {
		# Remove all species with a single observation
		keep = which(!sub_data$Species %in% c("blue rockfish", "yellowtail rockfish", "greenstriped rockfish", "rosy rockfish",
		     "freckled rockfish"))
		tmp = sub_data[-keep,]
		tmp$reason = "single_observation"
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[keep, ]
		# 
		# Remove the single quillback observation in the 30-50 bin
		remove = which(sub_data$Species == "quillback rockfish" & sub_data$Depth.Bin.fm == "30-50")
		tmp = sub_data[remove,]
		tmp$reason = "single_observation"
		#
		removed_data = rbind(removed_data, tmp)
		sub_data = sub_data[-remove, ]
		# Can only retain them if the one observation is a live fish, jags model errors with only a dead observation (yellowtail)
		# Remove for consistency
	}
	############################################################################################
	# Remove observations of black rockfish (N=33) and deacon rockfish (N = 1) from 0-10 fathoms since no
	# in order to avoid biasing estimates of discard mort from 10-30 fathoms
	# Shallow Pelagic species
	remove = which(sub_data$Depth.Interval == " 0-10")
	#data_0_10 = sub_data[remove, ]
	sub_data$Depth.Bin.fm[remove] = "0-10"
	#sub_data = sub_data[-remove, ]
	# Rename the 0-30 bin to 10-30 to accurately represent the depth interval
	find = which(sub_data$Depth.Bin.fm == "0-30")
	sub_data$Depth.Bin.fm[find] = "10-30"	

	write.csv(removed_data, file.path(save_dir, "barotrauma_removed_data.csv"))
	
	output <- list()
	output$sub_data = sub_data
	output$removed_data = removed_data
	output$demersal_lt_mort = demersal_lt_mort 
	output$pelagic_lt_mort = pelagic_lt_mort
	output$grouped_lt_mort 	= grouped_lt_mort
	return(output)
}