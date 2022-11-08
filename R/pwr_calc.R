#save_dir <- save_dir
file <- "combined_species_mort_ests_ci"

pwr_calc <- function(save_dir, file){
	
	data <- read.csv(file.path(save_dir, paste0(file, ".csv")))
	N <- nrow(data)
	ind <- which(data$species == "unobserved")
	out <- matrix(NA, N, 5)

	for(y in 1:N){
		bin <- data$depth_bin[y]
		unobs <- data[data$species == "unobserved" & data$depth_bin == bin, ]

		for(x in 1:5){
			p1 <- round(as.numeric(data[y, x + 3]), 3)
			p2 <- round(as.numeric(unobs[x + 3]), 3)
			h <- as.numeric(abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))))
			if(h == 0){
				power = NA
			} else {
				n1 <- as.numeric(data[y, "total_samples"])
				n2 <- as.numeric(unobs["total_samples"])
				power <- round(pwr::pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = 0.05)$power, 2)				
			}
			out[y, x] <- power
		}
	}

	out <- cbind(data[, 2:3], out)
	write.csv(out, file = file.path(save_dir, paste0(file, "_power.csv")))
}