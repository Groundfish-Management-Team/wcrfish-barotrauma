
moods_median(save_dir = save_dir,
    file = mod_pelagic1030, 
    name = "_pelagic_10_30")
moods_median(save_dir = save_dir,
    file = mod_pelagic3050, 
    name = "_pelagic_30_50")
moods_median(save_dir = save_dir,
    file = mod_pelagic50plus, 
    name = "_pelagic_50plus")
moods_median(save_dir = save_dir,
    file = mod_demersal1030, 
    name = "_demersal_10_30")
moods_median(save_dir = save_dir,
    file = mod_demersal3050, 
    name = "_demersal_30_50")
moods_median(save_dir = save_dir,
    file = mod_demersal50plus, 
    name = "_demersal_50plus")
moods_median(save_dir = save_dir,
    file = mod_all1030, 
    name = "_all_10_30")
moods_median(save_dir = save_dir,
    file = mod_all3050, 
    name = "_all_30_50")
moods_median(save_dir = save_dir,
    file = mod_all50plus, 
    name = "_all_50plus")
moods_median(save_dir = save_dir,
    file = mod_dwarf3050, 
    name = "_dwarf_30_50")

#put the following into the 2 by K table:
moods_median = function(save_dir, file, name, exact = TRUE) {

    out <- NULL
    data <- file$Jags$BUGSoutput$sims.matrix
    ind <- as.numeric(as.factor(file$dat$Species))
    species_name <- file$specs
    N <- length(data[1, 4:ncol(data)]) - 1
    unobs <- data[, "mupred"]

    for (i in 1:length(species_name)){

        if(N == 1){
            col = "mu"
        } else{
            col <- paste("mu[",i,"]", sep = "")
        }

        species <- data[, col]
        df <- data.frame(
            group = c(rep(species_name[i], nrow(data)), rep("unobserved", nrow(data))),
            value = c(species, unobs))
        facs <- unique(df$group)
        factorN <- length(facs)

        MoodsMedianTable = matrix(NA, nrow = 2, ncol = factorN)
        rownames(MoodsMedianTable) = c("> overall median", "<= overall median")
        colnames(MoodsMedianTable) = c(facs[1:factorN])
        colnames(MoodsMedianTable) = paste("Factor:",colnames(MoodsMedianTable))

        for(j in 1:factorN){ #for each factor level
        
            g = facs[j] #assign a temporary "group name"
        
            #count the number of observations in the factor that are greater than
            #the overall median and save it to the table
            MoodsMedianTable[1,j] = sum(df[,2][ which(df[,1]==g)] > overallmedian)
        
            #count the number of observations in the factor that are less than
            # or equal to the overall median and save it to the table
            MoodsMedianTable[2,j] = sum(df[,2][ which(df[,1]==g)] <= overallmedian)
        
        }

        if(exact == FALSE){
            # Pearson's Chi-squared test
            chi <- chisq.test(MoodsMedianTable)
            fisher <- NULL          
        }

        if(exact == TRUE){
            chi <- chisq.test(MoodsMedianTable)
            fisher <- fisher.test(MoodsMedianTable)
        }

        out <- rbind(out, 
            c(species_name[i], 
                as.numeric(chi$p.value), 
                as.numeric(chi$parameter), 
                mean(fisher$conf.int)))
    } #species loop
    colnames(out) <- c('species', 'p_value', 'df', "odd_ratio")
    write.csv(out, file = file.path(save_dir, paste0("moods_median_chi_squared_", name, ".csv")))
}


