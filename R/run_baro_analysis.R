#######################################################################################
#
#                   Updated barotrauma analysis for 2022
#                       Chantel Wetzel, GMT
#
######################################################################################

# Load packages
# installed jags from https://sourceforge.net/projects/mcmc-jags/
# and saved here: C:\Users\Chantel.Wetzel\AppData\Local\JAGS\JAGS-4.3.0
rm(list=ls(all=TRUE))
library(rjags)
library(R2jags)
library(runjags)
library(binom)

gamma <- "uninformed"
#gamma <- "informed"

if(gamma == "uninformed"){
    gamma_prior = c(0.01, 0.01)
} else {
    gamma_prior = c(1, 0.1)
}

analysis <- "analysis_longterm_adj"

# set the workding directory
dir <-  "C:/Assessments/Council/GMT/wcrfish-barotrauma"
save_dir <- file.path(dir, paste0(gamma, "_gamma"), analysis)

# load functions
# function that runs jags and creates distribution plots
source(file.path(dir, "R", "hierarchical_plot.R"))
source(file.path(dir, "R", "functions.R"))
source(file.path(dir, "R", "calc_cum_mort.R"))
source(file.path(dir, "R", "process_data.R"))
source(file.path(dir, "R", "create_jags_model.R"))
source(file.path(dir, "R", "plot_post_model_pre_data.R"))
source(file.path(dir, "R", "plot_rates_by_depth.R"))

# load the data from 2022
baro <- read.csv(file.path(dir, "data", "MortalityData2022.csv"), stringsAsFactors = FALSE)

out = clean_data(data = baro, analysis = analysis, save_dir = save_dir)
sub_baro = out$sub_data 
demersal_lt_mort = 0 #out$demersal_lt_mort
pelagic_lt_mort  = 0 #out$pelagic_lt_mort 
grouped_lt_mort  = 0 #out$grouped_lt_mort 

###############################################################################################
# Options for binning for each guild
##############################################################################################
# Shallow Demersal: 10-30
# Deep Demersal: 30-50, 50+
# Shallow Pelagic: 0-10, 10-30
# Deep Pelagic: 30-50, 50+
# Dwarf: 30-50
# Also create a single demersal and pelagic grouping
# Demersal: 10-30, 30-50, 50+
# Pelagic:  10-30, 30-50, 50+
# Or can create a combined rockfish group excluding dwarf rockfish
#
###########################################################################################
# All Pelagic Combined 
###########################################################################################

pelagic1030 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("10-30") & 
                      sub_baro$Guild %in% c("Deep Pelagic", "Shallow Pelagic"), ] 
pelagic3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") & 
                    sub_baro$Guild %in% c("Deep Pelagic", "Shallow Pelagic"), ]
pelagic50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") & 
                      sub_baro$Guild %in% c("Deep Pelagic", "Shallow Pelagic"), ]


#####################################################################################################
# All Demersal
###########################################################################################                             

demersal1030 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("10-30") &
                      sub_baro$Guild %in% c("Deep Demersal", "Shallow Demersal"), ] 
demersal3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") &
                     sub_baro$Guild %in% c("Deep Demersal", "Shallow Demersal"), ]
demersal50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") &
                       sub_baro$Guild %in% c("Deep Demersal", "Shallow Demersal"), ]    

#####################################################################################################
# Demersal & Pelagic Combined
###########################################################################################                             

all1030 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("10-30") &
                      !sub_baro$Guild %in% c("Dwarf"), ] 
all3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") &
                     !sub_baro$Guild %in% c("Dwarf"), ]
all50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") &
                       !sub_baro$Guild %in% c("Dwarf"), ] 

###########################################################################################
# Dwarf 
###########################################################################################                      
 
dwarf3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") &
                      sub_baro$Guild %in% c("Dwarf"), ] 

###########################################################################################
# Save all the data splits
###########################################################################################                      

save(pelagic1030,
     pelagic3050,
     pelagic50plus,
     demersal1030,
     demersal3050,
     demersal50plus,
     all1030,
     all3050, 
     all50plus,
     dwarf3050,
     file = file.path(save_dir, "datagrouping.Rdata"))
 
##########################################################################################

create_jags_model(save_dir = save_dir, gamma = gamma)

##########################################################################################
# Run JAGS model by guild and depth grouping
# Load existing data groupings
# load(file.path(dir, "analysis", "datagrouping.Rdata"))

set.seed(12345)

# Pelagic shallow and deep grouped together analysis
mod_pelagic1030 <- hierarchical_plot(dir = save_dir,
                              dat = pelagic1030,
                              filenote = "pelagic_10-30",
                              titlenote = "Pelagic 10-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_pelagic3050 <- hierarchical_plot(dir = save_dir,
                                 dat = pelagic3050,
                                 filenote = "pelagic_30-50",
                                 titlenote = "Pelagic 30-50 fathoms",
                                 Nsim = 5e4,
                                 ymax = 10)

mod_pelagic50plus <- hierarchical_plot(dir = save_dir,
                                 dat = pelagic50plus,
                                 filenote = "pelagic_50_plus",
                                 titlenote = "Pelagic 50+ fathoms",
                                 Nsim = 5e4,
                                 ymax = 10)


# Demersal ###############################################################
mod_demersal1030 <- hierarchical_plot(dir = save_dir,
                              dat = demersal1030,
                              filenote = "demersal_10-30",
                              titlenote = "Demersal 10-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_demersal3050 <- hierarchical_plot(dir = save_dir,
                                 dat = demersal3050,
                                 filenote = "demersal_30-50",
                                 titlenote = "Demersal 30-50 fathoms",
                                 Nsim = 5e4,
                                 ymax = 10,
                                 h_inch = 12)

mod_demersal50plus <- hierarchical_plot(dir = save_dir,
                                 dat = demersal50plus,
                                 filenote = "demersal_50_plus",
                                 titlenote = "Demersal 50+ fathoms",
                                 Nsim = 5e4,
                                 ymax = 10)

# Pelagic & Demersal Combined ##################################################################

mod_all1030 <- hierarchical_plot(dir = save_dir,
                              dat = all1030,
                              filenote = "combined_demersal_pelagic_10_30",
                              titlenote = "10-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_all3050 <- hierarchical_plot(dir = save_dir,
                                 dat = all3050,
                                 filenote = "combined_demersal_pelagic_30-50",
                                 titlenote = "30-50 fathoms",
                                 Nsim = 5e4,
                                 ymax = 10,
                                 h_inch = 14)

mod_all50plus <- hierarchical_plot(dir = save_dir,
                                 dat = all50plus,
                                 filenote = "combined_demersal_pelagic_50_plus",
                                 titlenote = "50+ fathoms",
                                 Nsim = 5e4,
                                 ymax = 10)


# Dwarf Only ##########################################################################

mod_dwarf3050 <- hierarchical_plot(dir = save_dir,
                              dat = dwarf3050,
                              filenote = "dwarf_30-50",
                              titlenote = "Dwarf 30-50 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

###################################################################################################

save(mod_pelagic1030,
     mod_pelagic3050,
     mod_pelagic50plus,
     mod_demersal1030,
     mod_demersal3050,
     mod_demersal50plus,
     mod_all1030, 
     mod_all3050,
     mod_all50plus,
     mod_dwarf3050,
     file = file.path(save_dir, "model_estimates.Rdata"))


#################################################################################################
# Calculate the Percentiles
#################################################################################################

mort_pelagic1030   <- get_mu(data = mod_pelagic1030)
mort_pelagic3050   <- get_mu(data = mod_pelagic3050)
mort_pelagic50plus <- get_mu(data = mod_pelagic50plus)

mort_demersal1030 <- get_mu(data = mod_demersal1030)
mort_demersal3050 <- get_mu(data = mod_demersal3050)
mort_demersal50plus <- get_mu(data = mod_demersal50plus)

mort_all1030  <- get_mu(data = mod_all1030)
mort_all3050  <- get_mu(data = mod_all3050)
mort_all50plus <- get_mu(data = mod_all50plus)

mort_dwarf3050 <- get_mu(data = mod_dwarf3050)

###############################################################################################
# Create estimate table
###############################################################################################
if (save_dir == file.path(dir, "analysis")) {
    plus50_lt_mort = 0
    plus50_lt_mort_demersal = 0
} 
if (save_dir == file.path(dir, "shortterm")) {
    plus50_lt_mort = grouped_lt_mort
    plus50_lt_mort_demersal = demersal_lt_mort 
}else {
    plus50_lt_mort = grouped_lt_mort / 2
    plus50_lt_mort_demersal = demersal_lt_mort / 2
}

# bocaccio 50-100 mort < 30-50 mort - only deep species
pelagic = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_pelagic1030, mort_pelagic3050, mort_pelagic50plus),
              depth_bins = c("10-30", "30-50", "50-100"),
              pi = "90%",
              guild = "pelagic",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05, 
              plus50_lt_mort = plus50_lt_mort)

demersal = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_demersal1030, mort_demersal3050, mort_demersal50plus),
              depth_bins = c("10-30", "30-50", "50-100"),
              pi = "90%",
              guild = "demersal",
              long_term_mort = demersal_lt_mort,
              add_mort = 0.05, 
              plus50_lt_mort = plus50_lt_mort_demersal)

combined = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_all1030, mort_all3050, mort_all50plus),
              depth_bins = c("10-30", "30-50", "50-100"),
              pi = "90%",
              guild = "combined",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05, 
              plus50_lt_mort = plus50_lt_mort)


dwarf = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_dwarf3050),
              depth_bins = c("30-50"),
              pi = "90%",
              guild = "dwarf",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05)

###################################################################################################

save(pelagic,
     demersal,
     combined,
     dwarf,
     file = file.path(save_dir, "ci_estimates.Rdata"))

#################################################################################################

plot_post_model_pre_data(
  dir = save_dir, 
  data_list = list(mod_pelagic1030, mod_pelagic3050, mod_pelagic50plus), 
  line_names = c("10-30 fathoms", "30-50 fathoms", "50+ fathoms"), 
  gamma_prior = gamma_prior,
  file_add = "all_pelagic")  

plot_post_model_pre_data(
  dir = save_dir, 
  data_list = list(mod_demersal1030, mod_demersal3050, mod_demersal50plus), 
  line_names = c("10-30 fathoms", "30-50 fathoms", "50+ fathoms"), 
  file_add = "all_demersal",
  gamma_prior = gamma_prior,
  ymax = 18)

plot_post_model_pre_data(
  dir = save_dir, 
  data_list = list(mod_all1030, mod_all3050, mod_all50plus)[2], 
  line_names = c("10-30 fathoms", "30-50 fathoms", "50+ fathoms")[2], 
  file_add = "all_combined",
  gamma_prior = gamma_prior,
  ymax = 18)

plot_post_model_pre_data(
  dir = save_dir, 
  data_list = list(mod_dwarf3050), 
  line_names = c("30-50 fathoms"), 
  gamma_prior = gamma_prior,
  file_add = "all_dwarf")

#################################################################################################

plot_rates_by_depth(dir = save_dir, 
    data = pelagic, group = "Pelagic", ci = "90%")

plot_rates_by_depth(dir = save_dir, 
    data = demersal, group = "Demersal", ci = "90%")

plot_rates_by_depth(dir = save_dir, 
    data = combined, group = "Combined_Observations", ci = "90%")

plot_rates_by_depth(dir = save_dir, 
    data = dwarf, group = "Dwarf", ci = "90%")


################################################################################################

sub_baro$Group = NA
find = which(sub_baro$Guild %in% c("Shallow Demersal", "Deep Demersal"))
sub_baro$Group[find] = "Demersal"
find = which(sub_baro$Guild %in% c("Shallow Pelagic", "Deep Pelagic"))
sub_baro$Group[find] = "Pelagic"
find = which(sub_baro$Guild %in% c("Dwarf"))
sub_baro$Group[find] = "Dwarf"

for(g in sort(unique(sub_baro$Group))){
    tmp <- sub_baro[sub_baro$Group == g, ]
    sp <- sort(unique(tmp$Species))
    quants <- col_names <- NULL
    for(b in unique(tmp$Depth.Bin)) {
        values <- NULL
        tmp2 <- tmp[tmp$Depth.Bin == b, ]
        for(a in sp) {
          n <- sum(tmp2$Species == a)
          dead <- sum(tmp2[tmp2$Species == a, "dead"])
          mort <- dead / n
          values <- rbind(values, c(dead, n, mort))
        }
        col_names = c(col_names, c(paste0("dead_",b), 
                      paste0("N_", b), 
                      paste0("Mortality_", b)))
        quants <- cbind(quants, values)
    }
    rownames(quants) = sp
    colnames(quants) = col_names
    write.csv(quants, file.path(save_dir, paste0("Used_Observations_by_Depth_Species_", g, ".csv")))
}

################################################################################################

baro$Group = NA
find = which(baro$Guild %in% c("Shallow Demersal", "Deep Demersal"))
baro$Group[find] = "Demersal"
find = which(baro$Guild %in% c("Shallow Pelagic", "Deep Pelagic"))
baro$Group[find] = "Pelagic"
find = which(baro$Guild %in% c("Dwarf"))
baro$Group[find] = "Dwarf"

for(g in sort(unique(baro$Group))){
    tmp <- baro[baro$Group == g, ]
    sp <- sort(unique(tmp$Species))
    quants <- col_names <- NULL
    for(b in unique(tmp$Depth.Bin)) {
        values <- NULL
        tmp2 <- tmp[tmp$Depth.Bin == b, ]
        for(a in sp) {
          n <- sum(tmp2$Species == a)
          dead <- sum(tmp2[tmp2$Species == a, "dead"])
          mort <- dead / n
          values <- rbind(values, c(dead, n, mort))
        }
        col_names = c(col_names, c(paste0("dead_",b), 
                      paste0("N_", b), 
                      paste0("Mortality_", b)))
        quants <- cbind(quants, values)
    }
    rownames(quants) = sp
    colnames(quants) = col_names
    write.csv(quants, file.path(save_dir, paste0("All_Observations_by_Depth_Species_", g, ".csv")))
}