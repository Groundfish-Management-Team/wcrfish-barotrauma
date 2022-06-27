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

# set the workding directory
dir <-  "C:/Assessments/Council/GMT/barotrauma"
save_dir <- file.path(dir, "analysis")
#save_dir <- file.path(dir, "analysis_longterm_adj")

# load functions
# function that runs jags and creates distribution plots
source(file.path(dir, "R", "hierarchical_plot.R"))
source(file.path(dir, "R", "functions.R"))
source(file.path(dir,"R", "calc_cum_mort.R"))
source(file.path(dir,"R", "process_data.R"))
source(file.path(dir, "R", "create_jags_model.R"))
source(file.path(dir, "R", "plot_post_model_pre_data.R"))
source(file.path(dir, "R", "plot_rates_by_depth.R"))

# load the data from 2022
baro <- read.csv(file.path(dir, "data", "MortalityData2022.csv"), stringsAsFactors = FALSE)

out = clean_data(data = baro, save_dir = save_dir)
sub_baro = out$sub_data 
demersal_lt_mort = out$demersal_lt_mort
pelagic_lt_mort  = out$pelagic_lt_mort 
grouped_lt_mort  = out$grouped_lt_mort 

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
###########################################################################################
# All Pelagic Combined 
###########################################################################################

pelagic1030 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("10-30") & 
                      sub_baro$Guild %in% c("Deep Pelagic", "Shallow Pelagic"), ] 
pelagic3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") & 
                    sub_baro$Guild %in% c("Deep Pelagic", "Shallow Pelagic"), ]
pelagic50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") & 
                      sub_baro$Guild %in% c("Deep Pelagic", "Shallow Pelagic"), ]

###########################################################################################
# Deep Pelagic 
###########################################################################################

deep_pelagic3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") & 
                         sub_baro$Guild %in% c("Deep Pelagic"), ] 
deep_pelagic50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") & 
                         sub_baro$Guild %in% c("Deep Pelagic"), ] 

###########################################################################################
# Shallow Pelagic 
###########################################################################################                         

shallow_pelagic10less <- sub_baro[sub_baro$Depth.Bin.fm %in% c("0-10") & 
                             sub_baro$Guild %in% c("Shallow Pelagic"), ] 
shallow_pelagic1030 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("10-30") & 
                             sub_baro$Guild %in% c("Shallow Pelagic"), ] 

#####################################################################################################
# All Demersal
###########################################################################################                             

demersal1030 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("10-30") &
                      sub_baro$Guild %in% c("Deep Demersal", "Shallow Demersal"), ] 
demersal3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") &
                     sub_baro$Guild %in% c("Deep Demersal", "Shallow Demersal"), ]
demersal50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") &
                       sub_baro$Guild %in% c("Deep Demersal", "Shallow Demersal"), ]    

###########################################################################################
# Deep Demersal 
###########################################################################################                       
                    
deep_demersal3050 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("30-50") &
                         sub_baro$Guild %in% c("Deep Demersal"), ]
deep_demersal50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") &
                           sub_baro$Guild %in% c("Deep Demersal"), ] 

###########################################################################################
# Shallow Demersal 
###########################################################################################
 
shallow_demersal1030 <- sub_baro[sub_baro$Depth.Bin.fm %in% c("10-30") &
                         sub_baro$Guild %in% c("Shallow Demersal"), ] 

###########################################################################################
# Deep (Demersal + Pelagic)
###########################################################################################
deep50plus <- sub_baro[sub_baro$Depth.Bin.fm %in% c("50-100") &
                           sub_baro$Guild %in% c("Deep Demersal", "Deep Pelagic"), ] 

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
     shallow_pelagic10less,
     shallow_pelagic1030,
     deep_pelagic3050,
     deep_pelagic50plus, 
     demersal1030,
     demersal3050,
     demersal50plus,
     deep_demersal3050, 
     deep_demersal50plus,
     shallow_demersal1030, 
     deep50plus,
     dwarf3050,
     file = file.path(save_dir, "datagrouping.Rdata"))
 
##########################################################################################
create_jags_model(save_dir = save_dir)
##########################################################################################
# Run JAGS model by guild and depth grouping
# Load existing data groupings
# load(file.path(dir, "analysis", "datagrouping.Rdata"))

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


# Shallow Pelagic ####################################################################
mod_shallow_pelagic10less <- hierarchical_plot(dir = save_dir,
                              dat = shallow_pelagic10less,
                              filenote = "shallow_pelagic_0-10",
                              titlenote = "Shallow Pelagic 0-10 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_shallow_pelagic1030 <- hierarchical_plot(dir = save_dir,
                              dat = shallow_pelagic1030,
                              filenote = "shallow_pelagic_10-30",
                              titlenote = "Shallow Pelagic 10-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

# Deep Pelagic ########################################################################
mod_deep_pelagic3050 <- hierarchical_plot(dir = save_dir,
                              dat = deep_pelagic3050,
                              filenote = "deep_pelagic_30-50",
                              titlenote = "Deep Pelagic 30-50 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_deep_pelagic50plus <- hierarchical_plot(dir = save_dir,
                              dat = deep_pelagic50plus,
                              filenote = "deep_pelagic_50_plus",
                              titlenote = "Deep Pelagic > 50 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

# Demersal Shallow & Deep Together ###############################################################
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
                                 ymax = 10)

mod_demersal50plus <- hierarchical_plot(dir = save_dir,
                                 dat = demersal50plus,
                                 filenote = "demersal_50_plus",
                                 titlenote = "Demersal 50+ fathoms",
                                 Nsim = 5e4,
                                 ymax = 10)

# Demersal Deep Only ##################################################################
mod_deep_demersal3050 <- hierarchical_plot(dir = save_dir,
                                 dat = deep_demersal3050,
                                 filenote = "deep_demersal_30-50",
                                 titlenote = "Deep Demersal 30-50 fathoms",
                                 Nsim = 5e4,
                                 ymax = 10)

mod_deep_demersal50plus <- hierarchical_plot(dir = save_dir,
                                 dat = deep_demersal50plus,
                                 filenote = "deep_demersal_50_plus",
                                 titlenote = "Deep Demersal 50+ fathoms",
                                 Nsim = 5e4,
                                 ymax = 10)

# Demersal Shallow Only ###############################################################
mod_shallow_demersal1030 <- hierarchical_plot(dir = save_dir,
                              dat = shallow_demersal1030,
                              filenote = "shallow_demersal_10_30",
                              titlenote = "Shallow Demersal 10-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)
# Deep Combined ######################################################################
mod_deep50plus <- hierarchical_plot(dir = save_dir,
                                 dat = deep50plus,
                                 filenote = "deep_50_plus",
                                 titlenote = "Deep 50+ fathoms",
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
     mod_shallow_pelagic10less,
     mod_shallow_pelagic1030,
     mod_deep_pelagic3050,
     mod_deep_pelagic50plus,
     mod_demersal1030,
     mod_demersal3050,
     mod_demersal50plus,
     mod_deep_demersal3050,
     mod_deep_demersal50plus,
     mod_shallow_demersal1030, 
     mod_deep50plus,
     mod_dwarf3050,
     file = file.path(save_dir, "model_estimates.Rdata"))

#################################################################################################
# Calculate the confidence intervals
#################################################################################################

mort_pelagic1030 <- get_mu(data = mod_pelagic1030)
mort_pelagic3050   <- get_mu(data = mod_pelagic3050)
mort_pelagic50plus <- get_mu(data = mod_pelagic50plus)

mort_shallow_pelagic1030 <- get_mu(data = mod_shallow_pelagic1030)
mort_shallow_pelagic10less <- get_mu(data = mod_shallow_pelagic10less)

mort_deep_pelagic3050 <- get_mu(data = mod_deep_pelagic3050)
mort_deep_pelagic50plus <- get_mu(data = mod_deep_pelagic50plus)

mort_demersal1030 <- get_mu(data = mod_demersal1030)
mort_demersal3050 <- get_mu(data = mod_demersal3050)
mort_demersal50plus <- get_mu(data = mod_demersal50plus)

mort_deep_demersal3050 <- get_mu(data = mod_deep_demersal3050)
mort_deep_demersal50plus <- get_mu(data = mod_deep_demersal50plus)

mort_shallow_demersal1030<- get_mu(data = mod_shallow_demersal1030)

mort_deep50plus <- get_mu(data = mod_deep50plus)

mort_dwarf3050 <- get_mu(data = mod_dwarf3050)

###############################################################################################
# Create estimate table
###############################################################################################

# bocaccio 50-100 mort < 30-50 mort - only deep species
pelagic = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_pelagic1030, mort_pelagic3050, mort_pelagic50plus),
              depth_bins = c("10-30", "30-50", "50-100"),
              ci = "90%",
              guild = "pelagic",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05)

demersal = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_demersal1030, mort_demersal3050, mort_demersal50plus),
              depth_bins = c("10-30", "30-50", "50-100"),
              ci = "90%",
              guild = "demersal",
              long_term_mort = demersal_lt_mort,
              add_mort = 0.05)

shallow_demersal = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_shallow_demersal1030),
              depth_bins = c("10-30"),
              ci = "90%",
              guild = "shallow_demersal",
              long_term_mort = demersal_lt_mort,
              add_mort = 0.05)

shallow_pelagic = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_shallow_pelagic10less, mort_shallow_pelagic1030),
              depth_bins = c("0-10", "10-30"),
              ci = "90%",
              guild = "shallow_pelagic",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05)


deep_demersal = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_deep_demersal3050, mort_deep_demersal50plus),
              depth_bins = c("30-50", "50-100"),
              ci = "90%",
              guild = "deep_demersal",
              long_term_mort = demersal_lt_mort,
              add_mort = 0.05)

# bocaccio 50-100 mort < 30-50 mort
deep_pelagic = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_deep_pelagic3050, mort_deep_pelagic50plus),
              depth_bins = c("30-50","50-100"),
              ci = "90%",
              guild = "deep_pelagic",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05)

deep_pelagic = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_deep50plus),
              depth_bins = c("50-100"),
              ci = "90%",
              guild = "deep",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05)

dwarf = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_dwarf3050),
              depth_bins = c("30-50"),
              ci = "90%",
              guild = "dwarf",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05)

deep = calc_cum_mort(dir = save_dir, 
              dat_list = list(mort_deep50plus),
              depth_bins = c("50-100"),
              ci = "90%",
              guild = "deep",
              long_term_mort = grouped_lt_mort,
              add_mort = 0.05)


###################################################################################################

save(pelagic,
     shallow_pelagic,
     deep_pelagic,
     demersal,
     deep_demersal,
     shallow_demersal, 
     deep,
     dwarf,
     file = file.path(save_dir, "ci_estimates.Rdata"))

#################################################################################################

plot_post_model_pre_data(
  dir = save_dir, 
  data_list = list(mod_pelagic1030, mod_pelagic3050, mod_pelagic50plus), 
  line_names = c("10-30 fathoms", "30-50 fathoms", "50+ fathoms"), 
  file_add = "all_pelagic")  

plot_post_model_pre_data(
  dir = save_dir, 
  data_list = list(mod_demersal1030, mod_demersal3050, mod_demersal50plus), 
  line_names = c("10-30 fathoms", "30-50 fathoms", "50+ fathoms"), 
  file_add = "all_demersal")

#################################################################################################

plot_rates_by_depth(dir = save_dir, 
    data = pelagic, group = "Pelagic", ci = "90%")

plot_rates_by_depth(dir = save_dir, 
    data = demersal, group = "Demersal", ci = "90%")

plot_rates_by_depth(dir = save_dir, 
    data = dwarf, group = "Dwarf", ci = "90%")

plot_rates_by_depth(dir = save_dir, 
    data = deep, group = "Deep_Observations", ci = "90%")

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