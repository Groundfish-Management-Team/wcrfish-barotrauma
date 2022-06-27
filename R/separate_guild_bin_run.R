##########################################################################################
# Run JAGS model by guild and depth grouping

# Load existing data groupings
# load(file.path(dir, "analysis", "datagrouping_guild_specific.Rdata"))

# Shallow Pelagic
mod_shallow_pelagic_0_10 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = shallow_pelagic_0_10,
                              filenote = "shallow pelagic_0-10",
                              titlenote = "Shallow Pelagic < 10 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_shallow_pelagic_10_20 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = shallow_pelagic_10_20,
                              filenote = "shallow pelagic_10-20",
                              titlenote = "Shallow Pelagic 10-20 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_shallow_pelagic_20_30 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = shallow_pelagic_20_30,
                              filenote = "shallow pelagic_20-30",
                              titlenote = "Shallow Pelagic 20-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

# Deep Pelagic
mod_deep_pelagic_10_20 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_pelagic_10_20,
                              filenote = "deep pelagic_10-20",
                              titlenote = "Deep Pelagic 10-20 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_deep_pelagic_20_30 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_pelagic_20_30,
                              filenote = "deep pelagic_20-30",
                              titlenote = "Deep Pelagic 20-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_deep_pelagic_30_40 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_pelagic_30_40,
                              filenote = "deep pelagic_30-40",
                              titlenote = "Deep Pelagic 30-40 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_deep_pelagic_40_50 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_pelagic_40_50,
                              filenote = "deep pelagic_40-50",
                              titlenote = "Deep Pelagic 40-50 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_deep_pelagic_50plus <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_pelagic_50plus,
                              filenote = "deep pelagic_50+",
                              titlenote = "Deep Pelagic > 50 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

# Shallow Demersal
mod_shallow_demersal_10_20 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = shallow_demersal_10_20,
                              filenote = "shallow demersal_10-20",
                              titlenote = "Shallow Demersal 10-20 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_shallow_demersal_20_30 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = shallow_demersal_20_30,
                              filenote = "shallow demersal_20-30",
                              titlenote = "Shallow Demersal 20-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

# Deep Demersal
mod_deep_demersal_10_30 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_demersal_10_30,
                              filenote = "deep demersal_10_30",
                              titlenote = "Deep Demersal 10-30 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_deep_demersal_30_50 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_demersal_30_50,
                              filenote = "deep demersal_30_50",
                              titlenote = "Deep Demersal 30-50 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_deep_demersal_50plus <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = deep_demersal_50plus,
                              filenote = "deep demersal_50+",
                              titlenote = "Deep Demersal > 50 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

# Dwarf 
mod_dwarf_30_40 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = dwarf_30_40,
                              filenote = "dwarf_30_40",
                              titlenote = "Dwarf 30-40 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

mod_dwarf_40_50 <- hierarchical_plot(dir = file.path(dir, "analysis"),
                              dat = dwarf_30_40,
                              filenote = "dwarf_30_40",
                              titlenote = "Dwarf 30-40 fathoms",
                              Nsim = 5e4,
                              ymax = 10)

save(mod_shallow_pelagic_0_10,
     mod_shallow_pelagic_10_20,
     mod_shallow_pelagic_20_30,
     mod_deep_pelagic_10_20,
     mod_deep_pelagic_20_30,
     mod_deep_pelagic_30_40,
     mod_deep_pelagic_40_50,
     mod_deep_pelagic_50plus,
     mod_shallow_demersal_10_20,
     mod_shallow_demersal_20_30,
     mod_deep_demersal_10_30,
     mod_deep_demersal_30_50,
     mod_deep_demersal_50plus,
     mod_dwarf_30_40, 
     mod_dwarf_40_50,
     file = file.path(dir, "analysis", "model_estimates.Rdata"))