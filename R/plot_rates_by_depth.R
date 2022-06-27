
plot_rates_by_depth <- function(dir, data, group, ci = "90%")
{

  # data = demersal
  data$depth_min <- as.numeric(substring(data$depth_bin, first=1, last=2))
  data$depth_max <- as.numeric(substring(data$depth_bin, first=4))
  n = sort(unique(data$species))

  colvec <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
              "#44AA99", "#999933", "#882255", "#661100", "#0072B2", "#6699CC", "#888888")

  png(file.path(dir, paste0('Barotrauma_plot_', group, '.png')), res = 300, units = 'in', width = 8, height = 6)

  par(mfrow = c(1, 1), mar = c(5, 0.6, 1, 0.6), oma = c(0, 4, 3, 4), cex.main = 1)
  plot(0, type = 'n', xlim = c(0, 70), ylim = c(0, 1.05), xaxs = 'i', yaxs = 'i', axes = FALSE,
          xlab = "Depth (fathoms)", ylab = "", main = paste0("Hierarchical method (", group, ")"))
  abline(h = seq(0, 1, 0.2), lty = 3, col = 'grey')
  axis(1, at = c(0, 10, 30, 50, 100))
  axis(4, las = 1, at = seq(0, 1, 0.2), lab = paste0(seq(0, 1, 0.2)*100,"%"))

  #colvec <- rgb(0, 0, seq(1, 0.6, length = length(n)))#, alpha = seq(0.8, 0.4, length = length(n)))
  #quantlabs <- c("Surface","Point\nestimate","60%","75%","90%","95%")
  ind = 0
  for(a in n){

    lwd_val = 2
    lty_val = 1
    ind = ind + 1
    tab <- data[data$species == a, ] 
    use_col <- colvec[ind]    

    yvec <- xvec <- NULL
    for(b in 1:dim(tab)[1]) {
      yvec <- c(yvec, rep(as.numeric(tab[b, ci]), 2))
      xvec <- c(xvec, c(tab[b, "depth_min"], tab[b, "depth_max"]))
    }
    #xvec <- c(5,15,25,40,60)
    if (a == "unobserved") {
      use_col = rep(1, length(n))[ind]
      lwd_val = 3
      lty_val = 1
    }

    lines(xvec, yvec, lwd = lwd_val, lty = lty_val, col = use_col, cex = 1)    
    #mtext(side = 3, line = 1, outer = TRUE, paste("Total discard mortality estimates for",n),
    #  font = 2, cex = 1.2)
  }
  mtext(side = 2, line = 2.7, outer = TRUE, paste0("Total discard mortality (", ci, " Confidence Interval)")) 
  # add lengend
  legend('topleft', bty = 'n', legend = c("unobserved", n[n != "unobserved"]),
    lwd = c(3, rep(2, length(n)-1)), lty = 1, col = c(1, colvec[1:(length(n)-1)]))

  dev.off()
}
