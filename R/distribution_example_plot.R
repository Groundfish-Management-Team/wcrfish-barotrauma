set.seed(9233)
source("C:/Handy_Code/print_letter.R")
load("C:/assessments/Council/GMT/wcrfish-barotrauma/uninformed_gamma/analysis_longterm_adj/model_estimates.Rdata")
dist <- 100 * mod_all3050$Jags$BUGSoutput$sims.matrix[, "mu[4]"]

# Figure labeling function used in the hierarchical function
perc <- function(x, digits = 0){
  text <- ifelse(is.na(x),"NA", paste(round(100 * x, digits), "%", sep = ""))
  return(text)
}

rtnorm <- function(n, mean = 0, sd = 1, min = 0, max = 1) {
    bounds <- pnorm(c(min, max), mean, sd)
    u <- runif(n, bounds[1], bounds[2])
    qnorm(u, mean, sd)
}

mysamp <- rtnorm(1e7, 0.45, 0.15)

#h1 <- hist(mysamp, col = 'grey', border = 'grey',
#           breaks = seq(0, 1, 0.01), main = "test",
#           freq = FALSE, xlab = "", ylab = "", axes = FALSE)

d <- density(dist)
q <- quantile(dist, c(0.5, 0.8, 0.9))
colors <- RColorBrewer::brewer.pal(3, "Blues")

png('C:/assessments/Council/GMT/wcrfish-barotrauma/uninformed_gamma/analysis_longterm_adj/distribution_example.png',
    width = 8, height = 10, res = 300, units = 'in')
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1), oma = c(2,2,1,1))

plot(d, xlim = c(0, 100), ylim = c(0, 0.04), main = "", axes = FALSE,
	ylab = "", xlab = "", 
	xaxs = "i", yaxs = "i")
axis(side = 1); box()
mtext(side = 1, "Estimated Discard Mortality (%)", line = 3)
mtext(side = 2, "Density", line = 2)
polygon(c(d$x[d$x <= q[1]], q[1]), c(d$y[d$x <= q[1]], 0),
	col = scales::alpha(colors[3], 1) , border = NA)
polygon(c(q[1], d$x[d$x >=q[1] & d$x <= q[2]], q[2]), c(0, d$y[d$x >= q[1] & d$x <= q[2]], 0),
	col = scales::alpha(colors[2], alpha = 1), border = NA)
polygon(c(q[2], d$x[d$x >=q[2] & d$x <= q[3]], q[3]), c(0, d$y[d$x >= q[2] & d$x <= q[3]], 0),
	col = scales::alpha(colors[1], alpha = 1), border = NA)
lines(rep(q[1], 2), c(0, 0.035), lwd = 2, lty = 2, col = 1)
lines(rep(q[2], 2), c(0, 0.025), lwd = 2, lty = 2, col = 1)
lines(rep(q[3], 2), c(0, 0.015), lwd = 2, lty = 2, col = 1)
print.letter(xy = c(0.67, 0.93), label = "50% of values are expected to be less than")
print.letter(xy = c(0.73, 0.67), label = "80% of values are expected")
print.letter(xy = c(0.66, 0.61), label = "to be less than")
print.letter(xy = c(0.82, 0.47), label = "90% of values are expected")
print.letter(xy = c(0.74, 0.41), label = "to be less than")
print.letter(xy = c(0.02, 0.95), label = "a)", cex = 1.2)

x <- seq(0, 1, 0.01)
calc <- function(x, add){ 
	100*(1-(1-x)*(1-add)) }
colvec <- scales::viridis_pal()(5)
val <- 0.40

plot(x*100, calc(x, 0), type = 'l', lwd = 2, col = colvec[1], 
	ylab = "Cumulative Mortality (%)", xlab = "Estimated Discard Mortality (%)",
	xaxs = "i", yaxs = "i")
lines(x * 100, calc(x, 0.05), lty = 2, lwd = 2, col = colvec[2])
lines(x * 100, calc(x, 0.10), lty = 3, lwd = 2, col = colvec[3])
lines(x * 100, calc(x, 0.20), lty = 4, lwd = 2, col = colvec[4])
lines(c(val * 100, val * 100), c(0, calc(val, 0.2)), col = 'grey30', lty = 2)
lines(c(0, val * 100), c(val * 100, val * 100), col = 'grey30', lty = 2)
lines(c(0, val * 100), rep(calc(val, 0.05), 2), col = 'grey30', lty = 2)
lines(c(0, val * 100), rep(calc(val, 0.10), 2), col = 'grey30', lty = 2)
lines(c(0, val * 100), rep(calc(val, 0.20), 2), col = 'grey30', lty = 2)
legend('bottomright', bty = 'n', col = colvec[1:4], lwd = rep(2,4), lty = 1:4, cex = 1.2,
	legend = c("Add. Unaccounted Mort. = 0%", 
	"Add. Unaccounted Mort. = 5%", 
	"Add. Unaccounted Mort. = 10%", 
	"Add. Unaccounted Mort. = 20%"))
print.letter(xy = c(0.02, 0.95), label = "b)", cex = 1.2)
print.letter(xy = c(0.05, calc(val, 0.2)/100 + 0.03), label = "52%")
print.letter(xy = c(0.13, calc(val, 0.1)/100 + 0.03), label = "46%")
print.letter(xy = c(0.05, calc(val, 0.05)/100 + 0.02), label = "43%")
print.letter(xy = c(0.13, calc(val, 0)/100 - 0.03), label = "40%")
dev.off()

# Percentiles by guild
demersal <- rbind(
	quantile(100 * mod_demersal1030$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)),
	quantile(100 * mod_demersal3050$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)),
	quantile(100 * mod_demersal50plus$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)))

pelagic <- rbind(
	quantile(100 * mod_pelagic1030$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)),
	quantile(100 * mod_pelagic3050$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)),
	quantile(100 * mod_pelagic50plus$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)))

combined <- rbind(
	quantile(100 * mod_all1030$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)),
	quantile(100 * mod_all3050$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)),
	quantile(100 * mod_all50plus$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)))

dwarf <- rbind(
	quantile(100 * mod_dwarf3050$Jags$BUGSoutput$sims.matrix[, "mupred"], seq(0.5, 0.9, 0.1)))

png('C:/assessments/Council/GMT/wcrfish-barotrauma/uninformed_gamma/percentiles_by_guild.png',
    width = 10, height = 10, res = 300, units = 'in')
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(2,2,1,1))
lab_vec = c("50th", "60th", "70th", "80th", "90th")
	
plot(0:2, pelagic[,1], axes = FALSE, type = 'l', col = colvec[1], lwd = 2, ylim = c(0, 102),
	main = "Pelagic", xlab = "", ylab = "", xlim = c(0, 2.25))
print.letter(xy = c(0.9, pelagic[3,1] / 100 - 0.03), label = lab_vec[1])
print.letter(xy = c(0.03, 0.95), "a)")
for (a in 2:5){
	lines(0:2, pelagic[, a], lwd = 2, lty = 1, col = colvec[a])
	print.letter(xy = c(0.9, pelagic[3,a] / 100 - 0.03), label = lab_vec[a])
}
box();
axis(side = 1, at = 0:2, label = c("10", "30", "50"))
mtext(side = 1, "Depth (fm)", line = 2)
axis(side = 2); mtext(side = 2, "Estimated Mortality (%)", line = 2.5)

plot(0:2, demersal[,1], axes = FALSE, type = 'l', col = colvec[1], lwd = 2, ylim = c(0, 100),
	main = "Demersal", xlab = "", ylab = "", xlim = c(0, 2.25))
print.letter(xy = c(0.9, demersal[3,1] / 100), label = lab_vec[1])
print.letter(xy = c(0.03, 0.95), "b)")
for (a in 2:5){
	lines(0:2, demersal[, a], lwd = 2, lty = 1, col = colvec[a])
	print.letter(xy = c(0.9, demersal[3,a] / 100), label = lab_vec[a])
}
box();
axis(side = 1, at = 0:2, label = c("10", "30", "50"))
mtext(side = 1, "Depth (fm)", line = 2)
axis(side = 2); mtext(side = 2, "Estimated Mortality (%)", line = 2.5)
legend("topright", bty = 'n', col = colvec, lwd = rep(2, 5), lty = rep(1, 5),
	legend = c("50th Percentile", "60th Percentile", "70th Percentile",
 	"80th Percentile", "90th Percentile"))

plot(0:2, combined[,1], axes = FALSE, type = 'l', col = colvec[1], lwd = 2, ylim = c(0, 100),
	main = "Pelagic & Demersal", xlab = "", ylab = "", xlim = c(0, 2.25))
print.letter(xy = c(0.9, combined[3,1] / 100), label = lab_vec[1])
print.letter(xy = c(0.03, 0.95), "c)")
for (a in 2:5){
	lines(0:2, combined[, a], lwd = 2, lty = 1, col = colvec[a])
	print.letter(xy = c(0.9, combined[3,a] / 100), label = lab_vec[a])
}
box();
axis(side = 1, at = 0:2, label = c("10", "30", "50"))
mtext(side = 1, "Depth (fm)", line = 2)
axis(side = 2); mtext(side = 2, "Estimated Mortality (%)", line = 2.5)

plot(1, dwarf[,1], axes = FALSE, type = 'p', col = colvec[1], pch = 16, lwd = 2, ylim = c(0, 100),
	main = "Dwarf", xlab = "", ylab = "", xlim = c(0, 2.25))
print.letter(xy = c(0.52, dwarf[1] / 100 - 0.01), label = lab_vec[1])
print.letter(xy = c(0.03, 0.95), "d)")
for (a in 2:5){
	points(1, dwarf[a], lwd = 2, pch = 16, col = colvec[a])
	print.letter(xy = c(0.52, dwarf[a] / 100 - 0.01), label = lab_vec[a])
}
box();
axis(side = 1, at = 0:2, label = c("10", "30", "50"))
mtext(side = 1, "Depth (fm)", line = 2)
axis(side = 2); mtext(side = 2, "Estimated Mortality (%)", line = 2.5)

dev.off()