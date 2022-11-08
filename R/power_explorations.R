library(pwr)
library(plotly); library(stringr); library(reshape2); 
library(metR); library(ggplot2); library(HandyCode)

p1 <- 0.50 #seq(0.25, 1, 0.25)
p2 <- seq(0.01, 1, 0.01)
n  <- 1:75

grid <- expand.grid(p1, p2, n)
names(grid) <- c("p1", "p2", "n")

output <- matrix(
	NA,
	nrow = nrow(grid),
	ncol = 4,
	dimnames = list(NULL, c("p1", "p2", "n", "power")))


for(i in 1:nrow(grid)){
	a <- power.prop.test(
			n  = grid[i, "n"], 
			p1 = grid[i, "p1"], 
			p2 = grid[i, "p2"], 
			strict = TRUE)
	output[i, ] <- c(grid[i, "p1"], grid[i, "p2"], grid[i, "n"], a$power)
}

power.prop.test(
			n  = 30, 
			p1 = 0.50, 
			p2 = 0.90, 
			strict = TRUE)


x <- unique(output[ , "p2"])
y <- unique(output[ , "n"])
z <- matrix(output[ , "power"],
	ncol = length(y),
	byrow = TRUE,
	dimnames = list(as.character(x), as.character(y)))
mtrx_melt <- reshape2::melt(z, id.vars = c("p2", "n"), measure.vars = "Power")
names(mtrx_melt) <- c("p2", "n", "Power")

ggplot(mtrx_melt, aes(x = p2, y = n)) +
    geom_contour_filled(aes(z = Power), breaks = seq(0, 1, 0.05)) +
    #geom_text_contour(aes(z = Power), 
    #   breaks = seq(0.10, 1, 0.10), size = 7, color = 'white') +
    xlab("P2") +
    ylab("N") +
    theme(
      axis.text.y = element_text(size = 15, color = 1),
      axis.text.x = element_text(size = 15, color = 1), 
      axis.title.x = element_text(size = 20), 
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 15), 
      legend.title = element_text(size = 15)) +
    guides(fill = guide_legend(title = "Power"))


# Find N for power of 0.80
p1 <- seq(0.40, 0.60, 0.02)
p2 <- seq(0.01, 0.20, 0.02)
power <- 0.80

grid <- expand.grid(p1, p2, power)
names(grid) <- c("p1", "p2", "power")

output <- matrix(
	NA,
	nrow = nrow(grid),
	ncol = 4,
	dimnames = list(NULL, c("p1", "p2", "power", "n")))


for(i in 1:nrow(grid)){
	a <- power.prop.test(
			p1 = grid[i, "p1"], 
			p2 = grid[i, "p2"], 
			power = 0.80,
			strict = TRUE)
	output[i, ] <- c(grid[i, "p1"], grid[i, "p2"], grid[i, "power"], ceiling(a$n))
}

x <- unique(output[ , "p1"])
y <- unique(output[ , "p2"])
z <- matrix(output[ , "n"],
	ncol = length(y),
	byrow = TRUE,
	dimnames = list(as.character(x), as.character(y)))
mtrx_melt <- reshape2::melt(z, id.vars = c("p1", "p2"), measure.vars = "n")
names(mtrx_melt) <- c("p1", "p2", "n")

ggplot(mtrx_melt, aes(x = p1, y = p2)) +
    geom_contour_filled(aes(z = n), breaks = seq(1, 1000, 10)) +
    geom_text_contour(aes(z = n), 
       breaks = seq(10, 100, 10), size = 7, color = 'white') +
    xlab("P2") +
    ylab("N") +
    theme(
      axis.text.y = element_text(size = 15, color = 1),
      axis.text.x = element_text(size = 15, color = 1), 
      axis.title.x = element_text(size = 20), 
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 15), 
      legend.title = element_text(size = 15)) +
    guides(fill = guide_legend(title = "Power"))


# Two proportions with separate sample sizes
# Bocaccio versus Canary
n1 = 34;  n2 = 10
p1 = 0.14; p2 = 0.71
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))
pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = 0.05)
# power = 0.93

# Bocaccio versus Bocaccio
n1 = 64;  n2 = 34
p1 = 0.18; p2 = 0.14
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))
pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = 0.05)
# power = 0.081

# Canay
n1 = 45; n2 = 41
p1 = 0.01; p2 = 0.14
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))
pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = 0.05)
# power = 0.74

n1 = 41; n2 = 10
p1 = 0.14; p2 = 0.71
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))
pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = 0.05)
# power = 0.94

# Bocaccio versus NULL of 0.50
power.prop.test(
			p1 = 0.14, 
			p2 = 0.50, 
			power = 0.80,
			strict = TRUE)
#  n = 26

# Canary versus NULL of 0.50
power.prop.test(
			p1 = 0.71, 
			p2 = 0.50, 
			power = 0.80,
			strict = TRUE)
# n = 84

n = NULL
p = seq(0.01, 0.99, 0.02)
for (prop in p){

	out = power.prop.test(
		p1 = 0.5,
		p2 = prop,
		power = 0.80, 
		strict = TRUE)

	n = c(n, ceiling(out$n))
}
plot(p, n, ylim = c(0,100), type = 'b')
abline(v = 0.50, lty = 2, col = 'red')
abline(h = 89, lty = 3, col = 'green')

n  <- 35
p1 <- 0.50; q1 <- 1 - p1
p2 <- 0.15; q2 <- 1 - p2
p  <- (p1 + p2) / 2; q <- 1 - p
sd <- sqrt(p1*q1/n + p2*q2/n)

stats::power.prop.test(
n = n,
p1 = p1,
p2 = p2,
sig.level = 0.05,
power = NULL,
alternative = "two.sided", 
strict = TRUE)

p1 <- 0.5
p2 <- 0.15
h <- 2 *asin(sqrt(p2)) - 2 * asin(sqrt(p1))

pwr.2p.test(
	h = h,
	sig = 0.05,
	power = 0.80,
	alternative = "two.sided"
)


#sum(O-E)^2/E
chi2 <- (p2 - p1)^2/p1
w <- sqrt(chi2/(34*2))
pwr.chisq.test(w = w, df = 2, sig.level = 0.05, power = 0.80)

pwr.2p2n.test(
	h = NULL, # effect size
	n1 = NULL, 
	n2 = NULL, 
	sig.level = 0.05, 
	power = NULL,
    alternative = c("two.sided", "less","greater"))

pwr.2p2n.test(
	#h = h,
	n1 = 10,
	n2 = 34,
	sig.level = 0.05,
	power = 0.80,
	alternative = "two.sided")

pwr.t2n.test(d=0.6,n1=10,n2=34,alternative="two.sided")
