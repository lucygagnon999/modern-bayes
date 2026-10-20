# lab-06_lgg.R
set.seed(1)

n <- 1e6   # number of random samples

# sample from a proposal distribution, e.g.,  Normal
x <- rnorm(n, mean = 0, sd = 1)

# compute the importance weights: target_f(x) / proposal_g(x)
weights <- exp(-x^4) / dnorm(x, mean = 0, sd = 1)

# Monte Carlo estimate is the average of the weights
integral_infinite <- mean(weights)

# print result
print(integral_infinite)

# compare with exact value
gamma(1/4)/2

## importance sampling

# number samples
N <- 100000

# target function
f <- function(x) {
  return(exp(-x^4))
}

# proposal distribution g
g_density <- function(x) {
  return(dexp(x, rate = 1))
}

# generate samples from the proposal distribution g(x)
x_samples <- rexp(N, rate = 1)

# Calculate the importance weights: f(x) / g(x)
weights <- f(x_samples) / g_density(x_samples)

# estimate the integral (the sample mean of the weights)
estimated_integral <- mean(weights)

estimated_integral



library(xtable)
# define target functions for MC estimate
mc1 <- function(x){
  # folded normal target
  return(sqrt(2*pi/abs(x)))
}

mc2 <- function(x){
  # normal target
  exp(-x^4)/dnorm(x)
}

mc3 <- function(x){
  # uniform target
  return(20*exp(-x^4))
}
mcSE <- function(values){
  # this function calculates the standard error
  # of an MC estimate with target function values
  # eg values should be vector of f(x_i)
  return(sqrt(var(values)/length(values)))
}
# folded normal
draws <- rnorm(10000)
values <- sapply(draws,mc1)
# normal
draws2 <- rnorm(10000)
values2 <- sapply(draws2,mc2)
# uniform
draws3 <- runif(10000,min=-10,max=10)
values3 <- sapply(draws3,mc3)
# plot histograms
pdf(file="mc1hist.pdf")
hist(values*2^(-5/4), xlab = expression(f(X)),
     main = "Folded Normal")
dev.off()
pdf(file="mc2hist.pdf")
hist(values2, xlab = expression(f(X)),
     main = "Normal")
dev.off()
pdf(file="mc3hist.pdf")
hist(values3, xlab = expression(f(X)),
     main = "Uniform")
dev.off()
# put results in a data frame and display with xtable
mc.mean <- mean(values)*2^(-5/4)
mc.sd<- 2^(-10/4)*sd(values)/sqrt(length(values))
means <- c(gamma(1/4)/2,mc.mean,mean(values2),mean(values3))
SEs <- c(NA, mcSE(values),mcSE(values2),mcSE(values3))
mc.df <- data.frame(Mean = means, SE = SEs, 
                    row.names = c("True value", "row.names
                                  Normal", "Full Normal",  
                                  "Truncated Uniform"))
xtable(mc.df)
