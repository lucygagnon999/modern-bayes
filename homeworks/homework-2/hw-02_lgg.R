# hw-02_lgg.R
# Question 2

## Part C
# define gamma distribution (prior)
prior <- dgamma(x = thetas, shape = 0.1, rate = 1.0)
# define data
data <- c(20.9, 69.7, 3.6, 21.8, 21.4, 0.4, 6.7, 10.0)

# define the posterior dist
posterior <- dgamma(x = thetas, shape = 0.1+1, rate = 1.0 + sum(data))

# plot prior
plot(thetas, prior,type = "l", col = "blue",
     xlab = "Simulated support",ylab = "Likelihood",
     main = "Non-informative prior", yaxt = "n",
     xlim = range(thetas), yaxt="none")

# overlay posterior
par(new = TRUE)
# plot posterior
plot(thetas, posterior,type = "l", col = "red", lty = 3,
     axes = FALSE, xlab = "", ylab = "",xlim = range(thetas),yaxt="none")
# add legend
legend("topright",legend = c("Prior", "Posterior"),
       col = c("blue", "red"), lty = c(1, 2))

