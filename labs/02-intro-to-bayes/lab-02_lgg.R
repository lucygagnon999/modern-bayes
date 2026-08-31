# lab-02_lgg

# Task 2

set.seed(123)

obs_data<-rbinom(n=100, size=1, prob=0.01)

head(obs_data)
tail(obs_data)
length(obs_data)


# Task 3

### Bernoulli LH Function ###
# Input: obs_data, theta
# Output: bernoulli likelihood
bernoulli_lh <- function(obs_data, theta) {
  n<-length(obs_data) # define num samples
  x<-sum(obs_data) # define sample data
  return(theta^x*(1-theta)^(n-x)) 
}


### Plot LH for a grid of theta values ###
# Create the grid #
thetas = seq(0, 1, length.out = 1000)
# Store the LH values
lh_values<- bernoulli_lh(obs_data, thetas)
# Create the Plot
plot(thetas, lh_values, type = "l", main= "Likelihood Profile", 
     xlab="Simulated support",ylab = "Likelihood")

# Task 4

update_params <- function(obs_data, theta, a, b) {
  n<-length(obs_data) # define num samples
  x<-sum(obs_data) # define sample data

  results <- list(a_param = x+a, b_param = n-x+b)
  return(results)
}

a1<-1
b1<-1
a2<-3
b2<-1

output1 <- update_params(obs_data, thetas, a1, b1)
a1_new<- output1$a_param
b1_new<- output1$b_param

output2 <- update_params(obs_data, thetas, a2, b2)
a2_new<- output2$a_param
b2_new<- output2$b_param

# Task 5

prior1 <- dbeta(thetas, shape1 = a1, shape2 = b1)
posterior1 <- dbeta(thetas, shape1 = a1_new, shape2 = b1_new)

all_y <- range(c(lh_values, prior1, posterior1), na.rm = TRUE)

# First plot: likelihood
plot(thetas, lh_values,type = "l", col = "blue", pch = 16,
     xlab = "Simulated support",ylab = "Likelihood",
     main = "Non-informative prior", yaxt = "n",
     xlim = range(thetas), yaxt="none")

# Overlay prior
par(new = TRUE)

plot(thetas, prior1,type = "l", col = "red", pch = 17, lty = 2,
     axes = FALSE, xlab = "", ylab = "",xlim = range(thetas),
    yaxt="none")

# Overlay posterior
par(new = TRUE)

plot(thetas, posterior1,type = "l", col = "green", pch = 17, lty = 3,
     axes = FALSE, xlab = "", ylab = "",xlim = range(thetas),yaxt="none")

legend("topright",
       legend = c("Likelihood", "Prior", "Posterior"),
       col = c("blue", "red", "green"),
       lty = c(1, 2, 3),
       pch = c(16, 17, 17))
# informative prior
prior2 <- dbeta(thetas, shape1 = a2, shape2 = b2)
posterior2 <- dbeta(thetas, shape1 = a2_new, shape2 = b2_new)

all_y <- range(c(lh_values, prior2, posterior2), na.rm = TRUE)

# First plot: likelihood
plot(thetas, lh_values,type = "l", col = "blue", pch = 16,
     xlab = "Simulated support",ylab = "Likelihood",main = "Informative prior",
     yaxt = "n",xlim = range(thetas),yaxt="none")

# Overlay prior
par(new = TRUE)

plot(thetas, prior2, type = "l", col = "red", pch = 17, lty = 2,
     axes = FALSE, xlab = "", ylab = "", xlim = range(thetas), yaxt="none")
# Overlay posterior
par(new = TRUE)

plot(thetas, posterior2, type = "l", col = "green", pch = 17, lty = 3,
     axes = FALSE, xlab = "", ylab = "", xlim = range(thetas),yaxt="none")

legend("topright",legend = c("Likelihood", "Prior", "Posterior"),
       col = c("blue", "red", "green"),lty = c(1, 2, 3),pch = c(16, 17, 17))
#### Here, we see the posterior  overlap the likelihood in the case of the 
#### non-informative prior. When we implement an informative prior, the 
#### posterior moves away from the lieklihood and approach the prior.
