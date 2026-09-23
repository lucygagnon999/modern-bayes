## don't use data in package, use data on lab assignment #
library(plyr)
library(ggplot2)
library(dplyr)
library(xtable)
library(reshape)
set.seed(123)
# input data
# spurters
x = c(18, 40, 15, 17, 20, 44, 38)
# control group
y = c(-4, 0, -19, 24, 19, 10, 5, 10,
      29, 13, -9, -8, 20, -1, 12, 21,
      -7, 14, 13, 20, 11, 16, 15, 27,
      23, 36, -33, 34, 13, 11, -19, 21,
      6, 25, 30,22, -28, 15, 26, -1, -2,
      43, 23, 22, 25, 16, 10, 29)
# store data in data frame 
iqData = data.frame(Treatment = c(rep("Spurters", length(x)), 
                                  rep("Controls", length(y))),
                    Gain = c(x, y))
xLimits = seq(min(iqData$Gain) - (min(iqData$Gain) %% 5),
              max(iqData$Gain) + (max(iqData$Gain) %% 5),
              by = 5)

ggplot(data = iqData, aes(x = Gain, fill = Treatment, colour = I("black"))) + 
  geom_histogram(position = "dodge", alpha = 0.5, breaks = xLimits, closed = "left")+
  scale_x_continuous(breaks = xLimits, 
                     expand = c(0,0))+ 
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, 10, by = 1))+
  ggtitle("Histogram of Change in IQ Scores") + labs(x = "Change in IQ Score", 
                                                     fill = "Group") + 
  theme(plot.title = element_text(hjust = 0.5))  


#### The change in IQ score for the control group spans -35 to 45, while the change 
#### in IQ score spans 17.5 - 45 for spurters. So, we see positive, 0, and negative
#### changes in IQ score in the control group, while we only observe increases
#### in IQ score for the spurters group.


prior = data.frame(m = 0, c = 1, a = 0.5, b = 50)
findParam = function(prior, data){
  postParam = NULL
  c = prior$c
  m = prior$m
  a = prior$a
  b = prior$b
  n = length(data)
  postParam = data.frame(m = (c*m + n*mean(data))/(c + n), 
                         c = c + n, 
                         a = a + n/2, 
                         b =  b + 0.5*(sum((data - mean(data))^2)) + 
                           (n*c *(mean(data)- m)^2)/(2*(c+n)))
  return(postParam)
}
postS = findParam(prior, x)
postC = findParam(prior, y)


xtable(rbind(prior = prior,
             `Spurters Posterior` = postS,
             `Controls Posterior` = postC), 
       caption = "Parameters")
# sampling from two posteriors 

# Number of posterior simulations
sim = 1000

# initialize vectors to store samples
mus = NULL
lambdas = NULL
muc = NULL
lambdac = NULL

# Following formula from the NormalGamma with 
# the update paramaters accounted accounted for below 

lambdas = rgamma(sim, shape = postS$a, rate = postS$b)
lambdac = rgamma(sim, shape = postC$a, rate = postC$b)


mus = sapply(sqrt(1/(postS$c*lambdas)),rnorm, n = 1, mean = postS$m)
muc = sapply(sqrt(1/(postC$c*lambdac)),rnorm, n = 1, mean = postC$m)

# Store simulations
simDF = data.frame(lambda = c(lambdas, lambdac),
                   mu = c(mus, muc),
                   Treatment = rep(c("Spurters", "Controls"),
                                   each = sim))

simDF$lambda = simDF$lambda^{-0.5}

# Plot the simulations
ggplot(data = simDF, aes(x = mu, y = lambda, colour = Treatment, shape = Treatment)) +
  geom_point(alpha = 0.2) + 
  labs(x = expression(paste(mu, " (Mean Change in IQ Score)")),
       y = expression(paste(lambda^{-1/2}, " (Std. Dev. of Change)")))  + 
  ggtitle("Posterior Samples")+ 
  theme(plot.title = element_text(hjust = 0.5))


# task 4

#mean(spurters.postParams.s['mu',] > controls.postParams.s['mu',])
mean(mus>muc) # check if this is right

#### The posterior probability that the mean change in IQ score is greater in 
#### the spurters group versus the control group is 0.976, meaning that there is
#### a 97.6% chance that the mean change in IQ score is greater in 
#### the spurters group versus the control group
#####CHECK THIS!

# task 5

# Number of posterior simulations
sim = 1000

# initialize vectors to store samples
mus_prior = NULL
lambdas_prior = NULL

# Following formula from the NormalGamma with 
# the update paramaters accounted accounted for below 

lambdas_prior = rgamma(sim, shape = prior$a, rate = prior$b)


mus_prior = sapply(sqrt(1/(prior$c*lambdas_prior)),rnorm, n = 1, mean = prior$m)

# Store simulations
simDF_prior = data.frame(lambda = c(lambdas_prior),
                   mu = c(mus_prior),
                   Treatment = rep(c("Spurters", "Controls"),
                                   each = sim))

simDF_prior$lambda = simDF_prior$lambda^{-0.5}


ggplot(data = simDF_prior, aes(x = mu, y = lambda)) +
  geom_point(color = "darkgreen", alpha = 1, size = 1) + 
  labs(x = expression(paste(mu, " (Mean Change in IQ Score)")),
       y = expression(paste(lambda^{-1/2}, " (Std. Dev. of Change)")))  + 
  ggtitle("Prior Samples")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-50, 50) +
  ylim(0, 40)
#### Based on the samples drawn form the scatterplot, we observe that the choice
#### of prior conforms with our prior beliefs. We expect there to be no change
#### in IQ score (i.e., mean is 0) based on oru prior beliefs because we do not 
#### have any expert information suggesting a change in IQ score. Therefore, we
#### expect most points ot be clustered near $\mu = 0$, and we expect greater 
#### standard devistions of change with greater mean changes in IQ score since
#### we are likely to observe more variability with more pronounced mean IQ 
#### score changes.

