# lab-05_lgg.R

# task 1
# grid of points
x <- seq(0, 1, 10^-2)
fx <- function(x) sin(pi * x)^2
plot(fx, xlim = c(0,1), ylim = c(0,1.5), ylab = "f(x)", lwd = 2)
curve(dunif, add = TRUE, col = "blue", lwd = 2)
curve(dbeta(x,2,2), add = TRUE, col = "red", lwd = 2)
legend("bottom", legend = c(expression(paste("sin(",pi,"x)"^"2")),"Unif(0,1)",
                            "Beta(2,2)"), col = c("black", "blue", "red"), lty = c(1,1,1), bty = "n", cex = 1.1, lwd = 2)
sim_fun <- function(f, envelope = "unif", par1 = 0, par2 = 1, n = 10^2, plot = TRUE){
  
  r_envelope <- match.fun(paste0("r", envelope))
  d_envelope <- match.fun(paste0("d", envelope))
  proposal <- r_envelope(n, par1, par2)
  density_ratio <- f(proposal) / d_envelope(proposal, par1, par2)
  samples <- proposal[runif(n) < density_ratio]
  acceptance_ratio <- length(samples) / n
  if (plot) {
    hist(samples, probability = TRUE, 
         main = paste0("Histogram of ", 
                       n, " samples from ", 
                       envelope, "(", par1, ",", par2,
                       ").\n Acceptance ratio: ",
                       round(acceptance_ratio,2)), 
         cex.main = 0.75)
  }
  list(x = samples, acceptance_ratio = acceptance_ratio)
}
par(mfrow = c(2,2), mar = rep(4, 4))
unif_1 <- sim_fun(fx, envelope = "unif", par1 = 0, par2 = 1, n = 10^2) 
unif_2 <- sim_fun(fx, envelope = "unif", par1 = 0, par2 = 1, n = 10^5)
beta <- sim_fun(fx, envelope = "beta", par1 = 2, par2 = 2, n = 10^5)

par(mfrow = c(1,1))

#### The beta acceptance ratio will be better bc it is more similar in shape
#### versus the uniform distribution versus the PDF. To determine this
#### mathematcially, we can calculate the acceptance ratio?