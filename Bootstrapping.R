# 2 
# Consier the sample 1, 3, 4, 6 from some distribution

# a)
# For one random bootstrapp sample, find the probability the mean is 1. 
n <- 1000
boot.mean <- vector("numeric", length = n)
dat <- c(1, 3, 4, 6)

for (i in 1:n) {
  x <- sample(dat, size = 4, replace = TRUE)
  boot.mean[i] <- mean(x)
}

sum(boot.mean == 1) / n


# 7
# Consider a population that has a normal distribution with mean 36 and standard 
# deviation of 8. 

# a)
# The sampling distribution of Xbar for samples of size 200 will have what distribution,
# mean, and standard error?

# The distribution will be approximately normal, through the CLT. The standard error
# is the standard deviation divided by the square root of 200, and the mean will be
# the population mean, 36.
8 / sqrt(200)

# b)
# Use R to draw a random sample of size 200 from this population. Conduct EDA on
# your sample. 

samp <- rnorm(200, mean = 36, sd = 8)

hist(samp)
mean(samp)
sd(samp)

# c) Compute the bootstrap distribution for your sample and note the bootstrap mean
# and standard eroor. 

n <- 10000
boot.mean <- vector("numeric", length = n)

for (i in 1:n) {
  x <- sample(samp, size = 200, replace = TRUE)
  boot.mean[i] <- mean(x)
}

mean(boot.mean) # centered around the original sample
sd(boot.mean) # approximately the standard error of the sampling distribution




