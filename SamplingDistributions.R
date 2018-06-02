# 4.12
#A friend claims that she has drawn a random sample of size 30 from the 
# exponential distribution with lambda = 1/10. The mean of her samp;e is 12. 

# a) 
# What is the expected value of a sample mean?
# The expected value of the sample mean should be the mean of the population,
# which for an exponential distribution is 1 / lambda. 

expect.mean <- 1 / (1/10)

# b) 
# Run a simulation of darwwing 1000 random samples, each of size 30, from the
# above population and then compare the mean. What proportion of the sample means 
# are as large or larger than 12?

n <- 1000
sim.means <- vector("numeric", length = n)

for (i in 1:n) {
  samp <- rexp(n = 30, rate = 1/10)
  sim.means[i] <- mean(samp)
}

mean(sim.means) # Approximately our expected value of the sample mean - 10

sum(sim.means >= 12)) / n # proportion of simulated means greater than 12

# c)
# Is a mean of 12 unusual for sample of size 30 from Exp(1/10)?
# No, it is not that unusal, with it happening approximately 13% of the time. 

# 17
# Let X1, X2, ... , X20 be i.i.d. Expon(2). Let X be the sum of these. 

# a)
# Simulate the sampling distribution of X in R. 
n <- 1000
sim.samp <- vector("numeric", length = n)

for (i in 1:n) {
  x <- rexp(20, rate = 2)
  sim.samp[i] <- sum(x)
}
 
# b)
# From your simulation find E(X) and Var(X)
mean(sim.samp)
var(sim.samp)


# c)
# From your simulation, find P(X <= 10)
sum(sim.samp <= 10) / n

hist(sim.samp)


