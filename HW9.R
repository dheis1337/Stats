# Homework 9

# 4.18 - Let a X1, X2,...,Xn be a i.i.d. expon(1/3) and let Xbar be the sample mean
# a) Simulate the sampling distribution of Xbar
n <- 1000
sim.means <- vector("numeric", length = n)

for (i in 1:n) {
  sim.rv <- rexp(20, rate = 1/3)
  sim.means[i] <- mean(sim.rv)
}

hist(sim.means)


# b)
mean(sim.means)
sd(sim.means)

# c) 
length(sim.means[sim.means <= 3.5]) / length(sim.means)

# d) 


# 4.24 Let {X1,...,Xn} be i.i.d. expon(lambda). 
# b) Simulate in R the sampling distribution with lambda = 7. Compare the theoretical
# expected value of Xmin to the simulated expected value
n <- 1000
sim.min <- vector("numeric", length = n)

for (i in 1:n) {
  x <- rexp(25, rate = 7)
  sim.min[i] <- min(x)
}

hist(sim.min)
