setwd("C:/MyStuff/School/Math/Stats/Data")
# 6.8 
# Consider a population that has a gamma distribution with parameters r = 5 
# and lambda = 1/4
# a) Use simulation to generate an approximate sampling distribution of the mean;
# plot and describe the distribution
n <- 10^4
sim.means <- vector("numeric", length = n)
for (i in 1:n) {
  gams <- rgamma(200, shape = 5, rate = 1/4)
  sim.means[i] <- mean(gams)
}

hist(sim.means)
plot.ecdf(sim.means)

# b) Now, draw one random sample of size 200 from this population. Create a histogram
# of your sample and find the mean and standard deviation. 
rsamp <- rgamma(n = 200, shape = 5, rate = 1/4)
mean(rsamp)
sd(rsamp)
hist(rsamp)

# c) 
n <- 10^4
gam.boot.mean <- vector("numeric", length = n)

for (i in 1:n) {
  gam <- sample(rsamp, 200, replace = TRUE)
  gam.boot.mean[i] <- mean(gam)
}


hist(gam.boot.mean)
mean(gam.boot.mean)

# e) 
# sampling distribution of xbar 
fifty.mean <- vector("numeric", length = n)
for (i in 1:n) {
  gams <- rgamma(50, shape = 5, rate = 1/4)
  fifty.mean[i] <- mean(gams)
}

# sample of size 50
fifty.samp <- rgamma(50, shape = 5, rate = 1/4)
mean(fifty.samp)
sd(fifty.samp)

# bootstrap size of 50
fifty.boot <- vector("numeric", length = n)
for (i in 1:n) {
  boot <- sample(fifty.samp, size = 50, replace = TRUE)
  fifty.boot[i] <- mean(boot)
}


# sampling distribution of xbar 
ten.mean <- vector("numeric", length = n)
for (i in 1:n) {
  gams <- rgamma(10, shape = 5, rate = 1/4)
  ten.mean[i] <- mean(gams)
}
mean(ten.mean)
sd(ten.mean)

# sample of size 10
ten.samp <- rgamma(10, shape = 5, rate = 1/4)
mean(ten.samp)
sd(ten.samp)

# bootstrap size of 10
ten.boot <- vector("numeric", length = n)
for (i in 1:n) {
  boot <- sample(ten.samp, size = 10, replace = TRUE)
  ten.boot[i] <- mean(boot)
}
mean(ten.boot)
sd(ten.boot)

# 5.17
books <- read.csv("BookPrices.csv")

# a) Perform some exploratory analysis on book prices for each of the two 
# disciplinary areas
names(books)
head(books)
levels(books$Area)

ms <- books[books$Area == "Math & Science", ]$Price
ss <- books[books$Area == "Social Sciences", ]$Price

hist(ms)
hist(ss)

# b) Bootstrap the mean of the book price for each area spearately and describe
# the distribution
ms.boot <- vector("numeric", length = n)
for (i in 1:n) {
  boot <- sample(ms, size = length(ms), replace = TRUE)
  ms.boot[i] <- mean(boot)
}

hist(ms.boot)

ss.boot <- vector("numeric", length = n)
for (i in 1:n) {
  boot <- sample(ss, size = length(ms), replace = TRUE)
  ss.boot[i] <- mean(boot)
}

hist(ss.boot)

# c) Bootstrap the ratio of the means. Provide plots of the bootstrap distriubtion
# and comment
ratio.boot <- vector("numeric", length = n)
for (i in 1:n) {
  ms.boot <- sample(ms, size = length(ms), replace = TRUE)
  ss.boot <- sample(ss, size = length(ss), replace = TRUE)
  ratio.boot[i] <- mean(ms.boot) / mean(ss.boot)
}

hist(ratio.boot)

# d) Find the 95% confidence intervl for the ratio of means. Interpret this interval
quantile(ratio.boot, c(.025, .975))
mean(ratio.boot)

# e) What is the bootstrap estimate of the bias? What fraction of the bootstrap
# standard error does it represent
bias <- mean(ratio.boot) - (mean(ms) / mean(ss))

bias / sd(ratio.boot)
