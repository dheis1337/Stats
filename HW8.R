# Homework 8
setwd("C:/MyStuff/School/Math/Stats/Data")

# 3.1
# (a)
g1 <- c(8, 10, 15)
g2 <- c(5, 9)
g3 <- c(8, 10, 15, 5, 9)

mean(g1) - mean(g2)

# (c)
observed <- mean(g1) - mean(g2)
n <- 10^4 - 1

result <- numeric(n)
for (i in 1:n) {
  index <- sample(5, size = 3, replace = FALSE)
  result[i] <- mean(g3[index]) - mean(g3[-index])
} 

(sum(result >= observed) + 1) / (n +1)


# (d)
observed <- mean(g1)
result <- numeric(n)
for (i in 1:n) {
  index <- sample(5, size = 2, replace = FALSE)
  result[i] <- mean(g3[index])
}

(sum(result >= observed) + 1)/ (n + 1)

# 3.3 (b)
flights <- read.csv("FlightDelays.csv")
may.delays <- flights[flights$Month == "May",]$Delay
june.delays <- flights[flights$Month == "June",]$Delay
delays <- c(may.delays, june.delays)

# Observed
observed <- mean(may.delays) - mean(june.delays)

for (i in 1:n) {
  index <- sample(4029, size = 1999, replace = FALSE)
  result[i] <- mean(delays[index]) - mean(delays[-index])
}

(sum(result <= observed) + 1) / (n + 1) * 2


# 3.4 (b)
ua.delays <- flights[flights$Carrier == "UA", ]$Delay
aa.delays <- flights[flights$Carrier == "AA", ]$Delay
delays <- c(aa.delays, ua.delays)

# Observed 
observed <- var(ua.delays) - var(aa.delays)

for (i in 1:n) {
  index <- sample(4029, size = 1123, replace = FALSE)
  result[i] <- var(delays[index]) - var(delays[-index])
}
  
(sum(result >= observed) + 1) / (n + 1) 


# 3.13 
# (b)
fish <- rbind(c(14, 30, 42, 78, 33, 14), c(11, 28, 53, 66, 27, 9), c(10, 17, 61, 53, 22, 10))
chisq.test(fish)

# 3.16
gss <- read.csv("GSS2002.csv")

# (b)
pres <- table(gss$Gender, gss$Pres00)
pres <-as.matrix(pres)
pres <- rbind(c(459, 5, 492, 26, 3), c(426, 5, 289, 31, 13))
chisq.test(pres)

# (c)

chisq <- function(obs) {
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  sum((obs - expected)^2 / expected)
}

pres <- gss$Pres00
gender <- gss$Gender
observed <- chisq(table(pres, gender))

index <- which(is.na(pres) | is.na(gender))
pres2 <- pres[-index]
gender2 <- gender[-index]


n <- 10^4 - 1
result <- numeric(n)
for (i in 1:n) {
  dp.permuted <- sample(gender2)
  gss.table <- table(pres2, dp.permuted)
  result[i] <- chisq(gss.table)
}

(sum(result >= observed) + 1) / (n + 1)

a <- matrix(nrow = 3, ncol = 3, c(15, -1, 18,
                             -16, 0, 18,
                             11, -1, -14), byrow = TRUE)

determinant(a, logarithm = FALSE)
