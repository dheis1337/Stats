setwd("C:/MyStuff/School/Math/Stats/Data")
# 1) Suppose you conduct an experiment and inject a drug into three mice. Their 
# times for running a maze are 8, 10, and 15 seconds; the times for the two 
# control mice are 5 and 9 seconds. 

# a) Compute the difference in mean times between the treatment and control group
treat <- c(8, 10, 15)
contr <- c(5, 9)

mean(treat) - mean(contr)

# c) What proportion of the differences are as large or larger than the 
# observed difference in mean times?

result <- vector("numeric", 0)
observed <- mean(treat) - mean(contr)
pool <- c(8, 10, 15, 5, 9)
n <- 10^4 - 1

for (i in 1:n) {
  index <- sample(5, size = 3, replace = FALSE)
  result[i] <- mean(pool[index]) - mean(pool[-index])
}

(sum(result >= observed) + 1) / (n + 1)

# d) For each permutation, calculate the mean of the treatment group only. What
# proportion of these means are as large or larger than the observed mean of the 
# treatment group?

treat.mean <- vector("numeric", 0)
observed <- mean(treat)

for (i in 1:n) {
  index <- sample(5, size = 3, replace = FALSE)
  treat.mean[i] <- mean(pool[index])
}

(sum(treat.mean >= observed) + 1) / (n + 1)


# 3) In the Flight Delays Case Study in Section 1.1
# b) The data contains flight delays for two airlines, American Airlines and United
# Airlines. Conduct a two-side permutation test to see if the mean delay times between
# the two carriers are statistically significant. 

flights <- read.csv("FlightDelays.csv")
ua.delays <- flights[flights$Carrier == "UA", ]$Delay
aa.delays <- flights[flights$Carrier == "AA", ]$Delay
observed <- mean(ua.delays) - mean(aa.delays)
pool <- c(ua.delays, aa.delays)
n <- 10^4 - 1

result <- vector("numeric", length = 0)
for (i in 1:n) {
  index <- sample(4029, size = 1123, replace = FALSE)
  result[i] <- mean(pool[index]) - mean(pool[-index])
} 

(sum(result >= observed) + 1) / (n + 1) * 2

# 4) In the Flight Delays Case Study in Section 1.1, the data contain flight delays
# two airlines, American Airlines and United Airlines. 
# a) Compute the proportion of times that each carrier's flights was delayed more
# than 20 min. Conduct a two-sided test to see if the difference in these proportions
# is statistically significant. 
more.than.20.ua <- ua.delays[ua.delays > 20]
more.than.20.aa <- aa.delays[aa.delays > 20]
pool <- c(ua.delays[ua.delays > 20], aa.delays[aa.delays > 20])
more.than.20.ua <- length(more.than.20.ua) / length(ua.delays)
more.than.20.aa <- length(more.than.20.aa) / length(aa.delays)

observed <- more.than.20.ua - more.than.20.aa

result <- vector("numeric", length = 0)
for (i in 1:n) {
  index <- sample(532, size = 239)
  result[i] <- length(pool[index]) / length(pool) - length(pool[-index]) / length(pool)
}

(sum(result >= observed) + 1) / (n + 1)

# 9) The file Phillies2009 contains data from the 2009 season for the baseball team 
# of the Philadelphia Philes. Import these data into R

# a) Compare the empirical distribution functions of the number of strikeouts per
# game (StirkeOuts) for games played at home and games played away (Location)
philles <- read.csv("Phillies2009.csv")
home.strikes <- philles[philles$Location == "Home", ]$StrikeOuts 
away.strikes <- philles[philles$Location == "Away", ]$StrikeOuts 

plot.ecdf(home.strikes)
plot.ecdf(away.strikes)


# 10) Researchers at the University of Nebraska conducted a study to investigate 
# sex differences in dieting trends among a group of midwestern college students. 
# Students were recruited from an introductory nutrition course during one term. 
# Below are data from on question asked to 286 participants.

diet.data <- data.frame(Gender = c("Women", "Men"), Yes = c(35, 8), No = c(146, 97),
                        stringsAsFactors = TRUE)
diet.data <- as.matrix(diet.data)
diet.data <- diet.data[, -1]

# a) Carry out a test to determine the relationship between gender and diet. 
