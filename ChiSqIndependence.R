# Conducting a Chi-square test for indepdence

setwd("C:/MyStuff/DataScience/Projects/Stats") # set working directory

gss <- read.csv("GSS2002.csv") # read in data to test

names(gss) # find out the names of the variables

# Let's test for indepedence among a few things. First, let's try Religion
# and PolParty (political party) 

party <- gss$PolParty
relig <- gss$Religion

# The Chi-square test for independence works through a contingency table. We test
# for independence by observing a test statistic - the chi-square test statistic - 
# from our origingal data. The chi-square is the sum of the square difference 
# of observed and expected values divided by the expected values. Expected values
# are calculated as the Ri (row sum of row i) multiplied by Cj (column sum of column j)
# divided by the number of observations n. Let's create a function to compute the 
# chi-square test statistic
ChiSq <- function(obs) {
  # Observation is a contingency table of two factor variables
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  sum((obs - expected)^2 / expected)
}

# Create a contingecy table for party and relig
party.relig.table <- table(party, relig)

# Calculate the observed chi-square test statistic for our data
observed <- ChiSq(party.relig.table)

# Now we need to make a hypothesis and run a test to determine its truth. We will 
# work under a null stating that a person's religion and their choice of political 
# party are completely independent. In order to conduct our test, we need to permute
# the data in one of the two variables, let's choose religion (relig). We're permuting
# our data, because under the null hypothesis we're stating the two variables are 
# independent, thus we could come up with a theoretical resample of our data and 
# calculate the chi-square test statistic for this. If we do this a lot of times, we
# can calculate numerous chi-square test statistics and determine a p-value from these
# Let's begin with our for loop
n <- 10^5 - 1 # how many times we run the loop
index <- which(is.na(party) | is.na(relig))
party.clean <- party[-index] # remove NAs
relig.clean <- relig[-index] # remove NAs

result <- vector("numeric", length = n)
for (i in 1:n) {
  relig.permuted <- sample(relig.clean)
  table <- table(party.clean, relig.permuted)
  result[i] <- ChiSq(table)
}


# Now that we've calculated a chi-square test statistic for a resample of our 
# data, let's find our p-value. Wel'll do this by determining the proportion of 
# theoretical observations that are greater than or equal to our observed value.
(sum(result >= observed) + 1) / (n + 1)

