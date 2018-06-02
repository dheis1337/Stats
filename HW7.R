library(ggplot2)
# 2.14 

# a 

x <- rnorm(15, mean = 0, sd = 1)

qqnorm(x)
qqline(x)


hist(x)

# b
x <- rnorm(100, mean = 0, sd = 1)

qqnorm(x)
qqline(x)


hist(x)

# 2.17 
flights <- read.csv("C:/MyStuff/School/Math/Stats/Data/FlightDelays.csv")

ua.delays <- flights[flights$Carrier == "UA", ]$Delay
aa.delays <- flights[flights$Carrier == "AA", ]$Delay


boxplot(ua.delays)
boxplot(aa.delays)
