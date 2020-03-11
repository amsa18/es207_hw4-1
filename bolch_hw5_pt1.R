# HW5 Part 1
# Erik Bolch
# 2/27/2020
# https://github.com/ebolch/es207_hw4

# Erik, you did a great job in the most efficient way. 
# And I could not find any code or script that I could improve it. 
# Thank you for your modification on my code, I learned a lot  
# Required Packages
require(tidyverse)
require(ggplot2)

# Helsel and Hirsch #3.1 & 3.4

## 3.1 Compute both nonparametric and parametric 95% interval estimates for the median of
## the granodiorite data of exercise 2.3. Which is more appropriate for these data? Why?

#Chloride Concentrations (mg/L)
my.data <- tibble(
  Gran = c(6.0, 0.4, 0.4, 0.7, 0.8, 6.0, 5.0, 0.6, 1.2, 0.3, 0.2, 0.5, 0.5, 10, 0.2, 0.2, 1.7, 3.0),
  Qtz_M = c(1.0, 0.2, 1.2, 1.0, 0.3, 0.1, 0.1, 0.4, 3.2, 0.3, 0.4, 1.8, 0.9, 0.1, 0.2, 0.3, 0.5, NA))
hist(my.data$Gran)
qqnorm(my.data$Gran)
qqline(my.data$Gran)

#Non-Parametric Prediction Interval
mednppi <- function(x,y){# requires data vector and alpha
  x <- sort(x)
  print(x)
  med <- median(x)
  print(med)
  alpha <- y
  xprime <- qbinom(alpha/2, size = length(x), prob = 0.5)# 0.5 is for median of small sample sizes
  Rl <- xprime + 1
  Ru <- length(x) - xprime
  print(paste("True median falls within", x[Rl], "to", x[Ru], "at", 100*(1-alpha), "% Confidence Interval"))
}

mednppi(my.data$Gran, 0.05)

# Parametric Prediction

medppi <- function(x,a,t){ #t values required. values calculated from t.test(function of mean?) were different book example
  alpha <- a
  n <- length(x)
  y <- log(x)
  ybar <- mean(y)
  GM<- exp(ybar) #Geometric Mean of Log transform is approximation of median if they are symmetric
  s <- sd(y)
  Rl <- exp(ybar-t*sqrt(s^2/n))
  Ru <- exp(ybar+t*sqrt(s^2/n))
  print(paste("True median falls within", Rl, "to", Ru, "at", 100*alpha, "% Confidence Interval"))
}
t<- 2.110 #for .05 two tail, df = 17
medppi(my.data$Gran, 0.95, t)

hist(log(my.data$Gran))
qqnorm(log(my.data$Gran))
qqline(log(my.data$Gran))

# A non-parametric interval estimate is probably more appropriate for this 
#data due to its skewed distribution. However the distribution of the log 
#appears to be a normal distribution, so it's also possible to use the 
#parametric interval estimate.

##########################################################################################

# 3.4 Construct the most appropriate 95 percent interval estimates for the mean and median
# annual streamflows for the Conecuh River at Brantley, Alabama (data in Appendix C2).

C2 <- read_csv("./data/Conecuh_River_apxc2.csv")
head(C2)
hist(C2$`Flow (cfs)`)
qqnorm(C2$`Flow (cfs)`)
qqline(C2$`Flow (cfs)`)

# Median

mednppi(C2$`Flow (cfs)`, 0.05) #function defined above

# Mean

t.test(C2$`Flow (cfs)`,conf.level = 0.95)

# Because the data are non-parametric, a non-parametric interval estimate using rank
#sum is appropriate for determining the median of the data. As for the mean, the 
#data are somewhat symmetric, meaning that the t-test function can be used to 
#generate an interval from the data without a transformation.
