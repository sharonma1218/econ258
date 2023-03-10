#--------------------------------------------
# RDD Coding Exercise 
#
# Question: Do voters elect or effect policies?  
#
#--------------------------------------------

setwd("~/Downloads/Econ 258 R/Lesson 10 Regression Discontinuity Design")

#install.packages("rdd")

library(haven)
library(rdd)

# Import the voting data from Canvas. 

vote_data <- read_dta("voting_data_for_RDD.dta", encoding = "UTF-8")

# Question 1, 2, and 3 below replicates the results table in the slide. 

#1) Regress score on lag democrat for when lagdemvoteshare is between 0.48 to 0.52, 
#   should be able to do this in one line after importing data, use function lm() to regress.

f1 <- lm(score~lagdemocrat, data=vote_data, subset=(lagdemvoteshare>0.48 & lagdemvoteshare<0.52))
summary(f1)

#2) Regress score on democrat for when lagdemvoteshare is between 0.48 to 0.52 

f2 <- lm(score~democrat, data=vote_data, subset=(lagdemvoteshare>0.48 & lagdemvoteshare<0.52))
summary(f2)

#3) Regress democrat on lagdemocrat, again when lagdemvoteshare is between 0.48 to 0.52.

f3 <-lm(democrat~lagdemocrat, data=vote_data, subset=(lagdemvoteshare>0.48 & lagdemvoteshare<0.52))
summary(f3)

#4) How would 1) change when using the entire sample instead of with restrictions? 


#5) Based on your results in part 1,2, and 3, calculate the "affect" component. 
#
#   So do voters elect or affect policy based on your results?


# The elect component is so strong that the affect component is negative but small.
# Therefore, the paper result suggest that voters elect politicians, and politicians 
# choose whatever policy they want. 

# Additional code for using a package to get the graph. 

RDD_est <- RDestimate(score~lagdemvoteshare, data=voting_data, 
                      subset=(voting_data$lagdemvoteshare>0.48 & voting_data$lagdemvoteshare<0.52), 
                      cutpoint = 0.5)

plot(RDD_est)
title(xlab="Democrat vote share at time t",ylab="ADA score time t+1")

# Look at that nice jump on the cutoff! 