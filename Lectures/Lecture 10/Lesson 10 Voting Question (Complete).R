#--------------------------------------------
# RDD Coding Exercise 
#
# Question: Do voters elect or effect policies?  
#
#--------------------------------------------

setwd("C:/Users/ardin/OneDrive/Desktop/Econ 258 Winter 2020/Data/5_voting_with_RDD")

#install.packages("rdd")
library(haven)
library(rdd)

# Import the voting data from Canvas. 

voting_data  <- read_dta("voting_data_for_RDD.dta")

# Question 1, 2, and 3 below replicates the results table in the slide. 

#1) Regress score on lag democrat for when lagdemvoteshare is between 0.48 to 0.52, 
#   should be able to do this in one line after importing data. 

lm(score ~ lagdemocrat, data = voting_data, subset=(lagdemvoteshare>0.48 & lagdemvoteshare<0.52))

#2) Regress score on democrat for when lagdemvoteshare is between 0.48 to 0.52 

lm(score ~ democrat, data = voting_data, subset=(lagdemvoteshare>0.48 & lagdemvoteshare<0.52))

#3) Regress democrat on lagdemocrat, again when lagdemvoteshare is between 0.48 to 0.52.

lm(democrat ~ lagdemocrat, data = voting_data, subset=(lagdemvoteshare>0.48 & lagdemvoteshare<0.52))

#4) How would 1) change when using the entire sample instead of with restrictions? 

lm(score ~ lagdemocrat, data = voting_data)
# The difference in ADA score is much larger, but this is due to the fact that 
# we do have very different populations for places where a democrat won versus a republican. 

#5) Based on your results in part 1,2, and 3, calculate the "affect" component. 
#   So do voters elect or affect policy based on your results?
  
# 21.28 - 47.71*0.4843 = -1.825 

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