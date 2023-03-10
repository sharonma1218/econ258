#------------------------------------------------------------- #
# Lesson 9: Propensity Score Matching                          #
#                                                              #      
# Ardina Hasanbasri                                            #
#                                                              #
#--------------------------------------------------------------# 

#install.packages("MatchIt")
#install.packages("cem")
#install.packages("arm")
library(MatchIt)
library(mosaic)
library(cem) # cannot install this on mac but doesn't matter. will not get same answer but similar. 
data(lalonde)
library(arm)

##########################################
######### Part 1: Lalonde Data ###########
##########################################

# Question 1

View(lalonde) # Take a look at the data (non-experimental)
# Make sure you have 614 obs and 10 variables

# Run a regression and see if we can get the main result in the paper by lalonde. 
fit1 <- lm(re78~treat, data = lalonde)
display(fit1) # using arm

# Question 2 

# Let us check the sample distribution. 
# Treat = 1 receives training, Treat = 0 did not receive training

# comparing age
densityplot(~age, groups=factor(treat), data = lalonde, auto.key=TRUE)
favstats(~age, data = lalonde, groups = treat) # those in the non-treatment group are much older

# comparing race 
lalonde$black <- 0 
lalonde$black[lalonde$race=="black"] <- 1
histogram(~black|factor(treat), data = lalonde, type = 'percent') # 0 is not treatment, 1 is treatment
favstats(~black, data = lalonde, groups = treat) # 80% black in treatment group. "not comparing apples to apples."

##############################################################################
######## Part 2: Creating a Treatment and Control Group through Matching 
##############################################################################

# Question 3

# This command conducts the matching function. 

lalonde$hispan <- 0
lalonde$hispan[lalonde$race=="hispanic"] <- 1

m.out <- matchit(treat ~ age + educ + black + hispan 
                 + married + nodegree + re74 + re75, 
                 data = lalonde, method = "cem")

m.out

fit2 <- lm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75, data = lalonde)
summary(fit2)

# Question 4

# Graph propensity score: probability who being in the treatment group as a function of covariates 

plot(m.out, type = "hist") # This comes with the matchit profile 
# Need the plot window be large to view this

m.data <- match.data(m.out) # Recover data after matching

# Question 5

# Let's look at the density plots again 

densityplot(~age, groups=factor(treat), data = m.data, col=c("grey30", "black"), auto.key=TRUE)
histogram(~black|treat, data = m.data, type = 'percent')

favstats(~age, data = m.data, groups = treat)
favstats(~black, data = m.data, groups = treat)

####################################################
######## Part 3: Comparing treatment and control
####################################################

# Looking at basic statistics

# look at the means b/w orig & matched data. very diff.
favstats(~re78, data= lalonde, groups = treat) 
favstats(~re78, data= m.data, groups = treat)

# Run a simple regression and then more complicated ones

fit2 <- lm(re78~treat, data=m.data)
display(fit2) # receiving treatment actually increases wages by ~$1500 

fit3 <- lm(re78~treat + age + educ + black + hispan 
           + married + nodegree + re74 + re75, data=m.data)

display(fit3)



