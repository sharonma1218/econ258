####################################
#
# Lesson 12 Instrumental Variable Cigarette Example 
# Econ 258: Data Analytics with R
# 03/10/2022
#
####################################

# Further reference: https://rpubs.com/wsundstrom/t_ivreg

# install.packages("ivpack")
# install.packages("stargazer")
library(stargazer)
library(ivpack)

data("CigarettesSW", package = "AER")
# If the command above does not work, use the data provided in the folder.

help("CigarettesSW") # Explains the variables used in this example.

### Question: How do prices affect cigarette consumption? 

# Real prices of cigarette
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)  

# Real income per capita
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi) 

# Tax difference
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
    # This will be used as an instrument. 
    # This is a measure of real tax on cigarettes arising from state general sale tax.
    # Higher sales tax affects consumption of cigarettes but is not determined by the supply demand of cigarette market. 

# Estimating the effect on prices on the demand of cigarettes 
# Recall from principles of econ, price is endogenous. 
# Its determined by supply and demand. 

# regression:  Q of cigarettes ON rprice controls (second stage) 
# first stage: rprices ON controls tdiff I(tax/cpi)

help("ivreg")  
model_2sls <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
                    data = CigarettesSW, subset = year == "1995")
summary(model_2sls)

# We can get similar results (except for standard error):

stage1 <- lm(log(rprice) ~ log(rincome) + tdiff + I(tax/cpi), data = CigarettesSW, subset = year == "1995")
CigarettesSW$predict <- predict(stage1)

stage2 <- lm(log(packs) ~ predict + log(rincome), data = CigarettesSW, subset = year == "1995")

summary(stage2)

# Ordinary OLS just for comparison

model_ols <- lm(log(packs) ~ log(rprice) + log(rincome),
                data = CigarettesSW, subset = year == "1995")

# Create a table with all results. 
stargazer(CigarettesSW)
stargazer(model_ols, stage2, model_2sls)
