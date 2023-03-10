
#---------------------------------------------------#
# ECON 258                                          #
#                                                   #
# Lesson 2b: Importing and Understanding Your Data  #
#                                                   #      
#                                                   #
#---------------------------------------------------# 

#### Part 0: Packages & Libraries ####

install.packages("dplyr")
install.packages("mosaic")
install.packages("haven")
install.packages("readstata13")

library(dplyr)  # to use the filter and select command
library(mosaic) # we will use favstats command 
library(haven)  # needed to read stata .dta files 
library(readstata13)

##### Part 1: Importing Data #####

# In this class there are two types of data we will use: filename.csv and filename.dta (stata file). 

setwd("~/Desktop/Econ 258 R/Lesson 2 Data Frames and Understanding the Data")

data_wvs    <- read.dta13("week1_world_value_survey_stata.dta")
data_growth <- read.csv("week1_internet_and_growth.csv") 

# Check out country in data_wvs

data_wvs$B_COUNTRY  # The obs have value labels.
data_growth$county

only_US <- filter(data_wvs, B_COUNTRY=="United States") # Example of only getting US data 

##### Part 2: Understanding what type of data we have #####

# The key question you need to know: what is the unique observation for one row of data? 

View(data_wvs)     # Look at some variables, some of them are Factor, while others are integers
View(data_growth) 

head(data_wvs)

# A few nice commands to understand the data. 

# Q: How many observation do we have? 
length(data_wvs$B_COUNTRY)
length(data_growth$weekwage)

# Q: What variables do we have? 
names(data_wvs)
names(data_growth)

# Q: Is the growth data only for year 2000? 
unique(data_growth$year)
table(data_growth$year)
table(data_wvs$B_COUNTRY)

# Q: What is the unique obs for each dataset? 
length(unique(data_wvs$B_COUNTRY))
length(data_wvs$B_COUNTRY)            #Be careful, not length(data_wvs)
length(unique(data_wvs$D_INTERVIEW)) 
  
  # can also do: 
  length(data_wvs$B_COUNTRY)==length(unique(data_wvs$D_INTERVIEW))
  length(data_growth$county)==length(unique(data_growth$county))
  
##### Part 3: A Few Cleaning Commands and Descriptive Statistics #####

# Let's subset our data so that it's more managable: 
  
  data_subset <- select(data_growth, c(county, year, lnweekwage, lnemp, adv_int, 
                                       basic_int, pc_per_emp, iv_prog_in_oth, 
                                       iv_bartik, iv_num_arpanet_nodes, iv_cost_per_loop_monthly, 
                                       highinc, highpop, higheduc, highind))

# Suppose you want to change the column name: 
# this could be turning long names into a singular word so that they're easier to type.
# this could also be removing caps since commands are case-sensitive. 
  
  # county rename into county.name
  # lnweekwage rename into lwage
  
  names(data_subset)[1] <- "county.name" # w/o the quotations, r will think it's an object & try to find it in the environment 
  names(data_subset) # only shows the names of the colummn headers 
  
  names(data_subset)=="lnweekwage"
  names(data_subset)[names(data_subset)=="lnweekwage"] # the true condition is in the brackets 
  names(data_subset)[names(data_subset)=="lnweekwage"] <- "lwage" 
  names(data_subset)
  
  # Basic summary statistics 
  mean(data_subset$lwage)
  median(data_subset$lwage)
  min(data_subset$lwage)
  max(data_subset$lwage)
  range(data_subset$lwage)
  var(data_subset$lwage)
  sd(data_subset$lwage)
  
# Tips: You do not want to type "data_subset$" every single time? 
attach(data_subset)
mean(lwage)

# Other ways to get descriptive tables: 
n <- length(lwage)
table(higheduc) # gives a table of how many counties are highly educated (n=686) & how many are not (n=2057)
round(table(higheduc)/length(higheduc),4) # the same info but in percentages 

mean(lwage[higheduc==1]) # certain populations are extracted. mean lwage of the highly educated. 
mean(lwage[higheduc==0])
mean(lwage~higheduc) # single line command of the above 

# Be careful when using attach(), if you have two datasets with the same
# variable names, it might be tricky. You can always also use the original way

### Ready made Stats Table
# R has a summary and quantile function that also provides some statistics in a table 
summary(data_subset$lnemp)
quantile(data_subset$lnemp)

# using the mosaic package favstats is useful here
favstats(data_subset$lnemp)
favstats(data_subset$lnemp)[c("mean", "sd", "min", "max", "n")]
favstats(~lnemp, data = data_subset)

# Can also look at the statistics by group. 
favstats(lnemp~highpop, data = data_subset)

##### Part 4: A Little bit about missing data #####

# How R handles missing data? # Let's look at the world value survey.  
min(data_wvs$K_DURATION) # when there is missing data, some commands do not work. result: NA. 
min(data_wvs$K_DURATION, na.rm=TRUE) # ignores the missing data 
max(data_wvs$K_DURATION, na.rm=TRUE)

# Important commands for missing data.  
is.na(data_wvs$K_DURATION) # is.na tells you if something is missing or not 

sum(is.na(data_wvs$K_DURATION)) # This gives you number of missings.

mean(is.na(data_wvs$K_DURATION))

which(is.na(data_wvs$K_DURATION))

# Difference between NA, NaN, and NULL

-Inf + Inf # NaN: undefined or not defined. 

0/0

c(2, 4, NA, 8) # NA: missing data or value 

c(2, 4, NULL, 8) # NULL: not missing value but object simply does not exist 

# To drop observations where variable adv_int is missing
data_no_miss_duration <- data_wvs[!is.na(data_wvs$K_DURATION), ] # new data set is all the rows that is not missing 

length(data_no_miss_duration$K_DURATION)
sum(is.na(data_no_miss_duration$K_DURATION))

#########################################################################
# You can actually start HW1 to practice the commands you just learned. 
