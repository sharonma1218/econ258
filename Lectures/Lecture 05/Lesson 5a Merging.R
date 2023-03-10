
#---------------------------------------------------#
# ECON 258                                          #
#                                                   #
# Lesson 5a: Combining Multiple Datasets            #
#                                                   #      
# Ardina Hasanbasri                                 #
# ardinah@umich.edu                                 #
#                                                   #
#---------------------------------------------------# 

#-----------------------------------------------------
# Part 1: Combinig Datasets 
#-----------------------------------------------------

apt_units <- data.frame(building   = c(1, 2, 3, 4, 5), 
                       num_floors = c(1, 3, 3, 6, 2), 
                       rent       = c(1000, 1200, 800, 700, 300))

renters <- data.frame(name = c("bob", "linda", "gene", "tina", "louis"), 
                     building_name = c(1, 1, 2, 3, 3))

# the units of observations are unique. so the the units of observation in the dataset "apt_units" is the apartment buildings (1,2,3,4,5), and the units of observation in the dataset "renters" is the renters (bob, linda, gene, tina, louis).

# It's possible to merge by variable that has a different name 
merge(renters, apt_units, by.x = "building_name", by.y = "building")

# It's possible to keep observations not matched in both dataset 
merge(renters, apt_units, by.x = "building_name", by.y = "building", all=TRUE)

# formula: merge(x_dataset, y_dataset, by.x = "variable from x dataset", by.y = "variable from the y dataset, all=TRUE) 

#-----------------------------------------------------
# Part 2: Merging Example Two 
#-----------------------------------------------------

setwd("~/Desktop/Econ 258 R/Lesson 5 Merging and Reshaping")

# Lets practice merging by merging data from countyyear and countygrowth. 

county_year   <- read.csv("countyyear.csv") 
county_growth <- read.csv("countygrowth.csv")

# The first question you should ask is: 
# What is the unique identifier of these datasets? 
# county_year has county and year as a unique identifier. county_growth has county as a unique identifier, it does have year=2000
# We are merging this because we will need variables from both places. 

data_merged <- merge(county_year, county_growth, by=c("county", "year")) # want to make sure it's matched with the right county AND the right year 

# Cleaning 101: It would be nice if your variable names make sense. Reminder for cleaning. 

names(data_merged)[names(data_merged)=="surv_deeppost_00"]  <- "adv_int"
names(data_merged)[names(data_merged)=="surv_shalpost00" ]  <- "basic_int"
names(data_merged)[names(data_merged)=="surv_pcperemp"   ]  <- "pc_per_emp"
names(data_merged)[names(data_merged)=="iv_othcprog"     ]  <- "iv_prog_in_oth"

# To drop observations where variable adv_int is missing
data_clean <- data_merged[!is.na(data_merged$adv_int), ] 

# Saving your dataset
write.csv(data_clean, "clean_internet_and_growth.csv")

# Some extra commands that might be handy: 
rm(county_year)  # If you want to remove datasets not used anymore. 
rm(county_growth)

# The shortcut "Control + L" also clears your console. 
