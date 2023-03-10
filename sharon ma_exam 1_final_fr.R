#---------------------------------------------------
# Econ 258 Exam 1: 
# Youths Not in Employment, Education, or Training
# Sharon Ma 
# Due 03/01/2022
#---------------------------------------------------

#-------------------------------
# Part 1a: Setting Up Directory 
#-------------------------------
setwd("~/Downloads/Econ 258 R/Exam_1_Data")
library(dplyr)
library(mosaic) 
library(haven) 
library(ggplot2)

#-------------------------
# Part 1b: Importing Data 
#------------------------- 
indiv_data <- read_dta("b3a_cov.dta", encoding = "UTF-8")
high_edu_data <- read_dta("b3a_dl1.dta", encoding = "UTF-8") 
activity_employ_data <- read_dta("b3a_tk1.dta", encoding = "UTF-8") 

#--------------------
# Part 1c: Data Prep 
#--------------------
high_edu_data$high_edu_attain <- 0 
high_edu_data$high_edu_attain[high_edu_data$dl04==3] <- 0
high_edu_data$high_edu_attain[high_edu_data$dl06==2 | high_edu_data$dl06==72] <- 1
high_edu_data$high_edu_attain[high_edu_data$dl06==3 | high_edu_data$dl06==73 | 
                                high_edu_data$dl06==4] <- 2
high_edu_data$high_edu_attain[high_edu_data$dl06==5 | high_edu_data$dl06==74] <- 3 
high_edu_data$high_edu_attain[high_edu_data$dl06==6] <- 4 
high_edu_data$high_edu_attain[high_edu_data$dl06==13 | high_edu_data$dl06==60 | 
                                high_edu_data$dl06==61 | high_edu_data$dl06==62 | 
                                high_edu_data$dl06==63] <- 5 
high_edu_data$high_edu_attain[high_edu_data$dl06==11 | high_edu_data$dl06==12 | 
                                high_edu_data$dl06==15 | high_edu_data$dl06==17 | 
                                high_edu_data$dl06==14 | high_edu_data$dl06==90 | 
                                high_edu_data$dl06>90] <- NA
# Checked.
table(high_edu_data$dl06)
table(high_edu_data$high_edu_attain)
sum(is.na(high_edu_data$high_edu_attain))

# 2. Create a variable categorizing whether an individual is a NEET or not. 

# Merged all data sets. 
merge1 <- merge(indiv_data, high_edu_data, by=c("hhid14", "pid14"))
merge2 <- merge(merge1, activity_employ_data, by=c("hhid14", "pid14"))

# Filtered for ages 15-29. 
clean_merge2 <- merge2 %>% filter(age>14 & age<30)

# Created variable NEET. 
clean_merge2$neet <- ifelse(clean_merge2$dl07a==1 | clean_merge2$tk01a==1 | 
                              clean_merge2$tk02==1 | clean_merge2$tk03==1 | 
                              clean_merge2$tk04==1, 0, 1)

# 3. Create variable of different age categories.
clean_merge2$age_categories <- 0 
clean_merge2$age_categories[clean_merge2$age>14 & clean_merge2$age<20] <- 1
clean_merge2$age_categories[clean_merge2$age>19 & clean_merge2$age<25] <- 2
clean_merge2$age_categories[clean_merge2$age>24 & clean_merge2$age<30] <- 3

# Checked.
table(clean_merge2$age)
table(clean_merge2$age_categories)

# 4. Handling missing variables.
sum(is.na(clean_merge2$sex)) # 0 missing 
table(clean_merge2$dl04) # 0 missing 
table(clean_merge2$dl06) # 0 missing 

table(clean_merge2$dl07a) # 1 missing 
clean <- filter(clean_merge2, clean_merge2$dl07a!=8)
table(clean$dl07a) # 0 missing now 

sum(is.na(clean_merge2$neet)) # 69 missing
clean1 <- clean[!is.na(clean$neet),]
sum(is.na(clean1$neet)) # 0 missing now 

sum(is.na(clean1$high_edu_attain)) # 184 missing
clean1a <- clean1[!is.na(clean1$high_edu_attain),]
sum(is.na(clean1a$high_edu_attain)) # 0 missing now 

table(clean1a$tk01a) # 1 missing 
clean2 <- filter(clean1a, clean1a$tk01a!=8)
table(clean2$tk01a) # 0 missing now

table(clean2$tk02) # 0 missing 
table(clean2$tk03) # 0 missing 
table(clean2$tk04) # 0 missing 
table(clean2$marstat) # 0 missing 

#-------------------------------------
# Part 2a: Getting Summary Statistics 
#-------------------------------------

# 1. Look at the number of observations for highest lvl of education category. No
# schooling has very few observations, thus drop these individuals. 
table(clean2$high_edu_attain) # 0 no schooling 

# 2. Find proportion of individuals who are NEETS by gender AND highest edu. 
favstats(neet~sex & high_edu_attain, data=clean2)

# 52.264 % of females w/ elementary schooling are neets
# 41.052 % of females w/ junior high 
# 32.295 of females w/ general high school
# 28.777% of female w/ vocational high school
# 14.722% of females w/ higher edu

# 7.895% of males w/ elementary schooling are neets
# 6.489% of males w/ junior high school
# 6.543% of males w/ general high school
# 6.969% of males w/ vocational high school
# 4.483% of males w/ higher edu

# 3. Find proportion of individuals who are NEETS by different age category AND 
# gender. 
favstats(neet~age_categories & sex, data=clean2) 

# 19.657% of females ages 15-19 are neets
# 38.932% of females ages 20-24 
# 39.763% of females ages 25-29

# 7.883% of males ages 15-19 are neets 
# 8.372% of males ages 20-24 
# 3.803 of males ages 25-29

# 4. Find proportion of individuals who are NEETs by different age category AND 
# marital status. 
favstats(neet~age_categories & marstat, data=clean2) 

# 9.514% of singles ages 15-19 are neets
# 11.662% of singles ages 20-24
# 10.292% of singles ages 25-29

# 58.485% of married ages 15-19 are neets
# 40.931% of married ages 20-24 
# 26.066% of married ages 25-29

# 66.667% of separated ages 15-19 are neets
# 20% of separated ages 20-24
# 26.316% of separated ages 25-29 

# 16.667% of divorced ages 15-19 are neets
# 16.667% of divorced ages 20-24 are neets
# 17.308% of divorced ages 25-29 are neets

#------------------------
# Part 2b: Getting Plots 
#------------------------

# 5. For one of the above proportions, create a ggplot for them.
myplot <- ggplot(clean2, 
                   aes(x = factor(high_edu_attain,
                                  levels = c(1, 2, 3, 4, 5),
                                  labels = c("Elementary", "Junior High", 
                                             "HS General", "HS Vocational", 
                                             "Higher Ed")),
                       fill = factor(sex, 
                                     levels = c(1, 3),
                                     labels = c("Males", "Females")))) +
  geom_bar(position = "fill", color = "black") +
  facet_wrap(~factor(neet, labels = c("Not NEET", "NEET"))) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  scale_fill_brewer(palette = "Set3") + 
  labs(y = "Percent",
       fill = "Sex",
       x = "Highest Educational Attainment",
       title = "Proportion of NEETs by Sex & Highest Educational Attainment") +
  theme_bw()
print(myplot)
ggsave("myplot.tiff", myplot, height=4, width=6, units='in')

# 6. Graph frequency histogram of indivs who are considered NEET by education
# level and gender category. 

myplot2 <- ggplot(clean2, aes(x=high_edu_attain, y=neet)) +
  geom_bar(stat="identity", fill="lightblue") + 
  facet_wrap(~factor(sex, labels = c("Male", "Female"))) + 
  theme_bw() +
  ggtitle("Frequency of NEETs by Sex & Highest Educational Attainment") +
  xlab("Highest Eduational Attainment")
print(myplot2)
ggsave("myplot2.tiff", myplot2, height=4, width=6, units='in')

#------------------------------------
# Part 3: Brief Notes Given Results
#------------------------------------

# 1. The group that has the highest proportion of NEETs are females whose highest
# educational attainment was only elementary school. 

# 2. The gap in NEET rates between men and women is very big. For example, the gap
# b/w females whose highest educational attainment was elementary and males whose
# highest educational attainment was elementary was around 46%, meaning there are
# a lot more female NEETs. 

# 3. I was not surprised that there are more female NEETs than male NEETs, but I 
# am sad how large the gap could be. I was also not surprised by the negative 
# correlation b/w education level and NEETs (the higher one's educational attainment,
# the less likely they are to be a NEET). I was, however, surprised that there are 
# more NEETs among married than singles or divorced. But I realized afterwards that 
# it makes sense: the married probably have kids and are staying home to take care
# of them instead of working. I was also surprised that for females, as they get 
# older, they are more likely to be NEETs, but for males, as they get older, they 
# are less likely to be NEETs. I assume that this is also associated w/ the trend 
# of females staying at home and men going to work. 

#---------------------------------------------------