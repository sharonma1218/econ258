#########################################
#
# Lesson 6 Histograms and Bar Graphs
# 
# ardinah@umich.edu
#########################################

# The code below took bits and pieces from the group work of previous semesters
# and modified to create better graphs. 
# Use this to help you finish up the lab. 

# Some of the code uses a piping syntax %>%.
# Another code lesson is creating a new variable/column! ifelse, mutate, basic

setwd("C:/Users/ardin/OneDrive/Desktop/Econ 258 Winter 2022/Lesson Codes/Lesson 6 Work Wage and Majors")

library(haven)
library(ggplot2)
library(dplyr)
library(stats)

data <- read_dta("2019_acs_degree_data.dta", encoding = "UTF-8")

print_labels(data$degfield) #24 is engineers
print_labels(data$sex) #1 is male ; 2 is female
str(data$age) #age is double

cleaning        <- data %>% filter(degfield==24 & age <30) 
cleaning$gender <- ifelse(cleaning$sex==1, 'Male', 'Female')


sum(is.na(cleaning$incwage)) #  no missing values
sum(is.na(cleaning$sex))     #  no missing values

cleaning        <- cleaning %>% filter(incwage!=0) 

low  <- quantile(cleaning$incwage, 0.01)
high <- quantile(cleaning$incwage, 0.99)
cleaning        <- cleaning %>% filter(incwage>low & incwage<high) 

summary(cleaning$incwage) 

#-----------------------------------#
# A Frequency Histogram and Density #
#-----------------------------------#

# Let's make this code more efficient
# Let's do a frequency histogram
ggplot(cleaning, mapping = aes(x=incwage)) +
  geom_histogram() + 
  facet_wrap(~cleaning$sex)

ggplot(cleaning, aes(x=incwage)) +
        geom_histogram() +
        facet_wrap(~sex)

typeof(cleaning$sex)
cleaning$sex <- factor(cleaning$sex, labels=c("Male", "Female")) # doesn't spit 0/1 anymore but Male/Female 

version1 <- ggplot(cleaning, aes(x=incwage)) +
        geom_histogram(color="black", fill="lightgreen") +
        facet_wrap(~sex)  +
        theme_minimal()   +
        ggtitle("Engineer Majors Earnings by Gender for Age Under Thirty") +
        xlab("Earnings")  +
        ylab("Frequency") 

print(version1)  
ggsave("hist_v1.png", version1, height=4, width=8, units="in")

# Let's do a density instead
        
ggplot(cleaning, aes(x=incwage)) +
  geom_density(color="black", fill="lightgreen") 

ggplot(cleaning, aes(x=incwage, group=sex)) +
  geom_density(color="black") # problem: there's a tail, which would mess up our interpretations

ggplot(cleaning, aes(x=log(incwage), group=sex)) + # this is why economists love logs!
  geom_density(color="black") 

ggplot(cleaning, aes(x=log(incwage), group=sex, color=sex, weights=hhwt)) +
  geom_density()  +
  theme_minimal()   +
  ggtitle("Engineer Majors Earnings by Gender for Age Under Thirty") +
  xlab("Earnings")  +
  ylab("Frequency") 

# Below the color does not change, why?
ggplot(cleaning, aes(x=log(incwage), group=sex, color=sex, weights=hhwt)) +
  geom_density(size = 1.5)  + # thicker lines
  theme_minimal()   +
  ggtitle("Engineer Majors Earnings by Gender for Age Under Thirty") +
  xlab("Earnings")  +
  ylab("Frequency") +
  scale_fill_brewer(palette="Set1") # different colors; google the palettes  

version2 <- ggplot(cleaning, aes(x=log(incwage), group=sex, fill=sex, weights=hhwt)) +
  geom_density(size = 1, alpha = 0.4)  +
  theme_classic()   +
  ggtitle("Engineer Majors Earnings by Gender for Age Under Thirty") +
  xlab("Log Earnings")  +
  ylab("Frequency") +
  scale_fill_brewer(palette="Set1")

print(version2)

ggsave("hist_v2.png", version2, height=4, width=8, units="in")

#-------------------------------------------#
# A Proportion Graph Analyzing Unemployment #
#-------------------------------------------#

#Step 1: Get Subset and Calculate Percentages for Each Ed Level
data_subset <- select(data, c(educ, marst, hhwt)) #all data only marriage status and ed
data_subset <- filter(data_subset, educ>=7)       #includes all marital statuses and ed greater than and equal to 7

print_labels(data_subset$educ)
table(data_subset$educ)

data_subset$marst2 <- ifelse(data_subset$marst<2, 1, 0) 

marriage_perc <- aggregate(data_subset$marst2 ~ data_subset$educ, # mean of marital status by education level. the higher educated are more likely to be married.         
                           FUN = mean)                    

names(marriage_perc) <- c("educ", "marst")

# What happens when the factor is not there?
ggplot(marriage_perc, aes(x=factor(educ), y=marst))+
  geom_bar(stat="identity", fill="purple") +
  xlab("College Years")+ylab("Percentage Married")+
  ggtitle("Pctg of College Students Married by Ed Level")+
  ylim(range(c(0,0.8)))+
  theme_light()

# Let's do one without stat identity. (It's just going to count for each variable.)
# We know that geom_bar counts when no other option is added. 
ggplot(data_subset, aes(x=factor(educ)))+
  geom_bar()

# Without the factor on marst2, the code below won't work
ggplot(data_subset, aes(x=factor(educ), fill=factor(marst2)))+
  geom_bar()

ggplot(data_subset, aes(x=factor(educ), fill=factor(marst2)))+
  geom_bar(position= "fill")

data_subset$marst2 <- factor(data_subset$marst2, labels=c("Not Married", "Married"))

ggplot(data_subset, aes(x=factor(educ), fill=marst2))+
  geom_bar(position= "fill") +
  xlab("College Years")+ylab("Percentage Married")+
  ggtitle("Pctg of College Students Married by Ed Level")+
  theme_light()

ggplot(data_subset, aes(x=factor(educ), fill=marst2))+
  geom_bar(position= "fill") +
  xlab("College Years")+ylab("Percentage Married")+
  ggtitle("Pctg of College Students Married by Ed Level")+
  theme_light() + 
  scale_fill_brewer(palette = "Paired") + # notice that scale_fill_brewer uses already made palettes
  theme(legend.title=element_blank()) 
  
bar_perc <- ggplot(data_subset, aes(x=factor(educ), fill=marst2, weights=hhwt))+
            geom_bar(position= "fill") +
            xlab("College Years")+ylab("Percentage Married")+
            ggtitle("Pctg of College Students Married by Ed Level")+
            theme_classic() + 
            scale_fill_manual(values=c("#ECE2D0", "#A26769")) + # while scale_fill_manual uses hex codes
            theme(legend.title=element_blank()) + coord_flip() +
            theme(legend.position="bottom")
print(bar_perc)

ggsave("bar_perc.png", bar_perc, height=4, width=5, units="in")
