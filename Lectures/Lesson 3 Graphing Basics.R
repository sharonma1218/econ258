#------------------------------------------------------------- #
# Lesson 3: Graphing Basics                                   #
#                                                              #      
# Ardina Hasanbasri                                            #
# Econ 258                                                     #
#                                                              #
#--------------------------------------------------------------# 

setwd("C:/Users/ardin/OneDrive/Desktop/Econ 258 Winter 2022/Lesson Codes/Lesson 3 Codes")

library(dplyr)

### This code replicates a graph from the internet and growth paper. 

data_clean <- read.csv("clean_internet_and_growth.csv") 

data_clean <- filter(data_clean, adv_int>0 & adv_int<0.3 & wagegrowth<1) # formula: data_clean <- filter(data_set, bounds for x- & y-axes); cleans the data to contain only the values within those bounds 

# Panel A Graph
plot(data_clean$adv_int, data_clean$wagegrowth) # formula: plot(data_set$x_axis_variable, data_set$y_axis_variable)

# Add descriptions
plot(data_clean$adv_int, data_clean$wagegrowth,  
     main = "Advance Internet Investment and Wage Growth by County",
     xlab = "Fraction Firms with Advance Internet", 
     ylab = "Wage Growth 1995 to 2000", 
     xlim = range(c(0,0.3)), ylim = range(c(0,1))) # expands on previous formula to allow labeling of title & axes

# Lets draw Panel B
# Lets create a subset of the data and give it a name that we can easily reference

adv_int_high <- data_clean$adv_int[data_clean$allhigh.x==1] 
adv_int_low  <- data_clean$adv_int[data_clean$allhigh.x==0]
wagegrowth_high <- data_clean$wagegrowth[data_clean$allhigh.x==1] 
wagegrowth_low  <- data_clean$wagegrowth[data_clean$allhigh.x==0]

png("wage_growth_and_internet.png", width = 600, height = 400) 

plot(adv_int_high, wagegrowth_high, 
     main = "Advance Internet Investment and Wage Growth by County Type",
     xlab = "Fraction Firms with Advance Internet", ylab = "Wage Growth 1995 to 2000", 
     pch=19, cex = 0.5,  # pch is the coloring of the dots; cex is the size of the dots  
     col = "red", 
     xlim = range(c(0,0.3)), ylim = range(c(0,1)))

abline(lm(wagegrowth_high ~ adv_int_high), col = "red") # lm = linear model; makes a nice line that goes across scatter plot

points(adv_int_low, wagegrowth_low, col = "blue", pch=5, cex = 0.5,)
abline(lm(wagegrowth_low ~ adv_int_low), col = "blue") 

legend("topright", inset=.05, pch=c(19,19), cex=c(0.75,0.75), c("Top county","Non top county"), col=c("red", "blue")) 

dev.off()

# Play around with line 39 to 51. Start with a short code and then keep adding in the options to see what each code does. 

