#---------------------------------------------------------------------------------
#
# Dr. Strange and His Assistant
# (an exercise for the potential outcome mode)     
#
# Q: What is the effect of a blue pill on patient's longetivity after treatment?
#
# Created by ardinah@umich.edu
#----------------------------------------------------------------------------------

#-----------------------------------------
# PART 1: Dr. Strange and His Assistant
#-----------------------------------------

# Dr. Strange can go through different dimensions and see what happens in the future.
# He takes a peak at dimension 1 and dimension 2 and knows which one would benefit more 
#    from red (0) versus blue pill (1).

#---------
# Step 1: Create the dataframe based on the slide in class. 
#         Then create the individual treatment effect of the blue pill.  
#---------

data <- data.frame(patient_id = c(1,2,3,4,5,6,7,8,9,10),
                           yi_1 = c(7,5,5,7,4,10,1,5,3,9),
                           yi_0 = c(1,6,1,8,2,1,10,6,7,8))
View(data)

data$alpha_i <- data$yi_1 - data$yi_0

#----------
# Step 2: As a note to yourself, write down what each column in the dataset means.
#----------

#---------
# Step 3: Assume you are Dr. Strange, create a variable D = 1 if you assign this person to blue pill. 
#         Make a note on what it is.     
#---------

data$d_i <- ifelse(data$alpha_i < 0, 0, 1)

# another way:

data$d_i # create column 
data$d_i[data$alpha_i>0] <- 1 # get blue pill
data$d_i[data$alpha_i<=0] <- 0 # get red pill

#---------
#  Step 4: Finish coding exercise 1 
#---------
  
# average treatment effect of blue pill

ate <- mean(data$alpha_i) # 0.6 more yrs to live

# average treatment effect of blue pill to group that was assigned blue pill 

att <- mean(data$alpha_i[data$d_i==1]) # 4.4 

# average treatment effect of blue pill to group that was assigned red pill 

atu <- mean(data$alpha_i[data$d_i==0]) # -3.2 

# Dr. Strange's strategy makes sense (giving some people blue pill, some people 
# red - instead of giving everyone blue pill, even though on average, it makes 
# everyone better off, on average). He is able to do this b/c he can travel diff 
# dimensions. 

#######################

# Assume now you are Dr. Strange's assistant and you do not have Dr. Strange's time traveling abilities.

# Create data Y_i which is the data that the assistant will observe. 
# What would it look like, assuming you have access to Dr. Strange's data? 

#######################

#---------
# Step 5: Create the data that the assistant observes a.k.a what she sees in the current dimension.  
#--------- 
  
data$y_i <- data$yi_1 # create a new variable y_i. pretend it's dimension 1 (everyone gets blue pill).
data$y_i[data$d_i==0] <- data$yi_0[data$d_i==0] # but now let's replace those values. if dr. strange gave the person a red pill, take the data value from the "red pill years lived" column instead. 

#----------
# Step 6: Find the simple difference in outcomes (SDO).
#         which the average years lives by the blue pill group minus average of the red pill group. 
#----------

# Mean of group 1 and see the average life they live after taking the blue pill

m1 <- mean(data$y_i[data$d_i==1])

# Mean of group 1 and see the average life they live after taking the red pill

m2 <- mean(data$y_i[data$d_i==0])

# Simple difference of outcomes 

sdo <- m1-m2 # -0.4 

#--------------------------------
# PART 2: What is SDO  measuring?
#--------------------------------

# Why do we see a misleading SDO? 
# Because SDO is the ATE with selection bias and heterogeneous treatment bias. 
# Refer to equation from slide. 

# SDO is misleading. May think that blue pill is not effective at all or making 
# people live less. When in reality, average treatment affect is +0.6. Blue pill
# SHOULD be administered. 

#--------------------------------
# PART 2 EXTRA: Confirm SDO & ATE Relationship 
#--------------------------------

# expected additional years lived under red pill for group that was assigned blue pill 

unobserved1 <- mean(data$yi_0[data$d_i==1]) # 2.6

# expected additional years lived under blue pill for group that was assigned red pill

unobserved2 <- mean(data$yi_1[data$d_i==0]) # 4.2 

# pi (proportion of people who received blue pill)

pi_me <- sum(data$d_i==1)/length(data$d_i) # 0.5

pi_ans <- length(data$alpha_i[data$d_i==1])/length(data$alpha_i) # 0.5 

# now check if lhs = rhs for the assistant's scenario 

sdo == ate + (unobserved1-unobserved2) + (1-pi)*(att-atu) # false; shows that there was selection bias 

#--------------------------------
# PART 3: Randomization Exercise
#--------------------------------

# Suppose we create an experiment where we randomize patient into blue versus red pill. 

set.seed(123) # Set seed so that numbers drawn are the same 

# There are multiple ways to do this randomization.
# This choose randomly 5 patients from 1 to 10. 
First_Draw <- sample(1:10, 5) 
Group_D1   <- Data[First_Draw,] # row, column
Group_D2   <- Data[-First_Draw,] # The minus takes out the set from the data. the complements of first_draw

SDO_first_draw <- mean(Group_D1$Yi_1) - mean(Group_D2$Yi_0) # 1.2 b/c it's just one random draw 

# The above is a draft code. Now let's repeat this exercise 1000 times using a loop. 
vector_SDOs <- c() # empty vector to say SDO here later

for(x in 1:1000){
  get_sample <- sample(1:10,5)
  Group_D1 <- Data[get_sample]
  Group_D2 <- Data[-get_sample]
  sdo_sample <- mean(Group_D1$Yi_1) - mean(Group_D2$Yi_0)
  vector_SDOs <- c(vector_SDOs, sdo_sample)
} # this is how you create a loop 

# When you are done with the code above: 
mean_sdo_experiment <- mean(vector_SDOs)
print(ATE)
print(mean_sdo_experiment)

# The random experiment done multiple times give a closer number to ATE. 

# To show that a regression is the same as calculating the SDO

lm(Y_i~D_i, data=Data) # gives you SDO from the assistant. she just ran a regression, which you can only do if the data comes from a controlled experiment.

# If you had data where D is randomized, the SDO will then be the same as ATE in the regression as well. 
