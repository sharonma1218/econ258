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

Data <- data.frame("patient_id" = c(1,2,3,4,5,6,7,8,9,10), 
                   "Yi_1" = c(7,5,5,7,4,10,1,5,3,9), 
                   "Yi_0"  = c(1,6,1,8,2,1,10,6,7,8))

View(Data)

# Creating individual treatment effect of the blue pill.  
Data$alpha_i <- Data$Yi_1 - Data$Yi_0 

#----------
# Step 2: As a note to yourself, write down what each column in the dataset means.
#----------

   # patient_id "Patient ID" 
   # Yi_1 "Outcome when everyone received blue pill in dimension 1"
   # Yi_0 "Outcome when everyone received blue pill in dimension 0"
   # alpha_i "Individual treatment effect, how effective is blue compare to red"

#---------
# Step 3: Assume you are Dr. Strange, create a variable D = 1 if you assign this person to blue pill. 
#         Make a note on what it is.     
#---------

Data$D_i

Data$D_i[Data$alpha_i>0]  <- 1 
Data$D_i[Data$alpha_i<=0] <- 0 

# D is "In the current dimension, the patient either takes the blue (1) or the red (1)"


#---------
#  Step 4: Finish coding exercise 1
#---------
  
# ATE, ATT, ATNT
mean(Data$alpha_i)                 # Everyone
mean(Data$alpha_i[Data$D_i==1])    # Just group 1: given blue pill
mean(Data$alpha_i[Data$D_i==0])    # Just group 2: given red pill


# We will use the above calculations later on. 
# But from the calculation of ATE, on average, it seems the blue pill 
# increases years lived by 0.6 compared to the red pill. 


#######################

# Assume now you are Dr. Strange's assistant and you do not have Dr. Strange's time traveling abilities.

# Create data Y_i which is the data that the assistant will observe. 
# What would it look like, assuming you have access to Dr. Strange's data? 

#######################

#---------
# Step 5: Create the data that the assistant observes a.k.a what she sees in the current dimension.  
#--------- 
  
Data$Y_i <- Data$Yi_1
Data$Y_i[Data$D_i==0] <- Data$Yi_0[Data$D_i==0]

#----------
# Step 6: Find the simple difference in outcomes (SDO).
#         which the average years lives by the blue pill group minus average of the red pill group. 
#----------

# Mean of group 1 and see the average life they live after taking the blue pill
m1 <- mean(Data$Y_i[Data$D_i==1])

# Mean of group 1 and see the average life they live after taking the red pill
m0 <- mean(Data$Y_i[Data$D_i==0])

# Simple difference of outcomes 

SDO <- m1 - m0 
print(SDO)
  
# If the assistant takes SDO as the truth, then the number is misleading. 
# It seems the people who receive the blue pill live much less than those receiving the red pill. 

#--------------------------------
# PART 2: What is SDO  measuring?
#--------------------------------

# Why do we see a misleading SDO? 
# Because SDO is the ATE with selection bias and heterogeneous treatment bias. 
# Refer to equation from slide. 

ATE <- mean(Data$alpha_i)
ATT <- mean(Data$alpha_i[Data$D_i==1])
ATU <- mean(Data$alpha_i[Data$D_i==0])

pi  <- length(Data$alpha_i[Data$D_i==1])/length(Data$alpha_i) 

SDO_calculate <- ATE + 
                 mean(Data$Yi_0[Data$D_i==1]) - mean(Data$Yi_0[Data$D_i==0]) +
                 (1-pi)*(ATT-ATU)

print(SDO_calculate)
print(SDO)

#--------------------------------
# PART 3: Randomization Exercise
#--------------------------------

# Suppose we create an experiment where we randomize patient into blue versus red pill. 

set.seed(123) # Set seed so that numbers drawn are the same 

# There are multiple ways to do this randomization.
# This choose randomly 5 patients. 
First_Draw <- sample(1:10, 5)
Group_D1   <- Data[First_Draw,]
Group_D2   <- Data[-First_Draw,] # The minus takes out the set from the data

SDO_first_draw <- mean(Group_D1$Yi_1) - mean(Group_D2$Yi_0)

# The above is a draft code. Now let's repeat this exercise 1000 times using a loop. 
vector_SDOs <- c() # empty vector to say SDO here later

for(x in 1:1000){
  get_sample  <- sample(1:10, 5)
  Group_D1    <- Data[get_sample,]
  Group_D2    <- Data[-get_sample,] 
  sdo_sample  <- mean(Group_D1$Yi_1) - mean(Group_D2$Yi_0)
  vector_SDOs <- c(vector_SDOs, sdo_sample) 
}

mean_sdo_experiment <- mean(vector_SDOs)
print(ATE)
print(mean_sdo_experiment)

# The random experiment done multiple times give a closer number to ATE. 

# To show that a regression is the same as calculating the SDO

lm(Y_i~D_i, data=Data) # gives you SDO from the assistant

# If you had data where D is randomized, the SDO will then be the same as ATE in the regression as well. 
