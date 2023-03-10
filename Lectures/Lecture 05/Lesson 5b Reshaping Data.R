###########################################
#
# Lesson 5b: Reshaping Data  
#
# Prepared by Ardina Hasanbasri 
#
###########################################

library(reshape2)
library(ggplot2)

table1_wide <- data.frame(Household_ID=c(1,2,3),
                     Output_Plot1=c(12,15,23),
                     Output_Plot2=c(6,73,55), 
                     Output_Plot3=c(25,56,10))

# Reshaping from wide to long 

table1_long <- melt(table1_wide, id.vars="Household_ID")
# formula: table1_long <- melt(table_we_want_to_reshape, id.vars="unique_ID")

table1_long2 <- reshape(data = table1_wide, 
                           idvar = "Household_ID", 
                           varying = c("Output_Plot1", "Output_Plot2", "Output_Plot3"),
                           v.names="Output", 
                           timevar = "Plot Number", # without this, the variable will be called time instead
                                                    # timevar is the name of the variable that distinguishes between plot
                           direction="long")

# Reshaping from long to wide 

table_wide_v2 <- reshape(data = table1_long2, 
                          idvar = "Household_ID", 
                          v.names="Output", 
                          timevar = "Plot Number", # without this, the variable will be called time instead
                          direction="wide")

# Here is a graph using the reshape data. 
ggplot(table1_long, aes(x=Household_ID, y=value, fill=as.factor(variable))) + 
  geom_bar(stat = "identity", position = position_dodge())

# Some practice: 
# Can you reshape the table below from wide to long? 
# Try it without looking at the solution below but using the sample code above. 

table1_exercise <- data.frame(Household_ID =c(1,1,2,2,3,3),
                             individual_id =c(1,2,1,2,1,2),
                             income_year2005=c(15,16,20,15,18,30), 
                             income_year2010=c(20,30,10,20,20,40))

View(table1_exercise)

table1_exercise_long1 <- melt(table1_exercise, id.vars = c("Household_ID", "individual_id"))

View(table1_exercise_long1)

table1_exercise_long2 <- reshape(data=table1_exercise,
                                 idvar = c("Household_ID", "individual_id"),
                                 varying = c("income_year2005","income_year2010"),
                                 v.names = "income_year",
                                 timevar = "Year",
                                 direction = "long")

View(table1_exercise_long2)

ggplot(table1_exercise_long2, aes(x=Household_ID, y=income_year, fill=as.factor(Year))) + 
  geom_bar(stat = "identity", position = position_dodge())









# Solution:

table1_exercise_long <- reshape(data = table1_exercise, 
                          idvar = c("Household_ID", "individual_id"),  
                          varying = c("income_year2005", "income_year2010"), 
                          v.names = "income_year",
                          timevar = "Year", # without this, the variable will be called time instead
                          direction="long")
