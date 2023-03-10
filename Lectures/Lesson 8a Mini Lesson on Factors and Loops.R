#------------------------------------------------------------- #
# Lesson 8: Factors and Loops                                 #
#                                                              #      
# Prepared by: Ardina Hasanbasri                               #                                                  #
#                                                              #
#--------------------------------------------------------------# 

library(ggplot2)

#################################################
########## A Few Things About Factors ###########
#################################################

# Reference for types of vectors: https://r4ds.had.co.nz/vectors.html

color <- data.frame(person = c("1", "2", "3"), # when you put numbers, you still need to have quotation marks b/c they're characters, not objects
                    fav.color = c("Blue","Green","Blue"))

typeof(color$person)
color$person <- as.numeric(color$person)

typeof(color$fav.color)
as.factor(color$fav.color) # telling r that there are two diff levels. good if you want to graph.

# Depending on what you want to do, you might want to have it as a factor but with numbers. 

color$blue    <- ifelse(color$fav.color=="Blue", 1, 0)
color$blue

ggplot(color, aes(blue)) +
  geom_bar()

ggplot(color, aes(as.factor(blue))) + # adding as.factor, it knows it can only be 0 or 1. doesn't include 0.5.
  geom_bar()

color$blue <- factor(color$blue, levels =c(0,1), labels=c("Green", "Blue"))
color$blue

ggplot(color, aes(blue)) +
  geom_bar()

#################################################
########## Practice Using Loops #################
#################################################

# Using if else
a <- 2
b <- 5 
if(a<b){
  a <- a^2
} else {
  b <- b^2
}
a
b

# Using the while function
i <- 0 
while(i<5){
    print(i)
    i <- i+1
}

# Using the for loop function
# most common one
for(x in 1:30){
  print(x)
}

pets <- c("cat", "dogs", "hamsters")
for(k in pets){
  print(k)
}

# Let's do a loop within a loop! Pay attention to the parenthesis. 

v <- c(1:10) 
v # 1,2,3,4,5,,6,7,8,9,10
for(k in 1:10){
  if(v[k]<6){
    v[k] <- 0
  }
}
v # 0,0,0,0,0,6,7,8,9,10

