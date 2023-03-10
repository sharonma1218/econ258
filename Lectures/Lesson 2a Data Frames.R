#--------------------------------------------------------#
# Lesson 2a: Components of Data Frames                   #
#                                                        #      
# Ardina Hasanbasri                                      #
# Econ 258                                               #
#                                                        #
#--------------------------------------------------------# 

# Note: This code covers manipulating data frames.

# New package! 
# install.packages("dplyr")
library(dplyr) # Two commands: select and filter, comes from this package

##### Part 1: Lists and Data Frame #####

# Creating a list with three objects in it. The first object is the matrix, the
# second object are the T/F's, and the third object is "economics". 

mylist <- list(matrix(data=1:4, nrow=2, ncol=2), c(T,F,T,F), "economics")

mylist[1]
mylist[2]
mylist[3]

# Rename each object within the list: 

names(mylist) <- c("my_matrix", "my_logicals", "my_field")
mylist$my_matrix # This is nice b/c we don't need to keep on saying "object or column 2". 
mylist$my_logicals
mylist$my_field

# Creating a data frame. A data frame is not as general as a list. 
# Instead, the rows and columns are filled with data. 

mydata <- data.frame(person=c("Peter", "Meg", "Chris", "Stewie", "Lois"),
                     age = c(42,17,14,1, 40),
                     sex = c("M", "F", "M", "M", "F")) # May be useful to draw the table out first.

mydata # Produces a table/data frame.

mydata$age # Shows just the ages.

mydata$sex[2] #Shows the sex of Person #2.

mydata[1:2, 1:3] # Shows the table of just rows 1-2 and columns 1-3. 

# What if we want to see the data for females only?
# It is very difficult to keep track the numbers of row and columns when subsetting data.
# There are two ways to do this. 

mydata[2,5] # Recall that the first input is row and the second is column.
mydata[c(2,5), c("person", "age")] # Shows the second row and the columns "person" and "age".

# First way:

new_object <- mydata[mydata$sex=="F",] # Only select rows that has "F".
new_object

# Second way: 

mydata[mydata$sex=="F", "age"] # Only show the ages of the rows that has "F".
mydata[mydata$sex=="F", c("person","age")] # Only show the people and ages of the rows that has "F".

# Third way: with commands from the dplyr will come in handy with the select() and filter() function. 

select(mydata, c(person, age))
filter(mydata, sex=="F")
newdata <- filter(mydata, sex=="F")
select(newdata, c(person, age))

# Fourth way: you can do this in just one command: 

select(filter(mydata, sex=="F"), c(person, age))

# Exercises: 

# 0) Don't forget to reopen your package that has the functions "select" and "filter, 
# in case you reopened RStudio: 

library(dplyr)

# 1) Create a dataframe with your friends name, favorite movie/show, and year (a number) in college. 

mydata2 <- data.frame(name=c("Sharon", "Nyah", "Oleg", "Bob"), movie=c("The Shining", "The Office", 
          "The Godfather", "Spiderman"), year=c(3,3,4,4))
mydata2

# 2) Subset the data to movies column only.    
# 3 ways: 

mydata2[ , "movie"]
mydata2[ ,2]
mydata2$movie 

# 3) Subset the data to movies column only for those who are seniors.    

select(filter(mydata2, year=="4"), movie) # Only considers the rows that has seniors & only shows the movies column. 

# 4) Subset the data to one friend. You can choose which one, use their name.  

select(filter(mydata2, name=="Nyah"), c(1:3)) 

# 5) Subset the data to two friends and just show the movie and year of college column.

select(filter(mydata2,name==c("Sharon", "Nyah")), c("movie", "year"))


